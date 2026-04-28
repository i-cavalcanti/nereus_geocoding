main_first_stage_geocoding <- function(
  pathname_in,
  pathname_out,
  years = 2023:2018,
  encoding = "Latin-1",
  sd_threshold_km = 0.3,
  verbose = TRUE,
  filename_prefix = "sp",
  filter_municipio_7 = NULL,
  filter_cnae = NULL,
  select_cols = NULL,                 # <-- só para fread (input raw)
  stats_filename = "stats_geocoding.csv",
  overwrite = FALSE,
  debug_on_error = FALSE
) {
  stopifnot(
    dir.exists(pathname_in),
    is.character(pathname_out), length(pathname_out) == 1L,
    is.numeric(years), length(years) >= 1L,
    is.numeric(sd_threshold_km), length(sd_threshold_km) == 1L,
    is.character(filename_prefix), length(filename_prefix) == 1L
  )

  if (!requireNamespace("data.table", quietly = TRUE)) stop("Pacote 'data.table' não instalado.")

  log <- function(...) if (isTRUE(verbose)) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))

  # saída
  out_dir <- pathname_out
  if (overwrite && dir.exists(out_dir)) {
    log("Removendo saída anterior (overwrite=TRUE): ", out_dir)
    unlink(out_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  stats_file <- file.path(out_dir, stats_filename)
  if (overwrite && file.exists(stats_file)) unlink(stats_file, force = TRUE)

  years <- as.integer(years)

  rds_paths  <- setNames(vector("list", length(years)), as.character(years))
  stats_list <- setNames(vector("list", length(years)), as.character(years))

  # colunas mínimas que PRECISAM vir do CSV bruto
  needed_min_raw <- c("municipio", "cduf", "cep", "endereco", "estoque")

  # colunas usadas no geocoding (existem após pré-processamento)
  geocode_keep <- c("id","endereco", "numlograd", "cep", "bairro", "municipio_7", "uf_dom", "estoque", "identificad_m", "municipio", "matrizfilial")

  for (year in years) {
    t0 <- Sys.time()
    log("Ano: ", year)

    filename <- paste0(filename_prefix, "_estb_", year, "_limpo")
    file_in <- file.path(pathname_in, "raw", "rais_temp", paste0(filename, ".csv"))
    log("Arquivo: ", file_in)

    if (!file.exists(file_in)) {
      log("AVISO: arquivo não encontrado, pulando ano ", year)
      stats_list[[as.character(year)]] <- list(year = year, status = "missing_input_file")
      next
    }

    # --- select_cols: somente para fread; garantir que colunas mínimas sempre entram
    read_cols <- select_cols
    if (!is.null(read_cols)) {
      # IMPORTANTÍSSIMO: não inclua aqui colunas criadas depois (municipio_7, uf_dom etc.)
      read_cols <- unique(c(read_cols, needed_min_raw))
      log("fread(select=...) com ", length(read_cols), " colunas (inclui mínimas).")
    } else {
      log("fread sem select (lendo todas as colunas).")
    }

    dt <- data.table::fread(file_in, encoding = encoding, select = read_cols)
    log("Linhas lidas: ", nrow(dt))

    # validar colunas mínimas do RAW
    missing_min <- setdiff(needed_min_raw, names(dt))
    if (length(missing_min) > 0) {
      stop("Ano ", year, ": faltam colunas no CSV: ", paste(missing_min, collapse = ", "))
    }

    # opcionais: criar se faltar (RAW)
    if (!("bairro" %in% names(dt)))    dt[, bairro := NA_character_]
    if (!("numlograd" %in% names(dt))) dt[, numlograd := NA]


    # ---- Pré-processamentos ----
    dt[, municipio_7 := ibge6_to_7(municipio)]
    dt <- add_sigla_from_uf(dt, "cduf", "uf_dom")
    dt <- change_cep_99999999_to_na(dt, "cep", "municipio")

    # Filtro opcional por município
    if (!is.null(filter_municipio_7)) {
      n0 <- nrow(dt)
      dt <- dt[municipio_7 %in% unlist(filter_municipio_7)]
      log("Filtro municipio_7 aplicado: ", n0, " -> ", nrow(dt), " linhas")
    }

    # Filtro opcional por cnae
    if (!is.null(filter_cnae)) {
      n0 <- nrow(dt)
      dt <- dt[clascnae20 %in% unlist(filter_cnae)]
      log("Filtro cnae aplicado: ", n0, " -> ", nrow(dt), " linhas")
    }

    # reduzir dt
    miss_geo <- setdiff(geocode_keep, names(dt))
    if (length(miss_geo) > 0) {
      stop("Ano ", year, ": faltam colunas pós-processamento para geocoding: ", paste(miss_geo, collapse = ", "))
    }
    dt <- dt[, ..geocode_keep]
  
    campos <- geocodebr::definir_campos(
      logradouro = "endereco",
      numero     = "numlograd",
      cep        = "cep",
      localidade     = "bairro",
      municipio  = "municipio_7",
      estado     = "uf_dom"
    )

    campos_pdr <- correspondencia_campos(
      logradouro = "endereco",
      numero = "numlograd",
      cep = "cep",
      bairro = "bairro",
      municipio = "municipio_7",
      estado = "uf_dom"
    )

    res <- if (isTRUE(debug_on_error)) {
      main_geocodificacao(
        dt              = dt,
        campos          = campos,
        campos_pdr      = campos_pdr,
        var_col         = "estoque",
        sd_threshold_km = sd_threshold_km
      )
    } else {
      tryCatch(
        {
          main_geocodificacao(
            dt              = dt,
            campos          = campos,
            campos_pdr      = campos_pdr,
            var_col         = "estoque",
            sd_threshold_km = sd_threshold_km
          )
        },
        error = function(e) {
          log("ERRO no ano ", year, ": ", conditionMessage(e))
          cc <- conditionCall(e)
          if (!is.null(cc)) {
            log("COND_CALL: ", paste(deparse(cc), collapse = " "))
          }
          tb <- utils::capture.output(traceback())
          log("TRACEBACK:\n", paste(tb, collapse = "\n"))
          calls <- utils::capture.output(sys.calls())
          log("CALLS:\n", paste(calls, collapse = "\n"))
          stats_list[[as.character(year)]] <<- list(year = year, status = "error", error = conditionMessage(e))
          return(NULL)
        }
      )
    }

    if (is.null(res)) {
      rm(dt); gc()
      next
    }

    dt_f  <- res
    

    # salvar RDS por ano
    rds_file <- file.path(out_dir, paste0("dt_f_",filename_prefix,"_", year, ".rds"))
    saveRDS(dt_f, rds_file)
    rds_paths[[as.character(year)]] <- rds_file

    #stats_list[[as.character(year)]] <- stats

    #stats_row <- tryCatch(
    #   {
    #     sdt <- data.table::as.data.table(stats)
    #     if (nrow(sdt) != 1L) sdt <- sdt[1]
    #     sdt[, year := year]
    #     sdt
    #   },
    #   error = function(e) data.table::data.table(year = year, status = "stats_parse_failed")
    # )

    # data.table::fwrite(
    #   stats_row,
    #   stats_file,
    #   append    = file.exists(stats_file),
    #   col.names = !file.exists(stats_file)
    #)
    rm(dt, dt_f, res)
    #rm(dt, dt_f, res, stats, stats_row)
    gc()

    log(
      "Concluído: ", year,
      " | RDS: ", rds_file,
      " | Tempo: ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2), " min"
    )
  }

  invisible(list(
    rds_paths  = rds_paths,
    stats_list = stats_list,
    out_dir    = out_dir,
    stats_file = stats_file
  ))
}


montar_rds_paths_da_pasta <- function(
  rds_dir,
  pattern = "^dt_f_(\\d{4})\\.rds$"
) {
  stopifnot(dir.exists(rds_dir))
  files <- list.files(rds_dir, pattern = "\\.rds$", full.names = TRUE)

  ok  <- grepl(pattern, basename(files))
  files <- files[ok]
  yrs <- sub(pattern, "\\1", basename(files))

  # lista nomeada por ano
  out <- as.list(files)
  names(out) <- yrs
  out
}

main_second_stage_geocoding <- function(
  # INPUT 1: pasta com RDS geocodados mistos por região/ano
  # ex.: dt_f_norte_2020.rds, dt_f_sp_2021.rds, ...
  pathname_in_rds,

  # INPUT 2: pasta com CSV "prev" por região/ano
  # ex.: norte_estb_2020_limpo.csv, sp_estb_2021_limpo.csv, ...
  pathname_in_prev_csv,

  # OUTPUT: pasta onde serão salvos os RDS finais por ano
  out_dir_rds,

  # parâmetros de processamento
  diff_threshold = 0.3,
  year_col = "year",
  keep_cols = NULL,

  # controle de nomes / região
  filename_prefix = "sp",  # aqui representa a REGIÃO no loop
  input_rds_pattern = "^dt_f_{prefix}_(\\d{4})\\.rds$",
  prev_csv_template = "{prefix}_estb_{year}_limpo.csv",
  out_suffix = "_limpo_corr.rds",

  # processamento
  geo_threshold_m = 1000,
  overwrite_rds = FALSE,
  verbose = TRUE,

  # leitura CSV
  encoding = "Latin-1",

  # colunas para puxar do CSV prev
  geocode_keep = c(
    "id", "endereco", "numlograd", "cep", "bairro", "municipio_7", "uf_dom",
    "estoque", "identificad_m", "municipio", "matrizfilial"
  )
) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Pacote 'data.table' não instalado.")
  }
  library(data.table)

  log <- function(...) {
    if (isTRUE(verbose)) {
      message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))
    }
  }

  # ---------- helpers ----------
  build_pattern <- function(pattern_template, prefix) {
    gsub("\\{prefix\\}", as.character(prefix), pattern_template)
  }

  build_filename <- function(template, prefix, year) {
    out <- template
    out <- gsub("\\{prefix\\}", as.character(prefix), out)
    out <- gsub("\\{year\\}", as.character(year), out)
    out
  }

  # Indexa arquivos por ano via regex com 1 grupo de captura (ano)
  # Retorna named vector: names = ano, values = full path
  index_rds_by_year <- function(dir_path, pattern, label = "RDS") {
    files <- list.files(dir_path, pattern = pattern, full.names = TRUE)

    if (length(files) == 0L) {
      stop("Nenhum ", label, " encontrado em ", dir_path, " com pattern: ", pattern)
    }

    bases <- basename(files)
    m <- regexec(pattern, bases)
    regs <- regmatches(bases, m)

    ok <- lengths(regs) >= 2L
    if (!all(ok)) {
      bad <- bases[!ok]
      stop("Não foi possível extrair o ano de alguns ", label, ": ", paste(bad, collapse = ", "))
    }

    years <- suppressWarnings(as.integer(vapply(regs, `[`, character(1), 2L)))
    if (anyNA(years)) {
      bad <- bases[is.na(years)]
      stop("Ano inválido em alguns ", label, ": ", paste(bad, collapse = ", "))
    }

    dup_years <- unique(years[duplicated(years)])
    if (length(dup_years) > 0L) {
      stop(
        "Há mais de um arquivo ", label, " para a mesma região/ano em ", dir_path,
        ". Anos duplicados: ", paste(sort(dup_years), collapse = ", ")
      )
    }

    out <- setNames(files, as.character(years))
    out[order(as.integer(names(out)))]
  }

  # ---------- validações ----------
  if (missing(pathname_in_rds) || !nzchar(pathname_in_rds)) {
    stop("Você precisa informar pathname_in_rds (pasta com RDS dt_f_<regiao>_<ano>.rds).")
  }
  if (!dir.exists(pathname_in_rds)) {
    stop("pathname_in_rds não existe: ", pathname_in_rds)
  }

  if (missing(pathname_in_prev_csv) || !nzchar(pathname_in_prev_csv)) {
    stop("Você precisa informar pathname_in_prev_csv (pasta com CSVs <regiao>_estb_<ano>_limpo.csv).")
  }
  if (!dir.exists(pathname_in_prev_csv)) {
    stop("pathname_in_prev_csv não existe: ", pathname_in_prev_csv)
  }

  dir.create(out_dir_rds, recursive = TRUE, showWarnings = FALSE)

  # ---------- indexa RDS da região ----------
  input_pattern_built <- build_pattern(input_rds_pattern, filename_prefix)

  rds_paths <- index_rds_by_year(
    dir_path = pathname_in_rds,
    pattern  = input_pattern_built,
    label    = paste0("RDS input (região '", filename_prefix, "')")
  )

  years <- sort(as.integer(names(rds_paths)))
  if (length(years) == 0L) {
    stop("Nenhum ano encontrado para a região '", filename_prefix, "'.")
  }

  # opcional: remover outputs antigos dessa região
  if (isTRUE(overwrite_rds)) {
    for (yy in years) {
      f <- file.path(out_dir_rds, paste0(filename_prefix, "_estb_", yy, out_suffix))
      if (file.exists(f)) unlink(f)
    }
  }

  log("Região: ", filename_prefix)
  log("Pattern RDS (resolvido): ", input_pattern_built)
  log("Anos encontrados (RDS): ", paste(years, collapse = ", "))

  # ---------- leitura/merge por ano ----------
  partes <- vector("list", length(years))
  k <- 0L

  for (yy in years) {
    p_rds <- rds_paths[[as.character(yy)]]
    if (is.null(p_rds) || !file.exists(p_rds)) next

    # 1) lê RDS geocodado da região/ano
    log("Lendo RDS ano ", yy, ": ", p_rds)
    dt_year <- as.data.table(readRDS(p_rds))

    if (!("id" %in% names(dt_year))) {
      stop("RDS do ano ", yy, " (região ", filename_prefix, ") não tem coluna 'id'.")
    }

    # 2) lê CSV prev correspondente (<regiao>_estb_<ano>_limpo.csv)
    prev_csv_name <- build_filename(prev_csv_template, filename_prefix, yy)
    p_csv <- file.path(pathname_in_prev_csv, prev_csv_name)

    if (!file.exists(p_csv)) {
      stop(
        "Não achei CSV prev para a região '", filename_prefix, "' e ano ", yy, ": ", p_csv, "\n",
        "Template usado: ", prev_csv_template
      )
    }

    log("Lendo CSV prev ano ", yy, ": ", p_csv)

    # cabeçalho para selecionar só colunas úteis
    cols_available <- names(data.table::fread(
      p_csv, nrows = 0, encoding = encoding, showProgress = FALSE
    ))

    needed_min_raw <- c("municipio", "cduf", "cep", "endereco", "estoque")
    read_cols <- unique(c(
      needed_min_raw, "bairro", "numlograd", "identificad_m", "matrizfilial", "id"
    ))

    dt_prev <- data.table::fread(
      p_csv,
      encoding = encoding,
      select = intersect(read_cols, cols_available),
      showProgress = FALSE
    )

    # 3) checagens e pré-processamento do CSV prev
    miss_base <- setdiff(needed_min_raw, names(dt_prev))
    if (length(miss_base) > 0L) {
      stop(
        "Ano ", yy, " (região ", filename_prefix, "): CSV prev não tem colunas mínimas: ",
        paste(miss_base, collapse = ", ")
      )
    }

    if (!("bairro" %in% names(dt_prev)))    dt_prev[, bairro := NA_character_]
    if (!("numlograd" %in% names(dt_prev))) dt_prev[, numlograd := NA]

    # garante / cria municipio_7
    if (!("municipio_7" %in% names(dt_prev))) {
      dt_prev[, municipio_7 := ibge6_to_7(municipio)]
    }

    # garante / cria uf_dom
    if (!("uf_dom" %in% names(dt_prev))) {
      dt_prev <- add_sigla_from_uf(dt_prev, "cduf", "uf_dom")
    }

    # normaliza CEP
    dt_prev <- change_cep_99999999_to_na(dt_prev, "cep", "municipio")

    # garante id
    if (!("id" %in% names(dt_prev))) {
      stop(
        "Ano ", yy, " (região ", filename_prefix, "): CSV prev não tem coluna 'id'. ",
        "Inclua 'id' no CSV ou recrie aqui com a mesma regra do pipeline anterior."
      )
      # EXEMPLO (NÃO USAR SEM CONFIRMAR SUA REGRA):
      # dt_prev[, id := paste0(municipio, "_", cep, "_", endereco, "_", numlograd)]
    }

    # reduz prev para colunas desejadas
    keep_prev <- intersect(geocode_keep, names(dt_prev))
    dt_prev <- dt_prev[, ..keep_prev]

    # 4) merge por id: mantém tudo do RDS e completa com prev
    log("Merge por id (RDS + CSV prev) ano ", yy)
    dt_merged <- merge(
      x = dt_year,
      y = dt_prev,
      by = "id",
      all.x = TRUE,
      suffixes = c("", ".in")
    )

    # preenche NA das colunas do RDS com valores do prev (*.in)
    cols_in <- grep("\\.in$", names(dt_merged), value = TRUE)
    for (cin in cols_in) {
      cbase <- sub("\\.in$", "", cin)

      if (cbase %in% names(dt_merged)) {
        idx_fill <- which(is.na(dt_merged[[cbase]]) & !is.na(dt_merged[[cin]]))
        if (length(idx_fill) > 0L) {
          set(dt_merged, i = idx_fill, j = cbase, value = dt_merged[[cin]][idx_fill])
        }
        dt_merged[, (cin) := NULL]
      } else {
        setnames(dt_merged, cin, cbase)
      }
    }

    # reduz colunas (opcional)
    if (!is.null(keep_cols)) {
      keep <- intersect(keep_cols, names(dt_merged))
      dt_merged <- dt_merged[, ..keep]
    }

    # garante coluna de ano
    if (!(year_col %in% names(dt_merged))) {
      dt_merged[, (year_col) := yy]
    } else {
      dt_merged[, (year_col) := as.integer(get(year_col))]
      idx_na_year <- which(is.na(dt_merged[[year_col]]))
      if (length(idx_na_year) > 0L) {
        set(dt_merged, i = idx_na_year, j = year_col, value = yy)
      }
    }

    k <- k + 1L
    partes[[k]] <- dt_merged

    rm(dt_year, dt_prev, dt_merged)
    gc()
  }

  if (k == 0L) {
    stop("Nenhum RDS/CSV válido foi lido e mesclado para a região '", filename_prefix, "'.")
  }

  # ---------- empilha tudo ----------
  dt_all <- rbindlist(partes[seq_len(k)], use.names = TRUE, fill = TRUE)
  rm(partes)
  gc()

  log("dt_all montado (região ", filename_prefix, "). Linhas: ", nrow(dt_all), " | Colunas: ", ncol(dt_all))

  # ---------- processamento multianual ----------
  log("Rodando main_multiyear() para região ", filename_prefix, " ...")
  res <- main_multiyear(
    dt_all,
    diff_threshold = diff_threshold,
    geo_threshold_m = geo_threshold_m
  )

  dt_final <- data.table::as.data.table(res$dt_final)

  if (!(year_col %in% names(dt_final))) {
    stop("dt_final não tem a coluna '", year_col, "'. Garanta que main_multiyear preserve essa coluna.")
  }

  # ---------- salva 1 RDS por ano ----------
  yrs_out <- sort(unique(as.integer(dt_final[[year_col]])))
  log("Salvando RDS por ano (região ", filename_prefix, "): ", paste(yrs_out, collapse = ", "))

  out_paths <- vector("list", length(yrs_out))
  names(out_paths) <- as.character(yrs_out)

  for (yy in yrs_out) {
    out_file <- file.path(out_dir_rds, paste0(filename_prefix, "_estb_", yy, out_suffix))

    # garante extensão .rds
    if (!grepl("\\.rds$", out_file, ignore.case = TRUE)) {
      out_file <- sub("\\.csv$", ".rds", out_file, ignore.case = TRUE)
      if (!grepl("\\.rds$", out_file, ignore.case = TRUE)) {
        out_file <- paste0(out_file, ".rds")
      }
    }

    dt_out <- dt_final[get(year_col) == yy]

    if (!("geometry_updated" %in% names(dt_out))) {
      stop("Ano ", yy, ": falta geometry_updated no output.")
    }
    if (!inherits(dt_out[["geometry_updated"]], "sfc")) {
      stop("Ano ", yy, ": geometry_updated não é sfc.")
    }

    saveRDS(dt_out, out_file)
    out_paths[[as.character(yy)]] <- out_file
    log("  - OK: ", out_file)
  }

  rm(dt_all, res, dt_final)
  gc()

  invisible(list(
    region = filename_prefix,
    years_found = years,
    years_out = yrs_out,
    out_dir_rds = out_dir_rds,
    rds_paths = out_paths,
    input_rds_pattern = input_pattern_built,
    prev_csv_template = prev_csv_template
  ))
}


# main_second_stage_geocoding <- function(
#   out_dir_rds,
#   rds_subdir = "geocoded_rds",
#   rds_pattern = "^dt_f_(\\d{4})\\.rds$",
#   diff_threshold = 0.3,
#   year_col = "year",
#   keep_cols = NULL,

#   # output RDS
#   filename_prefix = "sp",
#   out_suffix = "_limpo_corr.rds",
#   geo_threshold_m = 1000,
#   overwrite_rds = FALSE,
#   verbose = TRUE,

#   # input RDS (1º estágio) - pasta com arquivos por ano
#   pathname_in_rds,
#   in_rds_suffix = "_limpo.rds",

#   # colunas para puxar do input
#   geocode_keep = c(
#     "id", "endereco", "numlograd", "cep", "bairro", "municipio_7", "uf_dom",
#     "estoque", "identificad_m", "municipio", "matrizfilial"
#   )
# ) {
#   if (!requireNamespace("data.table", quietly = TRUE)) {
#     stop("Pacote 'data.table' não instalado.")
#   }
#   library(data.table)

#   log <- function(...) {
#     if (isTRUE(verbose)) {
#       message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))
#     }
#   }

#   dir.create(out_dir_rds, recursive = TRUE, showWarnings = FALSE)


#   rds_paths <- montar_rds_paths_da_pasta(rds_dir, pattern = rds_pattern)
#   if (length(rds_paths) == 0L) {
#     stop("Nenhum RDS encontrado em ", rds_dir, " com padrão ", rds_pattern)
#   }

#   years <- sort(as.integer(names(rds_paths)))

#   # valida input RDS do 1º estágio
#   if (missing(pathname_in_rds) || !nzchar(pathname_in_rds)) {
#     stop("Você precisa informar pathname_in_rds (pasta com os RDS de input do 1º estágio).")
#   }
#   if (!dir.exists(pathname_in_rds)) {
#     stop("pathname_in_rds não existe: ", pathname_in_rds)
#   }

#   # opcional: limpar outputs antigos
#   if (isTRUE(overwrite_rds)) {
#     for (yy in years) {
#       f <- file.path(out_dir_rds, paste0(filename_prefix, "_estb_", yy, out_suffix))
#       if (file.exists(f)) unlink(f)
#     }
#   }

#   log("Lendo e empilhando anos: ", paste(years, collapse = ", "))

#   partes <- vector("list", length(rds_paths))
#   k <- 0L

#   for (y in names(rds_paths)) {
#     yy <- as.integer(y)
#     p <- rds_paths[[y]]

#     if (is.null(p) || !nzchar(p) || !file.exists(p)) next

#     # --- lê RDS processado do ano (saída intermediária)
#     log("Lendo RDS ano ", y, ": ", p)
#     dt_year <- as.data.table(readRDS(p))

#     if (!("id" %in% names(dt_year))) {
#       stop("RDS do ano ", y, " não tem coluna 'id'. Não dá pra fazer merge por id.")
#     }

#     # --- lê RDS de input do 1º estágio (por ano)
#     # ex.: sp_estb_2010_limpo.rds
#     file_in <- file.path(
#       pathname_in_rds,
#       paste0(filename_prefix, "_estb_", yy, in_rds_suffix)
#     )

#     if (!file.exists(file_in)) {
#       stop("Não achei o RDS de input do 1º estágio para o ano ", y, ": ", file_in)
#     }

#     needed_min_raw <- c("municipio", "cduf", "cep", "endereco", "estoque")
#     read_cols <- unique(c(
#       needed_min_raw, "bairro", "numlograd", "identificad_m", "matrizfilial", "id"
#     ))

#     log("Lendo RDS input ano ", y, ": ", file_in)
#     dt_in <- as.data.table(readRDS(file_in))

#     # reduz para colunas úteis (RAM)
#     keep_read <- intersect(read_cols, names(dt_in))
#     dt_in <- dt_in[, ..keep_read]

#     # garantir colunas base mínimas
#     miss_base <- setdiff(needed_min_raw, names(dt_in))
#     if (length(miss_base) > 0) {
#       stop("Ano ", y, ": RDS input não tem colunas mínimas: ", paste(miss_base, collapse = ", "))
#     }

#     if (!("bairro" %in% names(dt_in)))    dt_in[, bairro := NA_character_]
#     if (!("numlograd" %in% names(dt_in))) dt_in[, numlograd := NA]

#     # --- pré-processamentos (iguais ao 1º estágio), mas defensivos
#     if (!("municipio_7" %in% names(dt_in))) {
#       dt_in[, municipio_7 := ibge6_to_7(municipio)]
#     }

#     if (!("uf_dom" %in% names(dt_in))) {
#       dt_in <- add_sigla_from_uf(dt_in, "cduf", "uf_dom")
#     }

#     # sempre pode aplicar (se sua função já trata adequadamente)
#     dt_in <- change_cep_99999999_to_na(dt_in, "cep", "municipio")

#     # --- garantir id no input
#     if (!("id" %in% names(dt_in))) {
#       stop(
#         "Ano ", y, ": RDS input não tem coluna 'id'. ",
#         "Inclua 'id' no RDS do 1º estágio OU recrie aqui com a mesma regra usada antes."
#       )
#       # EXEMPLO (NÃO USAR SEM CONFIRMAR SUA REGRA):
#       # dt_in[, id := paste0(municipio, "_", cep, "_", endereco, "_", numlograd)]
#     }

#     # reduz input para geocode_keep (somente as que existirem)
#     keep_in <- intersect(geocode_keep, names(dt_in))
#     dt_in <- dt_in[, ..keep_in]

#     # --- merge por id: mantém tudo do RDS intermediário e completa com input
#     log("Merge por id (RDS intermediário + RDS input) ano ", y)
#     dt_merged <- merge(
#       x = dt_year,
#       y = dt_in,
#       by = "id",
#       all.x = TRUE,
#       suffixes = c("", ".in")
#     )

#     # preenche NAs do RDS intermediário com valores do input
#     cols_in <- grep("\\.in$", names(dt_merged), value = TRUE)
#     for (cin in cols_in) {
#       cbase <- sub("\\.in$", "", cin)

#       if (cbase %in% names(dt_merged)) {
#         idx_fill <- which(is.na(dt_merged[[cbase]]) & !is.na(dt_merged[[cin]]))
#         if (length(idx_fill)) {
#           set(dt_merged, i = idx_fill, j = cbase, value = dt_merged[[cin]][idx_fill])
#         }
#         dt_merged[, (cin) := NULL]
#       } else {
#         setnames(dt_merged, cin, cbase)
#       }
#     }

#     # reduz colunas para RAM (opcional)
#     if (!is.null(keep_cols)) {
#       keep <- intersect(keep_cols, names(dt_merged))
#       dt_merged <- dt_merged[, ..keep]
#     }

#     # garante coluna year
#     if (!(year_col %in% names(dt_merged))) {
#       dt_merged[, (year_col) := yy]
#     } else {
#       dt_merged[, (year_col) := as.integer(get(year_col))]
#     }

#     k <- k + 1L
#     partes[[k]] <- dt_merged

#     rm(dt_year, dt_in, dt_merged)
#     gc()
#   }

#   if (k == 0L) stop("Nenhum RDS válido foi lido/mesclado.")

#   dt_all <- rbindlist(partes[seq_len(k)], use.names = TRUE, fill = TRUE)
#   rm(partes)
#   gc()

#   log("dt_all montado. Linhas: ", nrow(dt_all), " | Colunas: ", ncol(dt_all))

#   # roda tudo de uma vez
#   log("Rodando main_multiyear()...")
#   res <- main_multiyear(
#     dt_all,
#     diff_threshold = diff_threshold,
#     geo_threshold_m = geo_threshold_m
#   )

#   dt_final <- data.table::as.data.table(res$dt_final)

#   if (!(year_col %in% names(dt_final))) {
#     stop("dt_final não tem a coluna '", year_col, "'. Garanta que main_multiyear preserve essa coluna.")
#   }

#   # escreve 1 RDS por ano (mantém geometry_updated como sfc)
#   yrs_out <- sort(unique(dt_final[[year_col]]))
#   log("Salvando RDS por ano: ", paste(yrs_out, collapse = ", "))

#   out_paths <- vector("list", length(yrs_out))
#   names(out_paths) <- as.character(yrs_out)

#   for (yy in yrs_out) {
#     out_file <- file.path(out_dir_rds, paste0(filename_prefix, "_estb_", yy, out_suffix))

#     # garante extensão .rds
#     if (!grepl("\\.rds$", out_file, ignore.case = TRUE)) {
#       out_file <- sub("\\.csv$", ".rds", out_file, ignore.case = TRUE)
#       if (!grepl("\\.rds$", out_file, ignore.case = TRUE)) {
#         out_file <- paste0(out_file, ".rds")
#       }
#     }

#     dt_y <- dt_final[get(year_col) == yy]

#     if (!("geometry_updated" %in% names(dt_y))) {
#       stop("Ano ", yy, ": falta geometry_updated no output.")
#     }
#     if (!inherits(dt_y[["geometry_updated"]], "sfc")) {
#       stop("Ano ", yy, ": geometry_updated não é sfc.")
#     }

#     saveRDS(dt_y, out_file)
#     out_paths[[as.character(yy)]] <- out_file
#     log("  - OK: ", out_file)
#   }

#   rm(dt_all, res, dt_final)
#   gc()

#   invisible(list(
#     years = yrs_out,
#     out_dir_rds = out_dir_rds,
#     rds_paths = out_paths
#   ))
# }

# main_second_stage_geocoding <- function(
#   pathname_out_prev,
#   out_dir_csv,
#   rds_subdir = "geocoded_rds",
#   rds_pattern = "^dt_f_(\\d{4})\\.rds$",
#   diff_threshold = 0.3,
#   year_col = "year",
#   keep_cols = NULL,
#   # padrão do nome do CSV final por ano
#   filename_prefix = "sp",
#   out_suffix = "_limpo_corr.csv",
#   geo_threshold_m = 1000,
#   overwrite_csv = FALSE,
#   verbose = TRUE
# ) {
#   log <- function(...) if (isTRUE(verbose)) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))
#   dir.create(out_dir_csv, recursive = TRUE, showWarnings = FALSE)

#   rds_dir <- file.path(pathname_out_prev, rds_subdir)
#   if (!dir.exists(rds_dir)) stop("Pasta RDS não encontrada: ", rds_dir)

#   rds_paths <- montar_rds_paths_da_pasta(rds_dir, pattern = rds_pattern)
#   if (length(rds_paths) == 0L) stop("Nenhum RDS encontrado em ", rds_dir, " com padrão ", rds_pattern)

#   years <- sort(as.integer(names(rds_paths)))

#   # opcional: limpar outputs antigos
#   if (isTRUE(overwrite_csv)) {
#     for (yy in years) {
#       f <- file.path(out_dir_csv, paste0(filename_prefix, "_estb_", yy, out_suffix))
#       if (file.exists(f)) unlink(f)
#     }
#   }

#   log("Lendo e empilhando anos: ", paste(years, collapse = ", "))

#   partes <- vector("list", length(rds_paths))
#   k <- 0L

#   for (y in names(rds_paths)) {
#     p <- rds_paths[[y]]
#     if (is.null(p) || !nzchar(p) || !file.exists(p)) next

#     log("Lendo ano ", y, ": ", p)
#     dt_year <- readRDS(p)

#     # reduz colunas para RAM (opcional)
#     if (!is.null(keep_cols)) {
#       keep <- intersect(keep_cols, names(dt_year))
#       dt_year <- dt_year[, ..keep]
#     }

#     # garante coluna year
#     if (!(year_col %in% names(dt_year))) {
#       dt_year[, (year_col) := as.integer(y)]
#     } else {
#       dt_year[, (year_col) := as.integer(get(year_col))]
#     }

#     k <- k + 1L
#     partes[[k]] <- dt_year
#     rm(dt_year); gc()
#   }

#   if (k == 0L) stop("Nenhum RDS válido foi lido.")

#   dt_all <- rbindlist(partes[seq_len(k)], use.names = TRUE, fill = TRUE)
#   rm(partes); gc()

#   log("dt_all montado. Linhas: ", nrow(dt_all), " | Colunas: ", ncol(dt_all))

#   # roda tudo de uma vez
#   log("Rodando main_multiyear()...")
#   res <- main_multiyear(dt_all, diff_threshold = diff_threshold, geo_threshold_m = geo_threshold_m)

#   dt_final <- as.data.table(res$dt_final)

#   if (!(year_col %in% names(dt_final))) {
#     stop("dt_final não tem a coluna '", year_col, "'. Garanta que main_multiyear preserve essa coluna.")
#   }

#   # escreve 1 CSV por ano
#   yrs_out <- sort(unique(dt_final[[year_col]]))
#   log("Salvando CSVs por ano: ", paste(yrs_out, collapse = ", "))

#   for (yy in yrs_out) {
#     out_file <- file.path(out_dir_csv, paste0(filename_prefix, "_estb_", yy, out_suffix))
#     fwrite(dt_final[get(year_col) == yy], out_file)
#     log("  - OK: ", out_file)
#   }

#   # limpeza
#   rm(dt_all, res, dt_final); gc()

#   invisible(list(
#     years = yrs_out,
#     out_dir_csv = out_dir_csv
#   ))
# }


# main_second_stage_geocoding <- function(
#   pathname_out_prev,
#   out_dir_csv,
#   rds_subdir = "geocoded_rds",
#   rds_pattern = "^dt_f_(\\d{4})\\.rds$",
#   diff_threshold = 0.3,
#   year_col = "year",
#   keep_cols = NULL,

#   # output CSV
#   filename_prefix = "sp",
#   out_suffix = "_limpo_corr.csv",
#   geo_threshold_m = 1000,
#   overwrite_csv = FALSE,
#   verbose = TRUE,

#   # --- NOVO: onde estão os CSVs de input do 1º estágio (pasta "data" que contém raw/rais_temp)
#   pathname_in_first_stage,
#   encoding = "Latin-1",

#   # --- NOVO: quais colunas puxar do input (tipicamente geocode_keep)
#   geocode_keep = c("id","endereco","numlograd","cep","bairro","municipio_7","uf_dom",
#                    "estoque","identificad_m","municipio","matrizfilial")
# ) {
#   if (!requireNamespace("data.table", quietly = TRUE)) stop("Pacote 'data.table' não instalado.")
#   library(data.table)

#   log <- function(...) if (isTRUE(verbose)) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))
#   dir.create(out_dir_csv, recursive = TRUE, showWarnings = FALSE)

#   rds_dir <- file.path(pathname_out_prev, rds_subdir)
#   if (!dir.exists(rds_dir)) stop("Pasta RDS não encontrada: ", rds_dir)

#   rds_paths <- montar_rds_paths_da_pasta(rds_dir, pattern = rds_pattern)
#   if (length(rds_paths) == 0L) stop("Nenhum RDS encontrado em ", rds_dir, " com padrão ", rds_pattern)

#   years <- sort(as.integer(names(rds_paths)))

#   # opcional: limpar outputs antigos
#   if (isTRUE(overwrite_csv)) {
#     for (yy in years) {
#       f <- file.path(out_dir_csv, paste0(filename_prefix, "_estb_", yy, out_suffix))
#       if (file.exists(f)) unlink(f)
#     }
#   }

#   if (missing(pathname_in_first_stage) || !nzchar(pathname_in_first_stage)) {
#     stop("Você precisa informar pathname_in_first_stage (a pasta que contém raw/rais_temp do 1º estágio).")
#   }
#   if (!dir.exists(pathname_in_first_stage)) stop("pathname_in_first_stage não existe: ", pathname_in_first_stage)

#   log("Lendo e empilhando anos: ", paste(years, collapse = ", "))

#   partes <- vector("list", length(rds_paths))
#   k <- 0L

#   for (y in names(rds_paths)) {
#     yy <- as.integer(y)
#     p <- rds_paths[[y]]
#     if (is.null(p) || !nzchar(p) || !file.exists(p)) next

#     # --- lê RDS do ano
#     log("Lendo RDS ano ", y, ": ", p)
#     dt_year <- as.data.table(readRDS(p))

#     if (!("id" %in% names(dt_year))) {
#       stop("RDS do ano ", y, " não tem coluna 'id'. Não dá pra fazer merge por id.")
#     }

#     # --- lê CSV input do 1º estágio (mesmo padrão do first_stage)
#     filename <- paste0(filename_prefix, "_estb_", yy, "_limpo")
#     file_in <- file.path(pathname_in_first_stage, "raw", "rais_temp", paste0(filename, ".csv"))

#     if (!file.exists(file_in)) {
#       stop("Não achei o CSV de input do 1º estágio para o ano ", y, ": ", file_in)
#     }

#     # lê só o mínimo necessário para recriar colunas do geocode_keep
#     # (se algumas já existirem no CSV, ótimo)
#     needed_min_raw <- c("municipio", "cduf", "cep", "endereco", "estoque")
#     read_cols <- unique(c(needed_min_raw, "bairro", "numlograd", "identificad_m", "matrizfilial", "id"))

#     log("Lendo CSV input ano ", y, ": ", file_in)
#     dt_in <- fread(file_in, encoding = encoding, select = intersect(read_cols, names(fread(file_in, nrows=0, encoding=encoding))))

#     # garantir colunas base
#     miss_base <- setdiff(needed_min_raw, names(dt_in))
#     if (length(miss_base) > 0) {
#       stop("Ano ", y, ": CSV input não tem colunas mínimas: ", paste(miss_base, collapse = ", "))
#     }
#     if (!("bairro" %in% names(dt_in)))    dt_in[, bairro := NA_character_]
#     if (!("numlograd" %in% names(dt_in))) dt_in[, numlograd := NA]

#     # --- pré-processamentos para chegar nas colunas do geocode_keep
#     # (iguais ao 1º estágio)
#     dt_in[, municipio_7 := ibge6_to_7(municipio)]
#     dt_in <- add_sigla_from_uf(dt_in, "cduf", "uf_dom")
#     dt_in <- change_cep_99999999_to_na(dt_in, "cep", "municipio")

#     # --- IMPORTANTE: garantir 'id' no input
#     # Se o CSV já tiver id, ok. Se NÃO tiver, você precisa recriar exatamente
#     # como no seu pipeline. Substitua o trecho abaixo pela sua regra real.
#     if (!("id" %in% names(dt_in))) {
#       stop(
#         "Ano ", y, ": CSV input não tem coluna 'id'. ",
#         "Inclua 'id' no arquivo do 1º estágio OU recrie aqui com a mesma regra usada antes."
#       )
#       # EXEMPLO (NÃO USAR SEM CONFIRMAR SUA REGRA):
#       # dt_in[, id := paste0(municipio, "_", cep, "_", endereco, "_", numlograd)]
#     }

#     # reduz input para geocode_keep (somente as que existirem)
#     keep_in <- intersect(geocode_keep, names(dt_in))
#     dt_in <- dt_in[, ..keep_in]

#     # --- merge por id: mantém tudo do RDS e completa com colunas do input
#     # preferindo valores do RDS quando já existirem
#     log("Merge por id (RDS + input) ano ", y)
#     dt_merged <- merge(
#       x = dt_year,
#       y = dt_in,
#       by = "id",
#       all.x = TRUE,
#       suffixes = c("", ".in")
#     )

#     # se a coluna existe no RDS e também veio do input como *.in,
#     # preenche NA do RDS com valor do input e remove *.in
#     cols_in <- grep("\\.in$", names(dt_merged), value = TRUE)
#     for (cin in cols_in) {
#       cbase <- sub("\\.in$", "", cin)
#       if (cbase %in% names(dt_merged)) {
#         set(dt_merged, which(is.na(dt_merged[[cbase]]) & !is.na(dt_merged[[cin]])), cbase, dt_merged[[cin]][which(is.na(dt_merged[[cbase]]) & !is.na(dt_merged[[cin]]))])
#         dt_merged[, (cin) := NULL]
#       } else {
#         setnames(dt_merged, cin, cbase)
#       }
#     }

#     # reduz colunas para RAM (opcional)
#     if (!is.null(keep_cols)) {
#       keep <- intersect(keep_cols, names(dt_merged))
#       dt_merged <- dt_merged[, ..keep]
#     }

#     # garante coluna year
#     if (!(year_col %in% names(dt_merged))) {
#       dt_merged[, (year_col) := yy]
#     } else {
#       dt_merged[, (year_col) := as.integer(get(year_col))]
#     }

#     k <- k + 1L
#     partes[[k]] <- dt_merged

#     rm(dt_year, dt_in, dt_merged); gc()
#   }

#   if (k == 0L) stop("Nenhum RDS válido foi lido/mesclado.")

#   dt_all <- rbindlist(partes[seq_len(k)], use.names = TRUE, fill = TRUE)
#   rm(partes); gc()

#   log("dt_all montado. Linhas: ", nrow(dt_all), " | Colunas: ", ncol(dt_all))

#   # roda tudo de uma vez
#   log("Rodando main_multiyear()...")
#   res <- main_multiyear(dt_all, diff_threshold = diff_threshold, geo_threshold_m = geo_threshold_m)

#   dt_final <- data.table::as.data.table(res$dt_final)

#   if (!(year_col %in% names(dt_final))) {
#     stop("dt_final não tem a coluna '", year_col, "'. Garanta que main_multiyear preserve essa coluna.")
#   }

#   # escreve 1 RDS por ano (mantém geometry_updated como sfc)
#   yrs_out <- sort(unique(dt_final[[year_col]]))
#   log("Salvando RDS por ano: ", paste(yrs_out, collapse = ", "))

#   out_paths <- vector("list", length(yrs_out))
#   names(out_paths) <- as.character(yrs_out)

#   for (yy in yrs_out) {
#     out_file <- file.path(out_dir_csv, paste0(filename_prefix, "_estb_", yy, out_suffix))

#     # garante extensão .rds
#     if (!grepl("\\.rds$", out_file, ignore.case = TRUE)) {
#       out_file <- sub("\\.csv$", ".rds", out_file, ignore.case = TRUE)
#       if (!grepl("\\.rds$", out_file, ignore.case = TRUE)) out_file <- paste0(out_file, ".rds")
#     }

#     dt_y <- dt_final[get(year_col) == yy]

#     if (!("geometry_updated" %in% names(dt_y))) stop("Ano ", yy, ": falta geometry_updated no output.")
#     if (!inherits(dt_y[["geometry_updated"]], "sfc")) stop("Ano ", yy, ": geometry_updated não é sfc.")

#     saveRDS(dt_y, out_file)
#     out_paths[[as.character(yy)]] <- out_file
#     log("  - OK: ", out_file)
#   }

#   rm(dt_all, res, dt_final); gc()

#   invisible(list(
#     years = yrs_out,
#     out_dir_rds = out_dir_csv,
#     rds_paths = out_paths
#   ))

  # # roda tudo de uma vez
  # log("Rodando main_multiyear()...")
  # res <- main_multiyear(dt_all, diff_threshold = diff_threshold, geo_threshold_m = geo_threshold_m)

  # dt_final <- data.table::as.data.table(res$dt_final) 

  # if (!(year_col %in% names(dt_final))) {
  #   stop("dt_final não tem a coluna '", year_col, "'. Garanta que main_multiyear preserve essa coluna.")
  # }

  # # escreve 1 RDS por ano (mantém geometry_updated como sfc)
  # yrs_out <- sort(unique(dt_final[[year_col]]))
  # log("Salvando RDS por ano: ", paste(yrs_out, collapse = ", "))

  # out_paths <- vector("list", length(yrs_out))
  # names(out_paths) <- as.character(yrs_out)

  # for (yy in yrs_out) {
  #   out_file <- file.path(out_dir_csv, paste0(filename_prefix, "_estb_", yy, out_suffix))
  #   # garante extensão .rds (se out_suffix vier ".csv", você pode forçar aqui)
  #   if (!grepl("\\.rds$", out_file, ignore.case = TRUE)) {
  #     out_file <- sub("\\.csv$", ".rds", out_file, ignore.case = TRUE)
  #     if (!grepl("\\.rds$", out_file, ignore.case = TRUE)) out_file <- paste0(out_file, ".rds")
  #   }

  #   dt_y <- dt_final[get(year_col) == yy]

  #   # checagem: garantir que a geometria está lá e é sfc
  #   if (!("geometry_updated" %in% names(dt_y))) stop("Ano ", yy, ": falta geometry_updated no output.")
  #   if (!inherits(dt_y[["geometry_updated"]], "sfc")) stop("Ano ", yy, ": geometry_updated não é sfc.")

  #   saveRDS(dt_y, out_file)
  #   out_paths[[as.character(yy)]] <- out_file
  #   log("  - OK: ", out_file)
  # }

  # rm(dt_all, res, dt_final); gc()

  # invisible(list(
  #   years = yrs_out,
  #   out_dir_rds = out_dir_csv,
  #   rds_paths = out_paths
  # ))

  # # escreve 1 CSV por ano
  # yrs_out <- sort(unique(dt_final[[year_col]]))
  # log("Salvando CSVs por ano: ", paste(yrs_out, collapse = ", "))

  # for (yy in yrs_out) {
  #   out_file <- file.path(out_dir_csv, paste0(filename_prefix, "_estb_", yy, out_suffix))
  #   fwrite(dt_final[get(year_col) == yy], out_file)
  #   log("  - OK: ", out_file)
  # }

  # rm(dt_all, res, dt_final); gc()

  # invisible(list(
  #   years = yrs_out,
  #   out_dir_csv = out_dir_csv
  # ))
# }

