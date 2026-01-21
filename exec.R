main_first_stage_geocoding <- function(
  pathname_in,
  pathname_out,
  years = 2023:2018,
  encoding = "Latin-1",
  sd_threshold_km = 0.3,
  verbose = TRUE,
  filename_prefix = "sp",
  filter_municipio_7 = NULL,
  select_cols = NULL,
  stats_filename = "stats_geocoding.csv",
  overwrite = FALSE
) {
  stopifnot(
    dir.exists(pathname_in),
    is.character(pathname_out), length(pathname_out) == 1L,
    is.numeric(years), length(years) >= 1L,
    is.numeric(sd_threshold_km), length(sd_threshold_km) == 1L,
    is.character(filename_prefix), length(filename_prefix) == 1L
  )

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Pacote 'data.table' não instalado.")
  }

  log <- function(...) {
    if (isTRUE(verbose)) {
      message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))
    }
  }

  # Pasta de saída para RDS
  out_dir <- file.path(pathname_out, "geocoded_rds")
  if (overwrite && dir.exists(out_dir)) {
    log("Removendo saída anterior (overwrite=TRUE): ", out_dir)
    unlink(out_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Stats em CSV (append)
  stats_file <- file.path(out_dir, stats_filename)
  if (overwrite && file.exists(stats_file)) {
    unlink(stats_file, force = TRUE)
  }

  years <- as.integer(years)

  # Em vez de guardar dt gigante, guardamos o caminho do arquivo salvo
  rds_paths <- setNames(vector("list", length(years)), as.character(years))
  stats_list <- setNames(vector("list", length(years)), as.character(years))

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

    dt <- data.table::fread(file_in, encoding = encoding, select = select_cols)
    log("Linhas lidas: ", nrow(dt))

    # Ajuste esta lista conforme seu schema real
    needed <- c("municipio", "cduf", "cep", "bairro", "endereco", "estoque")
    missing_cols <- setdiff(needed, names(dt))
    if (length(missing_cols) > 0) {
      stop("Ano ", year, ": faltam colunas no CSV: ", paste(missing_cols, collapse = ", "))
    }

    if (!("numlograd" %in% names(dt))) dt[, numlograd := NA]

    # Pré-processamentos (mantidos)
    dt[, municipio_7 := ibge6_to_7(municipio)]
    dt <- add_sigla_from_uf(dt, "cduf", "uf_dom")
    dt <- change_cep_99999999_to_na(dt, "cep", "municipio")
    dt <- add_col_if_missing(dt, "numlograd", NA)

    if (!is.null(filter_municipio_7)) {
      dt <- dt[municipio_7 %chin% as.character(filter_municipio_7)]
      log("Filtro municipio_7 aplicado. Linhas após filtro: ", nrow(dt))
    }

    campos <- correspondencia_campos(
      logradouro = "endereco",
      numero     = "numlograd",
      cep        = "cep",
      bairro     = "bairro",
      municipio  = "municipio_7",
      estado     = "uf_dom"
    )

    res <- tryCatch(
      {
        main_geocodificacao(
          dt              = dt,
          campos          = campos,
          var_col         = "estoque",
          sd_threshold_km = sd_threshold_km
        )
      },
      error = function(e) {
        log("ERRO no ano ", year, ": ", conditionMessage(e))
        stats_list[[as.character(year)]] <<- list(year = year, status = "error", error = conditionMessage(e))
        return(NULL)
      }
    )

    if (is.null(res)) {
      rm(dt)
      gc()
      next
    }

    dt_f  <- res$dt_f
    stats <- res$stats

    # Salvar RDS por ano (mantém geometry/sfc/sf intacto)
    rds_file <- file.path(out_dir, paste0("dt_f_", year, ".rds"))
    saveRDS(dt_f, rds_file)
    rds_paths[[as.character(year)]] <- rds_file

    # Guardar stats também em memória (pequeno) + append em CSV
    stats_list[[as.character(year)]] <- stats

    stats_row <- tryCatch(
      {
        sdt <- data.table::as.data.table(stats)
        if (nrow(sdt) != 1L) sdt <- sdt[1]
        sdt[, year := year]
        sdt
      },
      error = function(e) {
        data.table::data.table(year = year, status = "stats_parse_failed")
      }
    )

    data.table::fwrite(
      stats_row,
      stats_file,
      append = file.exists(stats_file),
      col.names = !file.exists(stats_file)
    )

    # Limpeza de RAM
    rm(dt, dt_f, res, stats, stats_row)
    gc()

    log(
      "Concluído: ", year,
      " | RDS: ", rds_file,
      " | Tempo: ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2), " min"
    )
  }

  invisible(list(
    rds_paths  = rds_paths,   # lista de caminhos (append-friendly, sem RAM)
    stats_list = stats_list,  # stats pequenos
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

listar_municipios_nos_rds <- function(
  rds_paths,
  municipio_col = "municipio_7",
  verbose = TRUE
) {
  log <- function(...) if (isTRUE(verbose)) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))

  mun_all <- character(0)

  for (y in names(rds_paths)) {
    p <- rds_paths[[y]]
    if (is.null(p) || !nzchar(p) || !file.exists(p)) next

    log("Lendo ano ", y, " para listar municipios: ", p)
    dt <- readRDS(p)

    if (!(municipio_col %in% names(dt))) {
      stop("Coluna '", municipio_col, "' não existe no RDS: ", p)
    }

    mun_all <- unique(c(mun_all, as.character(unique(dt[[municipio_col]]))))

    rm(dt); gc()
  }

  mun_all <- mun_all[!is.na(mun_all) & nzchar(mun_all)]
  sort(unique(mun_all))
}

library(data.table)

main_second_stage_geocoding <- function(
  pathname_out_prev,
  out_dir_csv,
  rds_subdir = "geocoded_rds",
  rds_pattern = "^dt_f_(\\d{4})\\.rds$",
  diff_threshold = 0.3,
  year_col = "year",
  keep_cols = NULL,
  # padrão do nome do CSV final por ano
  filename_prefix = "sp",
  out_suffix = "_limpo_corr.csv",
  overwrite_csv = FALSE,
  verbose = TRUE
) {
  log <- function(...) if (isTRUE(verbose)) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))
  dir.create(out_dir_csv, recursive = TRUE, showWarnings = FALSE)

  rds_dir <- file.path(pathname_out_prev, rds_subdir)
  if (!dir.exists(rds_dir)) stop("Pasta RDS não encontrada: ", rds_dir)

  rds_paths <- montar_rds_paths_da_pasta(rds_dir, pattern = rds_pattern)
  if (length(rds_paths) == 0L) stop("Nenhum RDS encontrado em ", rds_dir, " com padrão ", rds_pattern)

  years <- sort(as.integer(names(rds_paths)))

  # opcional: limpar outputs antigos
  if (isTRUE(overwrite_csv)) {
    for (yy in years) {
      f <- file.path(out_dir_csv, paste0(filename_prefix, "_estb_", yy, out_suffix))
      if (file.exists(f)) unlink(f)
    }
  }

  log("Lendo e empilhando anos: ", paste(years, collapse = ", "))

  partes <- vector("list", length(rds_paths))
  k <- 0L

  for (y in names(rds_paths)) {
    p <- rds_paths[[y]]
    if (is.null(p) || !nzchar(p) || !file.exists(p)) next

    log("Lendo ano ", y, ": ", p)
    dt_year <- readRDS(p)

    # reduz colunas para RAM (opcional)
    if (!is.null(keep_cols)) {
      keep <- intersect(keep_cols, names(dt_year))
      dt_year <- dt_year[, ..keep]
    }

    # garante coluna year
    if (!(year_col %in% names(dt_year))) {
      dt_year[, (year_col) := as.integer(y)]
    } else {
      dt_year[, (year_col) := as.integer(get(year_col))]
    }

    k <- k + 1L
    partes[[k]] <- dt_year
    rm(dt_year); gc()
  }

  if (k == 0L) stop("Nenhum RDS válido foi lido.")

  dt_all <- rbindlist(partes[seq_len(k)], use.names = TRUE, fill = TRUE)
  rm(partes); gc()

  log("dt_all montado. Linhas: ", nrow(dt_all), " | Colunas: ", ncol(dt_all))

  # roda tudo de uma vez
  log("Rodando main_multiyear()...")
  res <- main_multiyear(dt_all, diff_threshold = diff_threshold)

  dt_final <- as.data.table(res$dt_final)

  if (!(year_col %in% names(dt_final))) {
    stop("dt_final não tem a coluna '", year_col, "'. Garanta que main_multiyear preserve essa coluna.")
  }

  # escreve 1 CSV por ano
  yrs_out <- sort(unique(dt_final[[year_col]]))
  log("Salvando CSVs por ano: ", paste(yrs_out, collapse = ", "))

  for (yy in yrs_out) {
    out_file <- file.path(out_dir_csv, paste0(filename_prefix, "_estb_", yy, out_suffix))
    fwrite(dt_final[get(year_col) == yy], out_file)
    log("  - OK: ", out_file)
  }

  # limpeza
  rm(dt_all, res, dt_final); gc()

  invisible(list(
    years = yrs_out,
    out_dir_csv = out_dir_csv
  ))
}




