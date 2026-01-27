library(data.table)

calcular_stats_aceito_por_csv <- function(
  csv_dir,
  pattern = "\\.csv$",
  year_regex = "year=(\\d{4})",
  col_aceito = "aceito_updated",
  col_estoque = "estoque",
  verbose = TRUE
) {
  stopifnot(dir.exists(csv_dir))

  log <- function(...) if (isTRUE(verbose)) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))

  files <- list.files(csv_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0L) stop("Nenhum CSV encontrado em: ", csv_dir)

  out_perc <- list()
  out_estoque <- list()

  for (f in files) {
    bn <- basename(f)
    year <- suppressWarnings(as.integer(sub(paste0(".*", year_regex, ".*"), "\\1", bn)))
    if (is.na(year)) year <- NA_integer_

    log("Lendo: ", bn)

    dt_final <- data.table::fread(f)

    if (!(col_aceito %in% names(dt_final))) stop("Arquivo ", bn, " não tem a coluna '", col_aceito, "'.")
    if (!(col_estoque %in% names(dt_final))) stop("Arquivo ", bn, " não tem a coluna '", col_estoque, "'.")

    # 1) perc_aceito (proporção de linhas)
    perc_aceito <- dt_final[
      , .(N = .N),
      by = .(aceito_updated = get(col_aceito))
    ][
      , `:=`(
        prop = N / sum(N),
        perc = 100 * N / sum(N)
      )
    ]
    perc_aceito[, `:=`(year = year, file = bn)]

    # 2) prop_aceito_estoque (proporção de estoque)
    prop_aceito_estoque <- dt_final[
      , .(estoque_total = sum(get(col_estoque), na.rm = TRUE)),
      by = .(aceito_updated = get(col_aceito))
    ][
      , `:=`(
        prop_estoque = estoque_total / sum(estoque_total),
        perc_estoque = 100 * estoque_total / sum(estoque_total)
      )
    ]
    prop_aceito_estoque[, `:=`(year = year, file = bn)]

    out_perc[[length(out_perc) + 1L]] <- perc_aceito
    out_estoque[[length(out_estoque) + 1L]] <- prop_aceito_estoque

    rm(dt_final, perc_aceito, prop_aceito_estoque)
    gc()
  }

  dt_perc <- data.table::rbindlist(out_perc, use.names = TRUE, fill = TRUE)
  dt_estoque <- data.table::rbindlist(out_estoque, use.names = TRUE, fill = TRUE)

  data.table::setorder(dt_perc, year, aceito_updated)
  data.table::setorder(dt_estoque, year, aceito_updated)

  # -------- MÉDIAS GERAIS ENTRE ANOS (cada ano tem o mesmo peso) --------
  media_perc_anos <- dt_perc[
    !is.na(year),
    .(
      media_prop_anos = mean(prop, na.rm = TRUE),
      media_perc_anos = 100 * mean(prop, na.rm = TRUE)
    ),
    by = aceito_updated
  ][order(aceito_updated)]

  media_estoque_anos <- dt_estoque[
    !is.na(year),
    .(
      media_prop_estoque_anos = mean(prop_estoque, na.rm = TRUE),
      media_perc_estoque_anos = 100 * mean(prop_estoque, na.rm = TRUE)
    ),
    by = aceito_updated
  ][order(aceito_updated)]

  if (isTRUE(verbose)) {
    log("Média geral (anos) - proporção de linhas por aceito_updated:")
    print(media_perc_anos)
    log("Média geral (anos) - proporção de estoque por aceito_updated:")
    print(media_estoque_anos)
  }

  list(
    perc_aceito = dt_perc,
    prop_aceito_estoque = dt_estoque,
    media_anos_perc_aceito = media_perc_anos,
    media_anos_prop_estoque = media_estoque_anos
  )
}

res_stats <- calcular_stats_aceito_por_csv(
  csv_dir = "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/csv_por_ano",
  pattern = "\\.csv$",
  year_regex = "sp_estb_(\\d{4})_",
  col_aceito = "aceito_updated",
  col_estoque = "estoque",
  verbose = TRUE
)


# Ver na tela
print(res_stats$perc_aceito)
print(res_stats$prop_aceito_estoque)

# proporções por ano (linhas)
res_stats$perc_aceito

# proporções por ano (estoque)
res_stats$prop_aceito_estoque

# média geral entre anos (linhas) por valor de aceito_updated
res_stats$media_anos_perc_aceito

# média geral entre anos (estoque) por valor de aceito_updated
res_stats$media_anos_prop_estoque







dt <- fread("D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/geocoded_rds/stats_geocoding.csv")


dt_head <- fread(
  "D:/Arq-Azzoni/RAIS/rais-geocoding/data/raw/rais_temp/sp_estb_2002_limpo.csv",
  nrows = 1000,
  encoding = "Latin-1"
)






library(data.table)

calcular_stats_aceito_por_rds <- function(
  rds_dir,
  pattern = "^dt_f_(\\d{4})\\.rds$",
  year_regex = "(\\d{4})",
  col_aceito = "aceito",
  col_estoque = "estoque",
  verbose = TRUE
) {
  stopifnot(dir.exists(rds_dir))

  log <- function(...) if (isTRUE(verbose)) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))

  files <- list.files(rds_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0L) stop("Nenhum RDS encontrado em: ", rds_dir)

  ok <- grepl(pattern, basename(files))
  files <- files[ok]
  if (length(files) == 0L) stop("Nenhum RDS casou com pattern: ", pattern)

  out_perc <- list()
  out_estoque <- list()

  for (f in files) {
    bn <- basename(f)
    year <- suppressWarnings(as.integer(sub(paste0(".*", year_regex, ".*"), "\\1", bn)))
    if (is.na(year)) year <- NA_integer_

    log("Lendo: ", bn)

    dt_final <- readRDS(f)
    data.table::setDT(dt_final)

    if (!(col_aceito %in% names(dt_final))) stop("Arquivo ", bn, " não tem a coluna '", col_aceito, "'.")
    if (!(col_estoque %in% names(dt_final))) stop("Arquivo ", bn, " não tem a coluna '", col_estoque, "'.")

    # 1) perc_aceito
    perc_aceito <- dt_final[
      , .(N = .N),
      by = .(aceito = get(col_aceito))
    ][
      , `:=`(
        prop = N / sum(N),
        perc = 100 * N / sum(N)
      )
    ]
    perc_aceito[, `:=`(year = year, file = bn)]

    # 2) prop_aceito_estoque
    prop_aceito_estoque <- dt_final[
      , .(estoque_total = sum(get(col_estoque), na.rm = TRUE)),
      by = .(aceito = get(col_aceito))
    ][
      , `:=`(
        prop_estoque = estoque_total / sum(estoque_total),
        perc_estoque = 100 * estoque_total / sum(estoque_total)
      )
    ]
    prop_aceito_estoque[, `:=`(year = year, file = bn)]

    out_perc[[length(out_perc) + 1L]] <- perc_aceito
    out_estoque[[length(out_estoque) + 1L]] <- prop_aceito_estoque

    rm(dt_final, perc_aceito, prop_aceito_estoque)
    gc()
  }

  dt_perc <- data.table::rbindlist(out_perc, use.names = TRUE, fill = TRUE)
  dt_estoque <- data.table::rbindlist(out_estoque, use.names = TRUE, fill = TRUE)

  data.table::setorder(dt_perc, year, aceito)
  data.table::setorder(dt_estoque, year, aceito)

  # --- MÉDIAS (média simples entre anos, por aceito) ---
  # (cada ano tem o mesmo peso; se um aceito não existir em um ano, não entra na média)
  media_perc <- dt_perc[
    , .(
      media_prop_anos = mean(prop, na.rm = TRUE),
      media_perc_anos = 100 * mean(prop, na.rm = TRUE)
    ),
    by = aceito
  ][order(aceito)]

  media_estoque <- dt_estoque[
    , .(
      media_prop_estoque_anos = mean(prop_estoque, na.rm = TRUE),
      media_perc_estoque_anos = 100 * mean(prop_estoque, na.rm = TRUE)
    ),
    by = aceito
  ][order(aceito)]

  # imprimir no console
  if (isTRUE(verbose)) {
    log("Média geral (anos) - proporção de linhas por aceito:")
    print(media_perc)
    log("Média geral (anos) - proporção de estoque por aceito:")
    print(media_estoque)
  }

  list(
    perc_aceito = dt_perc,
    prop_aceito_estoque = dt_estoque,
    media_anos_perc_aceito = media_perc,
    media_anos_prop_estoque = media_estoque
  )
}

res_stats <- calcular_stats_aceito_por_rds(
  rds_dir = "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/geocoded_rds",
  col_aceito = "aceito",
  col_estoque = "estoque",
  verbose = TRUE
)

print(res_stats$perc_aceito)
print(res_stats$prop_aceito_estoque)
