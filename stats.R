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

    dt_final <- fread(f)

    if (!(col_aceito %in% names(dt_final))) stop("Arquivo ", bn, " não tem a coluna '", col_aceito, "'.")
    if (!(col_estoque %in% names(dt_final))) stop("Arquivo ", bn, " não tem a coluna '", col_estoque, "'.")

    # 1) perc_aceito
    perc_aceito <- dt_final[
      ,
      .N,
      by = .(aceito_updated = get(col_aceito))
    ][
      ,
      `:=`(
        prop = N / sum(N),
        perc = 100 * N / sum(N)
      )
    ]
    perc_aceito[, `:=`(year = year, file = bn)]

    # 2) prop_aceito_estoque
    prop_aceito_estoque <- dt_final[
      ,
      .(estoque_total = sum(get(col_estoque), na.rm = TRUE)),
      by = .(aceito_updated = get(col_aceito))
    ][
      ,
      `:=`(
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

  dt_perc <- rbindlist(out_perc, use.names = TRUE, fill = TRUE)
  dt_estoque <- rbindlist(out_estoque, use.names = TRUE, fill = TRUE)

  setorder(dt_perc, year, aceito_updated)
  setorder(dt_estoque, year, aceito_updated)

  list(
    perc_aceito = dt_perc,
    prop_aceito_estoque = dt_estoque
  )
}

res_stats <- calcular_stats_aceito_por_csv(
  csv_dir = "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/csv_por_ano",
  verbose = TRUE
)

# Ver na tela
print(res_stats$perc_aceito)
print(res_stats$prop_aceito_estoque)

dt <- fread("D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/csv_por_ano/sp_estb_2023_limpo_corr.csv")
