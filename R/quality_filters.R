filtrar_ceps_consistentes <- function(
  dt_ceps,
  sd_threshold_km,
  cep_col = "cep_padr",
  accept_single_point_cep = TRUE,
  distance_crs = 5880
) {
  stopifnot(data.table::is.data.table(dt_ceps))
  require_columns(dt_ceps, cep_col, "dt_ceps")
  stopifnot(is.numeric(sd_threshold_km), length(sd_threshold_km) == 1L)
  stopifnot(is.logical(accept_single_point_cep), length(accept_single_point_cep) == 1L, !is.na(accept_single_point_cep))

  ceps <- dt_ceps[!is.na(get(cep_col)), unique(get(cep_col))]
  if (length(ceps) == 0L) {
    return(list(ceps_aceitos = character(0), diagnostico = data.table::data.table()))
  }

  df_ceps <- geocodebr::busca_por_cep(cep = ceps, resultado_sf = TRUE, verboso = FALSE)
  if (nrow(df_ceps) == 0L) {
    return(list(ceps_aceitos = character(0), diagnostico = data.table::data.table()))
  }

  df_calc <- if (is.null(distance_crs)) df_ceps else sf::st_transform(df_ceps, distance_crs)
  coords <- sf::st_coordinates(df_calc)

  df_dt <- data.table::as.data.table(df_ceps)
  df_dt[, `:=`(x = coords[, 1], y = coords[, 2])]

  dispersion <- df_dt[, {
    if (.N < 2L) {
      list(mean_dist_m = NA_real_, sd_dist_m = NA_real_, r95_m = NA_real_, rmax_m = NA_real_)
    } else {
      cx <- mean(x, na.rm = TRUE)
      cy <- mean(y, na.rm = TRUE)
      d <- sqrt((x - cx)^2 + (y - cy)^2)
      list(
        mean_dist_m = mean(d, na.rm = TRUE),
        sd_dist_m = stats::sd(d, na.rm = TRUE),
        r95_m = as.numeric(stats::quantile(d, 0.95, na.rm = TRUE, names = FALSE)),
        rmax_m = max(d, na.rm = TRUE)
      )
    }
  }, by = cep]

  dispersion[, `:=`(
    mean_dist_km = mean_dist_m / 1000,
    sd_dist_km = sd_dist_m / 1000,
    r95_km = r95_m / 1000,
    rmax_km = rmax_m / 1000
  )]

  counts <- df_dt[, .(n_points = .N, single = .N == 1L), by = cep]
  diagnostico <- merge(dispersion, counts, by = "cep", all.x = TRUE)

  diagnostico[, criterio_aceite := data.table::fcase(
    single == TRUE & accept_single_point_cep == TRUE, "single_point_accepted",
    single == TRUE & accept_single_point_cep == FALSE, "single_point_rejected",
    single == FALSE & !is.na(sd_dist_km) & sd_dist_km <= sd_threshold_km, "sd_within_threshold",
    single == FALSE & !is.na(sd_dist_km) & sd_dist_km > sd_threshold_km, "sd_above_threshold",
    default = "not_evaluated"
  )]

  diagnostico[, aceito := as.integer(criterio_aceite %in% c("single_point_accepted", "sd_within_threshold"))]
  data.table::setorder(diagnostico, cep)

  list(
    ceps_aceitos = diagnostico[aceito == 1L, cep],
    diagnostico = diagnostico
  )
}

filtrar_enderecos_aceitos <- function(dt, ceps_aceitos, classe_col, cep_col = "cep_padr", aceito_col = "aceito") {
  stopifnot(data.table::is.data.table(dt))
  require_columns(dt, c(cep_col, classe_col), "dt")

  ceps_ok <- data.table::data.table(cep_tmp = unique(ceps_aceitos))
  data.table::setnames(ceps_ok, "cep_tmp", cep_col)
  dt[, cep_ok := 0L]
  if (nrow(ceps_ok) > 0L) dt[ceps_ok, cep_ok := 1L, on = cep_col]

  dt[, (aceito_col) := data.table::fcase(
    get(classe_col) == 0L, 1L,
    get(classe_col) == 1L & cep_ok == 1L, 1L,
    default = 0L
  )]

  dt[, cep_ok := NULL]
  dt[]
}

main_geocodificacao <- function(
  dt,
  campos,
  campos_pdr,
  sd_threshold_km = 0.3,
  accept_single_point_cep = TRUE,
  cep_distance_crs = 5880,
  verbose = TRUE,
  return_diagnostics = FALSE
) {
  project_log(verbose, "Running first-stage geocoding for ", nrow(dt), " rows")

  dt_geocoded <- geocode_pipeline(dt, campos, campos_pdr)
  data.table::setDT(dt_geocoded)
  dt_geocoded <- classificar_precisao_best(dt_geocoded, precisao_best_col = "precisao_best", classe_col = "classe")

  cep_results <- filtrar_ceps_consistentes(
    dt_ceps = dt_geocoded[classe == 1L],
    sd_threshold_km = sd_threshold_km,
    cep_col = "cep_padr",
    accept_single_point_cep = accept_single_point_cep,
    distance_crs = cep_distance_crs
  )

  project_log(verbose, "Accepted ", length(cep_results$ceps_aceitos), " CEPs after consistency filter")

  dt_final <- filtrar_enderecos_aceitos(
    dt = dt_geocoded,
    ceps_aceitos = cep_results$ceps_aceitos,
    cep_col = "cep_padr",
    classe_col = "classe",
    aceito_col = "aceito"
  )

  if (isTRUE(return_diagnostics)) {
    return(list(
      dt_final = dt_final,
      cep_diagnostics = cep_results$diagnostico
    ))
  }

  dt_final
}
