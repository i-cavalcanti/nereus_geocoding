filtrar_ids_estoque_zero <- function(dt, id_col, estoque_col) {
  stopifnot(data.table::is.data.table(dt))
  require_columns(dt, c(id_col, estoque_col), "dt")

  ids_zero <- dt[, .(all_zero = all(is.na(get(estoque_col)) | get(estoque_col) == 0)), by = id_col][all_zero == TRUE, get(id_col)]
  dt[!get(id_col) %in% ids_zero]
}

imputar_endereco_geometry_por_score_fast <- function(
  dt,
  id_col,
  endereco_col,
  score_col,
  municipio_col = "municipio",
  matrizfilial_col = "matrizfilial",
  geometry_col = "geometry",
  validated_col = "aceito",
  endereco_updated_col = "endereco_updated",
  geometry_updated_col = "geometry_updated",
  score_updated_col = "score_precisao_updated",
  validated_updated_col = "aceito_updated",
  diff_threshold = 0.2,
  geo_threshold_m = 1000,
  copy_dt = TRUE,
  string_method = "lv",
  verbose = TRUE
) {
  stopifnot(data.table::is.data.table(dt))
  require_columns(dt, c(id_col, endereco_col, score_col, municipio_col, matrizfilial_col, geometry_col, validated_col), "dt")
  if (!inherits(dt[[geometry_col]], "sfc")) stop("Geometry column must be an sf/sfc object.", call. = FALSE)

  if (isTRUE(copy_dt)) dt <- data.table::copy(dt)

  dt[, imputada := 0L]
  dt[, diff := NA_real_]
  dt[, dist_m := NA_real_]
  dt[, (endereco_updated_col) := get(endereco_col)]
  dt[, (geometry_updated_col) := get(geometry_col)]
  dt[, (score_updated_col) := get(score_col)]
  dt[, (validated_updated_col) := get(validated_col)]
  if (is.numeric(dt[[validated_col]]) || is.integer(dt[[validated_col]])) dt[, (validated_updated_col) := as.integer(get(validated_col))]

  data.table::setorderv(dt, cols = c(id_col, score_col), order = c(1, 1), na.last = TRUE)
  ref <- dt[, .SD[1], by = id_col, .SDcols = c(endereco_col, score_col, geometry_col, validated_col, municipio_col, matrizfilial_col)]
  data.table::setnames(ref, c(endereco_col, score_col, geometry_col, validated_col, municipio_col, matrizfilial_col), c("ref_endereco", "ref_score", "ref_geom", "ref_validated", "ref_municipio", "ref_matrizfilial"))

  dt[ref, `:=`(
    ref_endereco = i.ref_endereco,
    ref_score = i.ref_score,
    ref_geom = i.ref_geom,
    ref_validated = i.ref_validated,
    ref_municipio = i.ref_municipio,
    ref_matrizfilial = i.ref_matrizfilial
  ), on = id_col]

  cand_idx <- dt[
    !is.na(ref_municipio) & !is.na(get(municipio_col)) & get(municipio_col) == ref_municipio &
      !is.na(ref_matrizfilial) & !is.na(get(matrizfilial_col)) & get(matrizfilial_col) == ref_matrizfilial &
      !is.na(ref_score) & !is.na(get(score_col)) & get(score_col) > ref_score,
    which = TRUE
  ]

  if (length(cand_idx) == 0L) {
    dt[, c("ref_endereco", "ref_score", "ref_geom", "ref_validated", "ref_municipio", "ref_matrizfilial") := NULL]
    return(dt[])
  }

  ref_addr <- dt$ref_endereco[cand_idx]
  x_addr <- dt[[endereco_col]][cand_idx]
  ok_str <- !is.na(ref_addr) & !is.na(x_addr)
  diff_vec <- rep(NA_real_, length(cand_idx))

  if (any(ok_str)) {
    d <- stringdist::stringdist(ref_addr[ok_str], x_addr[ok_str], method = string_method)
    diff_vec[ok_str] <- d / pmax(nchar(ref_addr[ok_str]), nchar(x_addr[ok_str]))
  }

  cond_string <- !is.na(diff_vec) & diff_vec <= diff_threshold
  need_geo_local <- which(!cond_string)
  dist_vec <- rep(NA_real_, length(cand_idx))

  if (length(need_geo_local) > 0L) {
    idx_geo <- cand_idx[need_geo_local]
    g1 <- dt[[geometry_col]][idx_geo]
    g2 <- dt$ref_geom[idx_geo]
    ok_geom <- !is.na(g1) & !is.na(g2)

    if (any(ok_geom)) {
      dist_vec[need_geo_local[ok_geom]] <- as.numeric(sf::st_distance(g1[ok_geom], g2[ok_geom], by_element = TRUE))
    }
  }

  cond_geo <- !is.na(dist_vec) & dist_vec <= geo_threshold_m
  imputar_string <- cond_string
  imputar_geo <- !cond_string & cond_geo
  imputar <- imputar_string | imputar_geo

  if (any(imputar)) {
    idx_upd <- cand_idx[imputar]
    dt[idx_upd, imputada := 1L]
    dt[idx_upd, (endereco_updated_col) := ref_endereco]
    dt[idx_upd, (geometry_updated_col) := ref_geom]
    dt[idx_upd, (score_updated_col) := ref_score]
    dt[idx_upd, (validated_updated_col) := ref_validated]

    if (any(imputar_string)) data.table::set(dt, cand_idx[imputar_string], "diff", diff_vec[imputar_string])
    if (any(imputar_geo)) data.table::set(dt, cand_idx[imputar_geo], "dist_m", dist_vec[imputar_geo])
  }

  dt[, c("ref_endereco", "ref_score", "ref_geom", "ref_validated", "ref_municipio", "ref_matrizfilial") := NULL]
  project_log(verbose, "Imputed ", dt[imputada == 1L, .N], " rows")
  dt[]
}

main_multiyear <- function(
  dt_first_stage,
  id_col = "identificad_m",
  endereco_col = "endereco_best",
  diff_threshold = 0.2,
  geo_threshold_m = 1000,
  verbose = TRUE
) {
  stopifnot(data.table::is.data.table(dt_first_stage))

  needed_cols <- unique(c(id_col, endereco_col, "geometry", "precisao_best", "estoque", "municipio", "matrizfilial", "aceito", "year", "municipio_7", "id"))
  require_columns(dt_first_stage, needed_cols, "dt_first_stage")

  dt <- dt_first_stage[, ..needed_cols]
  dt <- criar_score_precisao(dt, precisao_col = "precisao_best", score_col = "score_precisao")

  dt <- imputar_endereco_geometry_por_score_fast(
    dt = dt,
    id_col = id_col,
    endereco_col = endereco_col,
    score_col = "score_precisao",
    municipio_col = "municipio",
    matrizfilial_col = "matrizfilial",
    geometry_col = "geometry",
    validated_col = "aceito",
    diff_threshold = diff_threshold,
    geo_threshold_m = geo_threshold_m,
    copy_dt = FALSE,
    verbose = verbose
  )

  dt <- filtrar_ids_estoque_zero(dt, id_col = id_col, estoque_col = "estoque")
  require_columns(dt, "geometry_updated", "dt")
  if (!inherits(dt[["geometry_updated"]], "sfc")) stop("geometry_updated must be an sf/sfc object.", call. = FALSE)
  dt[, geometry := NULL]
  dt[]
}

main_second_stage_geocoding <- function(
  pathname_in_rds,
  pathname_in_prev_csv,
  out_dir_rds,
  filename_prefix,
  diff_threshold = 0.2,
  geo_threshold_m = 1000,
  year_col = "year",
  keep_cols = NULL,
  input_rds_pattern = "^dt_f_{prefix}_(\\d{4})\\.rds$",
  final_rds_template = "{prefix}_estb_{year}_limpo_corr.rds",
  prev_csv_template = "{prefix}_estb_{year}_limpo.csv",
  out_suffix = NULL,
  overwrite_rds = FALSE,
  verbose = TRUE,
  encoding = "Latin-1",
  geocode_keep = c("id", "endereco", "numlograd", "cep", "bairro", "municipio_7", "uf_dom", "estoque", "identificad_m", "municipio", "matrizfilial"),
  run_id = NULL,
  log_file = NULL
) {
  if (is.null(run_id)) run_id <- make_run_id("second_stage")

  if (!dir.exists(pathname_in_rds)) stop("Input RDS directory does not exist: ", pathname_in_rds, call. = FALSE)
  if (!dir.exists(pathname_in_prev_csv)) stop("Previous CSV directory does not exist: ", pathname_in_prev_csv, call. = FALSE)
  ensure_dir(out_dir_rds)

  input_pattern <- build_template(input_rds_pattern, prefix = filename_prefix)
  rds_paths <- index_files_by_year(pathname_in_rds, input_pattern, paste0("first-stage RDS for ", filename_prefix))
  years <- sort(as.integer(names(rds_paths)))

  make_final_rds_name <- function(year) {
    if (!is.null(final_rds_template)) {
      return(build_template(final_rds_template, prefix = filename_prefix, year = year))
    }
    paste0(filename_prefix, "_estb_", year, out_suffix)
  }

  params <- paste0(
    "diff_threshold=", diff_threshold,
    ";geo_threshold_m=", geo_threshold_m
  )

  if (isTRUE(overwrite_rds)) {
    for (yy in years) {
      path <- file.path(out_dir_rds, make_final_rds_name(yy))
      if (file.exists(path)) unlink(path)
    }
  }

  parts <- vector("list", length(years))
  n_parts <- 0L
  year_log <- list()

  for (yy in years) {
    started_at <- Sys.time()
    project_log(verbose, "Second stage input: ", filename_prefix, " ", yy)

    input_rds <- rds_paths[[as.character(yy)]]
    prev_csv <- file.path(pathname_in_prev_csv, build_template(prev_csv_template, prefix = filename_prefix, year = yy))
    output_file <- file.path(out_dir_rds, make_final_rds_name(yy))

    year_log[[as.character(yy)]] <- list(
      started_at = started_at,
      input_file = paste(input_rds, prev_csv, sep = ";"),
      output_file = output_file,
      input_rows = NA_integer_,
      prev_rows = NA_integer_,
      merged_rows = NA_integer_
    )

    if (!file.exists(prev_csv)) {
      msg <- paste0("Previous CSV not found: ", prev_csv)

      append_run_log(log_file, make_log_row(
        run_id = run_id,
        stage = "second_stage",
        region = filename_prefix,
        year = yy,
        status = "missing_input_file",
        message = msg,
        input_file = year_log[[as.character(yy)]]$input_file,
        output_file = output_file,
        started_at = started_at,
        ended_at = Sys.time(),
        params = params
      ))

      stop(msg, call. = FALSE)
    }

    dt_year <- data.table::as.data.table(readRDS(input_rds))
    require_columns(dt_year, "id", paste0("first-stage RDS ", yy))
    year_log[[as.character(yy)]]$input_rows <- nrow(dt_year)

    cols_available <- names(data.table::fread(prev_csv, nrows = 0, encoding = encoding, showProgress = FALSE))
    needed_min_raw <- c("municipio", "cduf", "cep", "endereco", "estoque")
    read_cols <- unique(c(needed_min_raw, "bairro", "numlograd", "identificad_m", "matrizfilial", "id"))
    dt_prev <- data.table::fread(prev_csv, encoding = encoding, select = intersect(read_cols, cols_available), showProgress = FALSE)
    year_log[[as.character(yy)]]$prev_rows <- nrow(dt_prev)

    require_columns(dt_prev, needed_min_raw, paste0("previous CSV ", yy))
    add_col_if_missing(dt_prev, "bairro", NA_character_)
    add_col_if_missing(dt_prev, "numlograd", NA)
    if (!"municipio_7" %in% names(dt_prev)) dt_prev[, municipio_7 := ibge6_to_7(municipio)]
    if (!"uf_dom" %in% names(dt_prev)) dt_prev <- add_sigla_from_uf(dt_prev, "cduf", "uf_dom")
    dt_prev <- change_cep_99999999_to_na(dt_prev, "cep", "municipio")
    require_columns(dt_prev, "id", paste0("previous CSV ", yy))

    keep_prev <- intersect(geocode_keep, names(dt_prev))
    dt_prev <- dt_prev[, ..keep_prev]

    dt_merged <- merge(dt_year, dt_prev, by = "id", all.x = TRUE, suffixes = c("", ".in"))
    dt_merged <- fill_from_suffix(dt_merged, suffix = ".in")
    year_log[[as.character(yy)]]$merged_rows <- nrow(dt_merged)

    if (!is.null(keep_cols)) dt_merged <- dt_merged[, intersect(keep_cols, names(dt_merged)), with = FALSE]

    if (!year_col %in% names(dt_merged)) {
      dt_merged[, (year_col) := yy]
    } else {
      dt_merged[, (year_col) := as.integer(get(year_col))]
      idx_na <- which(is.na(dt_merged[[year_col]]))
      if (length(idx_na) > 0L) data.table::set(dt_merged, i = idx_na, j = year_col, value = yy)
    }

    n_parts <- n_parts + 1L
    parts[[n_parts]] <- dt_merged
  }

  if (n_parts == 0L) stop("No valid first-stage RDS/CSV pairs found for ", filename_prefix, call. = FALSE)

  dt_all <- data.table::rbindlist(parts[seq_len(n_parts)], use.names = TRUE, fill = TRUE)

  dt_final <- tryCatch(
    main_multiyear(dt_all, diff_threshold = diff_threshold, geo_threshold_m = geo_threshold_m, verbose = verbose),
    error = function(e) {
      for (yy in years) {
        yy_key <- as.character(yy)
        append_run_log(log_file, make_log_row(
          run_id = run_id,
          stage = "second_stage",
          region = filename_prefix,
          year = yy,
          status = "error",
          message = conditionMessage(e),
          input_file = year_log[[yy_key]]$input_file,
          output_file = year_log[[yy_key]]$output_file,
          started_at = year_log[[yy_key]]$started_at,
          ended_at = Sys.time(),
          input_rows = year_log[[yy_key]]$input_rows,
          rows_after_filters = year_log[[yy_key]]$merged_rows,
          params = params
        ))
      }
      stop(e)
    }
  )

  require_columns(dt_final, year_col, "dt_final")

  years_out <- sort(unique(as.integer(dt_final[[year_col]])))
  out_paths <- stats::setNames(vector("list", length(years_out)), as.character(years_out))

  for (yy in years_out) {
    yy_key <- as.character(yy)
    out_file <- file.path(out_dir_rds, make_final_rds_name(yy))

    if (file.exists(out_file) && !isTRUE(overwrite_rds)) {
      msg <- paste0("Output already exists: ", out_file)

      append_run_log(log_file, make_log_row(
        run_id = run_id,
        stage = "second_stage",
        region = filename_prefix,
        year = yy,
        status = "output_exists",
        message = msg,
        input_file = year_log[[yy_key]]$input_file,
        output_file = out_file,
        started_at = year_log[[yy_key]]$started_at,
        ended_at = Sys.time(),
        input_rows = year_log[[yy_key]]$input_rows,
        rows_after_filters = year_log[[yy_key]]$merged_rows,
        params = params
      ))

      stop(msg, call. = FALSE)
    }

    dt_out <- dt_final[get(year_col) == yy]
    require_columns(dt_out, "geometry_updated", paste0("final output ", yy))
    if (!inherits(dt_out[["geometry_updated"]], "sfc")) stop("geometry_updated must be sfc for year ", yy, call. = FALSE)

    saveRDS(dt_out, out_file)
    out_paths[[as.character(yy)]] <- out_file

    summary <- summarise_second_stage_output(dt_out)

    append_run_log(log_file, make_log_row(
      run_id = run_id,
      stage = "second_stage",
      region = filename_prefix,
      year = yy,
      status = "success",
      input_file = year_log[[yy_key]]$input_file,
      output_file = out_file,
      started_at = year_log[[yy_key]]$started_at,
      ended_at = Sys.time(),
      input_rows = year_log[[yy_key]]$input_rows,
      rows_after_filters = year_log[[yy_key]]$merged_rows,
      output_rows = summary$output_rows,
      accepted_rows = summary$accepted_rows,
      rejected_rows = summary$rejected_rows,
      imputed_rows = summary$imputed_rows,
      geometry_nonmissing = summary$geometry_nonmissing,
      params = params
    ))
  }


  missing_output_years <- setdiff(years, years_out)
  for (yy in missing_output_years) {
    yy_key <- as.character(yy)
    append_run_log(log_file, make_log_row(
      run_id = run_id,
      stage = "second_stage",
      region = filename_prefix,
      year = yy,
      status = "no_output_rows",
      message = "No rows for this year after second-stage processing.",
      input_file = year_log[[yy_key]]$input_file,
      output_file = year_log[[yy_key]]$output_file,
      started_at = year_log[[yy_key]]$started_at,
      ended_at = Sys.time(),
      input_rows = year_log[[yy_key]]$input_rows,
      rows_after_filters = year_log[[yy_key]]$merged_rows,
      output_rows = 0L,
      params = params
    ))
  }

  invisible(list(
    run_id = run_id,
    region = filename_prefix,
    years_found = years,
    years_out = years_out,
    out_dir_rds = out_dir_rds,
    rds_paths = out_paths,
    prev_csv_template = prev_csv_template,
    final_rds_template = final_rds_template,
    log_file = log_file
  ))
}
