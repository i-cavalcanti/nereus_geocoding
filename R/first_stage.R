# -----------------------------------------------------------------------------
# First-stage RAIS establishment geocoding
# -----------------------------------------------------------------------------
# This file runs the first stage of the RAIS geocoding workflow. For each
# region-year CSV, it prepares and standardizes establishment address fields,
# evaluates multiple address-format configurations, and keeps the most precise
# geocoding result for each observation. It then applies CEP-level quality-control
# rules, saves a first-stage RDS output, and writes an append-only run log for
# auditability.

# -----------------------------------------------------------------------------
# First-stage configuration helpers
# -----------------------------------------------------------------------------

first_stage_required_raw_cols <- function() {
  c("municipio", "cduf", "cep", "endereco", "estoque")
}

first_stage_geocode_keep_cols <- function() {
  c(
    "id", "endereco", "numlograd", "cep", "bairro", "municipio_7", "uf_dom",
    "estoque", "identificad_m", "municipio", "matrizfilial"
  )
}

make_first_stage_params <- function(sd_threshold_km, accept_single_point_cep, cep_distance_crs) {
  paste0(
    "sd_threshold_km=", sd_threshold_km,
    ";accept_single_point_cep=", accept_single_point_cep,
    ";cep_distance_crs=", cep_distance_crs
  )
}

resolve_first_stage_input_dir <- function(input_csv_dir = NULL, pathname_in = NULL) {
  if (!is.null(input_csv_dir)) return(input_csv_dir)

  if (is.null(pathname_in)) {
    stop(
      "Provide input_csv_dir, or provide pathname_in so input_csv_dir can be inferred.",
      call. = FALSE
    )
  }

  file.path(pathname_in, "raw", "rais_temp")
}

validate_first_stage_paths <- function(input_csv_dir, pathname_out) {
  if (!dir.exists(input_csv_dir)) {
    stop("Input CSV directory does not exist: ", input_csv_dir, call. = FALSE)
  }
  ensure_dir(pathname_out)
  invisible(TRUE)
}

make_first_stage_paths <- function(
  input_csv_dir,
  pathname_out,
  filename_prefix,
  year,
  input_csv_template,
  output_rds_template
) {
  input_name <- build_template(input_csv_template, prefix = filename_prefix, year = year)
  output_name <- build_template(output_rds_template, prefix = filename_prefix, year = year)

  list(
    input_file = file.path(input_csv_dir, input_name),
    output_file = file.path(pathname_out, output_name)
  )
}

make_first_stage_read_cols <- function(select_cols = NULL) {
  if (is.null(select_cols)) return(NULL)

  unique(c(
    select_cols,
    first_stage_required_raw_cols(),
    "id", "bairro", "numlograd", "identificad_m", "matrizfilial", "clascnae20"
  ))
}

# -----------------------------------------------------------------------------
# Logging helpers
# -----------------------------------------------------------------------------

append_first_stage_log <- function(
  log_file,
  run_id,
  filename_prefix,
  year,
  status,
  input_file,
  output_file,
  started_at,
  params,
  message = NA_character_,
  input_rows = NA_integer_,
  rows_after_filters = NA_integer_,
  summary = NULL
) {
  if (is.null(summary)) {
    summary <- list(
      output_rows = NA_integer_,
      accepted_rows = NA_integer_,
      rejected_rows = NA_integer_,
      class0_rows = NA_integer_,
      class1_rows = NA_integer_,
      class2_rows = NA_integer_,
      cep_rows = NA_integer_,
      cep_accepted = NA_integer_,
      cep_rejected = NA_integer_,
      cep_single_point_accepted = NA_integer_,
      cep_single_point_rejected = NA_integer_,
      geometry_nonmissing = NA_integer_
    )
  }

  append_run_log(log_file, make_log_row(
    run_id = run_id,
    stage = "first_stage",
    region = filename_prefix,
    year = year,
    status = status,
    message = message,
    input_file = input_file,
    output_file = output_file,
    started_at = started_at,
    ended_at = Sys.time(),
    input_rows = input_rows,
    rows_after_filters = rows_after_filters,
    output_rows = summary$output_rows,
    accepted_rows = summary$accepted_rows,
    rejected_rows = summary$rejected_rows,
    class0_rows = summary$class0_rows,
    class1_rows = summary$class1_rows,
    class2_rows = summary$class2_rows,
    cep_rows = summary$cep_rows,
    cep_accepted = summary$cep_accepted,
    cep_rejected = summary$cep_rejected,
    cep_single_point_accepted = summary$cep_single_point_accepted,
    cep_single_point_rejected = summary$cep_single_point_rejected,
    geometry_nonmissing = summary$geometry_nonmissing,
    params = params
  ))
}

log_missing_first_stage_input <- function(
  log_file,
  run_id,
  filename_prefix,
  year,
  input_file,
  output_file,
  started_at,
  params,
  verbose = TRUE
) {
  project_log(verbose, "Missing input file: ", input_file)

  append_first_stage_log(
    log_file = log_file,
    run_id = run_id,
    filename_prefix = filename_prefix,
    year = year,
    status = "missing_input_file",
    message = "Input CSV was not found.",
    input_file = input_file,
    output_file = output_file,
    started_at = started_at,
    params = params
  )

  invisible(TRUE)
}

handle_existing_first_stage_output <- function(
  output_file,
  log_file,
  run_id,
  filename_prefix,
  year,
  input_file,
  started_at,
  params,
  continue_on_error
) {
  msg <- paste0("Output already exists. Set overwrite = TRUE to replace: ", output_file)

  append_first_stage_log(
    log_file = log_file,
    run_id = run_id,
    filename_prefix = filename_prefix,
    year = year,
    status = "output_exists",
    message = msg,
    input_file = input_file,
    output_file = output_file,
    started_at = started_at,
    params = params
  )

  if (isTRUE(continue_on_error)) return(invisible(FALSE))
  stop(msg, call. = FALSE)
}

# -----------------------------------------------------------------------------
# Data preparation helpers
# -----------------------------------------------------------------------------

read_first_stage_csv <- function(file_in, year, encoding, select_cols = NULL) {
  dt <- data.table::fread(
    file_in,
    encoding = encoding,
    select = make_first_stage_read_cols(select_cols),
    showProgress = FALSE
  )

  require_columns(dt, first_stage_required_raw_cols(), paste0("input ", year))
  dt
}

standardize_first_stage_input <- function(dt) {
  add_col_if_missing(dt, "bairro", NA_character_)
  add_col_if_missing(dt, "numlograd", NA)

  dt[, municipio_7 := ibge6_to_7(municipio)]
  dt <- add_sigla_from_uf(dt, "cduf", "uf_dom")
  change_cep_99999999_to_na(dt, "cep", "municipio")
}

apply_first_stage_filters <- function(
  dt,
  year,
  filter_municipio_7 = NULL,
  filter_cnae = NULL
) {
  if (!is.null(filter_municipio_7)) {
    dt <- dt[municipio_7 %in% unlist(filter_municipio_7)]
  }

  if (!is.null(filter_cnae)) {
    require_columns(dt, "clascnae20", paste0("input ", year))
    dt <- dt[clascnae20 %in% unlist(filter_cnae)]
  }

  dt[]
}

select_first_stage_geocode_input <- function(dt, year) {
  keep_cols <- first_stage_geocode_keep_cols()
  require_columns(dt, keep_cols, paste0("geocoding input ", year))
  dt[, ..keep_cols]
}

make_first_stage_address_fields <- function() {
  campos <- geocodebr::definir_campos(
    logradouro = "endereco",
    numero = "numlograd",
    cep = "cep",
    localidade = "bairro",
    municipio = "municipio_7",
    estado = "uf_dom"
  )

  campos_pdr <- enderecobr::correspondencia_campos(
    logradouro = "endereco",
    numero = "numlograd",
    cep = "cep",
    bairro = "bairro",
    municipio = "municipio_7",
    estado = "uf_dom"
  )

  list(campos = campos, campos_pdr = campos_pdr)
}

prepare_first_stage_geocode_input <- function(
  file_in,
  year,
  encoding,
  select_cols = NULL,
  filter_municipio_7 = NULL,
  filter_cnae = NULL
) {
  dt <- read_first_stage_csv(
    file_in = file_in,
    year = year,
    encoding = encoding,
    select_cols = select_cols
  )
  input_rows <- nrow(dt)

  dt <- standardize_first_stage_input(dt)
  dt <- apply_first_stage_filters(
    dt = dt,
    year = year,
    filter_municipio_7 = filter_municipio_7,
    filter_cnae = filter_cnae
  )
  rows_after_filters <- nrow(dt)

  dt <- select_first_stage_geocode_input(dt, year)

  list(
    data = dt,
    input_rows = input_rows,
    rows_after_filters = rows_after_filters
  )
}

# -----------------------------------------------------------------------------
# Geocoding helpers
# -----------------------------------------------------------------------------

run_first_stage_geocoding <- function(
  dt,
  sd_threshold_km,
  accept_single_point_cep,
  cep_distance_crs,
  verbose = TRUE
) {
  address_fields <- make_first_stage_address_fields()

  main_geocodificacao(
    dt = dt,
    campos = address_fields$campos,
    campos_pdr = address_fields$campos_pdr,
    sd_threshold_km = sd_threshold_km,
    accept_single_point_cep = accept_single_point_cep,
    cep_distance_crs = cep_distance_crs,
    verbose = verbose,
    return_diagnostics = TRUE
  )
}

process_first_stage_year <- function(
  file_in,
  rds_file,
  year,
  encoding,
  select_cols,
  filter_municipio_7,
  filter_cnae,
  sd_threshold_km,
  accept_single_point_cep,
  cep_distance_crs,
  verbose = TRUE
) {
  prepared <- prepare_first_stage_geocode_input(
    file_in = file_in,
    year = year,
    encoding = encoding,
    select_cols = select_cols,
    filter_municipio_7 = filter_municipio_7,
    filter_cnae = filter_cnae
  )

  geocoding_result <- run_first_stage_geocoding(
    dt = prepared$data,
    sd_threshold_km = sd_threshold_km,
    accept_single_point_cep = accept_single_point_cep,
    cep_distance_crs = cep_distance_crs,
    verbose = verbose
  )

  dt_final <- geocoding_result$dt_final
  cep_diagnostics <- geocoding_result$cep_diagnostics
  saveRDS(dt_final, rds_file)

  list(
    path = rds_file,
    input_rows = prepared$input_rows,
    rows_after_filters = prepared$rows_after_filters,
    summary = summarise_first_stage_output(dt_final, cep_diagnostics)
  )
}

run_first_stage_year_with_logging <- function(
  year,
  input_csv_dir,
  pathname_out,
  filename_prefix,
  input_csv_template,
  output_rds_template,
  encoding,
  sd_threshold_km,
  accept_single_point_cep,
  cep_distance_crs,
  filter_municipio_7,
  filter_cnae,
  select_cols,
  overwrite,
  continue_on_error,
  verbose,
  run_id,
  log_file,
  params
) {
  started_at <- Sys.time()
  project_log(verbose, "First stage: ", filename_prefix, " ", year)

  paths <- make_first_stage_paths(
    input_csv_dir = input_csv_dir,
    pathname_out = pathname_out,
    filename_prefix = filename_prefix,
    year = year,
    input_csv_template = input_csv_template,
    output_rds_template = output_rds_template
  )

  if (!file.exists(paths$input_file)) {
    log_missing_first_stage_input(
      log_file = log_file,
      run_id = run_id,
      filename_prefix = filename_prefix,
      year = year,
      input_file = paths$input_file,
      output_file = paths$output_file,
      started_at = started_at,
      params = params,
      verbose = verbose
    )

    return(list(path = NULL, skipped = "missing_input_file"))
  }

  if (file.exists(paths$output_file) && !isTRUE(overwrite)) {
    handle_existing_first_stage_output(
      output_file = paths$output_file,
      log_file = log_file,
      run_id = run_id,
      filename_prefix = filename_prefix,
      year = year,
      input_file = paths$input_file,
      started_at = started_at,
      params = params,
      continue_on_error = continue_on_error
    )

    return(list(path = NULL, skipped = "output_exists"))
  }

  run_one_year <- function() {
    process_first_stage_year(
      file_in = paths$input_file,
      rds_file = paths$output_file,
      year = year,
      encoding = encoding,
      select_cols = select_cols,
      filter_municipio_7 = filter_municipio_7,
      filter_cnae = filter_cnae,
      sd_threshold_km = sd_threshold_km,
      accept_single_point_cep = accept_single_point_cep,
      cep_distance_crs = cep_distance_crs,
      verbose = verbose
    )
  }

  if (isTRUE(continue_on_error)) {
    out <- tryCatch(run_one_year(), error = function(e) e)

    if (inherits(out, "error")) {
      msg <- conditionMessage(out)
      project_log(verbose, "Error in ", filename_prefix, " ", year, ": ", msg)

      append_first_stage_log(
        log_file = log_file,
        run_id = run_id,
        filename_prefix = filename_prefix,
        year = year,
        status = "error",
        message = msg,
        input_file = paths$input_file,
        output_file = paths$output_file,
        started_at = started_at,
        params = params
      )

      return(list(path = NULL, skipped = msg))
    }
  } else {
    out <- tryCatch(run_one_year(), error = function(e) {
      append_first_stage_log(
        log_file = log_file,
        run_id = run_id,
        filename_prefix = filename_prefix,
        year = year,
        status = "error",
        message = conditionMessage(e),
        input_file = paths$input_file,
        output_file = paths$output_file,
        started_at = started_at,
        params = params
      )
      stop(e)
    })
  }

  append_first_stage_log(
    log_file = log_file,
    run_id = run_id,
    filename_prefix = filename_prefix,
    year = year,
    status = "success",
    input_file = paths$input_file,
    output_file = out$path,
    started_at = started_at,
    input_rows = out$input_rows,
    rows_after_filters = out$rows_after_filters,
    summary = out$summary,
    params = params
  )

  list(path = out$path, skipped = NULL)
}

# -----------------------------------------------------------------------------
# Main first-stage workflow
# -----------------------------------------------------------------------------

main_first_stage_geocoding <- function(
  input_csv_dir = NULL,
  pathname_out,
  years,
  filename_prefix,
  input_csv_template = "{prefix}_estb_{year}_limpo.csv",
  output_rds_template = "dt_f_{prefix}_{year}.rds",
  pathname_in = NULL,
  encoding = "Latin-1",
  sd_threshold_km = 0.3,
  accept_single_point_cep = TRUE,
  cep_distance_crs = 5880,
  filter_municipio_7 = NULL,
  filter_cnae = NULL,
  select_cols = NULL,
  overwrite = FALSE,
  continue_on_error = FALSE,
  verbose = TRUE,
  run_id = NULL,
  log_file = NULL
) {
  if (is.null(run_id)) run_id <- make_run_id("first_stage")

  input_csv_dir <- resolve_first_stage_input_dir(
    input_csv_dir = input_csv_dir,
    pathname_in = pathname_in
  )
  validate_first_stage_paths(input_csv_dir, pathname_out)

  years <- as.integer(years)
  rds_paths <- stats::setNames(vector("list", length(years)), as.character(years))
  skipped <- list()

  params <- make_first_stage_params(
    sd_threshold_km = sd_threshold_km,
    accept_single_point_cep = accept_single_point_cep,
    cep_distance_crs = cep_distance_crs
  )

  for (year in years) {
    result <- run_first_stage_year_with_logging(
      year = year,
      input_csv_dir = input_csv_dir,
      pathname_out = pathname_out,
      filename_prefix = filename_prefix,
      input_csv_template = input_csv_template,
      output_rds_template = output_rds_template,
      encoding = encoding,
      sd_threshold_km = sd_threshold_km,
      accept_single_point_cep = accept_single_point_cep,
      cep_distance_crs = cep_distance_crs,
      filter_municipio_7 = filter_municipio_7,
      filter_cnae = filter_cnae,
      select_cols = select_cols,
      overwrite = overwrite,
      continue_on_error = continue_on_error,
      verbose = verbose,
      run_id = run_id,
      log_file = log_file,
      params = params
    )

    year_key <- as.character(year)
    if (!is.null(result$path)) rds_paths[[year_key]] <- result$path
    if (!is.null(result$skipped)) skipped[[year_key]] <- result$skipped
  }

  invisible(list(
    run_id = run_id,
    region = filename_prefix,
    years = years,
    rds_paths = rds_paths,
    skipped = skipped,
    out_dir = pathname_out,
    input_csv_dir = input_csv_dir,
    input_csv_template = input_csv_template,
    output_rds_template = output_rds_template,
    log_file = log_file
  ))
}
