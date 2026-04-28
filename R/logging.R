make_run_id <- function(prefix = "run") {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
}

as_log_value <- function(x) {
  if (length(x) == 0L || is.null(x)) return(NA_character_)
  if (length(x) > 1L) return(paste(as.character(x), collapse = ";"))
  if (is.na(x)) return(NA_character_)
  as.character(x)
}

count_equal <- function(dt, col, value) {
  if (!data.table::is.data.table(dt) || !(col %in% names(dt))) return(NA_integer_)
  as.integer(dt[!is.na(get(col)) & get(col) == value, .N])
}

count_nonmissing <- function(dt, col) {
  if (!data.table::is.data.table(dt) || !(col %in% names(dt))) return(NA_integer_)
  as.integer(dt[!is.na(get(col)), .N])
}

append_run_log <- function(log_file, row) {
  if (is.null(log_file) || !nzchar(log_file)) return(invisible(NULL))
  ensure_dir(dirname(log_file))

  row_dt <- data.table::as.data.table(row)
  row_dt[, timestamp_written := format(Sys.time(), "%Y-%m-%d %H:%M:%S")]

  data.table::fwrite(
    row_dt,
    file = log_file,
    append = file.exists(log_file),
    col.names = !file.exists(log_file),
    na = ""
  )

  invisible(log_file)
}

make_log_row <- function(
  run_id,
  stage,
  region,
  year = NA_integer_,
  status,
  message = NA_character_,
  input_file = NA_character_,
  output_file = NA_character_,
  started_at = NA,
  ended_at = Sys.time(),
  input_rows = NA_integer_,
  rows_after_filters = NA_integer_,
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
  imputed_rows = NA_integer_,
  geometry_nonmissing = NA_integer_,
  params = NA_character_
) {
  if (inherits(started_at, "POSIXt") && inherits(ended_at, "POSIXt")) {
    runtime_seconds <- as.numeric(difftime(ended_at, started_at, units = "secs"))
  } else {
    runtime_seconds <- NA_real_
  }

  data.table::data.table(
    run_id = as_log_value(run_id),
    stage = as_log_value(stage),
    region = as_log_value(region),
    year = suppressWarnings(as.integer(year)),
    status = as_log_value(status),
    message = as_log_value(message),
    input_file = as_log_value(input_file),
    output_file = as_log_value(output_file),
    started_at = if (inherits(started_at, "POSIXt")) format(started_at, "%Y-%m-%d %H:%M:%S") else NA_character_,
    ended_at = if (inherits(ended_at, "POSIXt")) format(ended_at, "%Y-%m-%d %H:%M:%S") else NA_character_,
    runtime_seconds = runtime_seconds,
    input_rows = suppressWarnings(as.integer(input_rows)),
    rows_after_filters = suppressWarnings(as.integer(rows_after_filters)),
    output_rows = suppressWarnings(as.integer(output_rows)),
    accepted_rows = suppressWarnings(as.integer(accepted_rows)),
    rejected_rows = suppressWarnings(as.integer(rejected_rows)),
    class0_rows = suppressWarnings(as.integer(class0_rows)),
    class1_rows = suppressWarnings(as.integer(class1_rows)),
    class2_rows = suppressWarnings(as.integer(class2_rows)),
    cep_rows = suppressWarnings(as.integer(cep_rows)),
    cep_accepted = suppressWarnings(as.integer(cep_accepted)),
    cep_rejected = suppressWarnings(as.integer(cep_rejected)),
    cep_single_point_accepted = suppressWarnings(as.integer(cep_single_point_accepted)),
    cep_single_point_rejected = suppressWarnings(as.integer(cep_single_point_rejected)),
    imputed_rows = suppressWarnings(as.integer(imputed_rows)),
    geometry_nonmissing = suppressWarnings(as.integer(geometry_nonmissing)),
    params = as_log_value(params)
  )
}

summarise_first_stage_output <- function(dt, cep_diagnostics = NULL) {
  if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)

  if (!is.null(cep_diagnostics) && nrow(cep_diagnostics) > 0L) {
    if (!data.table::is.data.table(cep_diagnostics)) cep_diagnostics <- data.table::as.data.table(cep_diagnostics)
    cep_rows <- nrow(cep_diagnostics)
    cep_accepted <- count_equal(cep_diagnostics, "aceito", 1L)
    cep_rejected <- count_equal(cep_diagnostics, "aceito", 0L)
    cep_single_point_accepted <- count_equal(cep_diagnostics, "criterio_aceite", "single_point_accepted")
    cep_single_point_rejected <- count_equal(cep_diagnostics, "criterio_aceite", "single_point_rejected")
  } else {
    cep_rows <- cep_accepted <- cep_rejected <- cep_single_point_accepted <- cep_single_point_rejected <- NA_integer_
  }

  list(
    output_rows = nrow(dt),
    accepted_rows = count_equal(dt, "aceito", 1L),
    rejected_rows = count_equal(dt, "aceito", 0L),
    class0_rows = count_equal(dt, "classe", 0L),
    class1_rows = count_equal(dt, "classe", 1L),
    class2_rows = count_equal(dt, "classe", 2L),
    cep_rows = cep_rows,
    cep_accepted = cep_accepted,
    cep_rejected = cep_rejected,
    cep_single_point_accepted = cep_single_point_accepted,
    cep_single_point_rejected = cep_single_point_rejected,
    geometry_nonmissing = count_nonmissing(dt, "geometry")
  )
}

summarise_second_stage_output <- function(dt) {
  if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)

  list(
    output_rows = nrow(dt),
    accepted_rows = count_equal(dt, "aceito_updated", 1L),
    rejected_rows = count_equal(dt, "aceito_updated", 0L),
    imputed_rows = count_equal(dt, "imputada", 1L),
    geometry_nonmissing = count_nonmissing(dt, "geometry_updated")
  )
}
