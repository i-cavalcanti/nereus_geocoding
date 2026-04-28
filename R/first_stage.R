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

  if (is.null(input_csv_dir)) {
    if (is.null(pathname_in)) {
      stop("Provide input_csv_dir, or provide pathname_in so input_csv_dir can be inferred.", call. = FALSE)
    }
    input_csv_dir <- file.path(pathname_in, "raw", "rais_temp")
  }

  if (!dir.exists(input_csv_dir)) stop("Input CSV directory does not exist: ", input_csv_dir, call. = FALSE)
  ensure_dir(pathname_out)

  years <- as.integer(years)
  rds_paths <- stats::setNames(vector("list", length(years)), as.character(years))
  skipped <- list()

  needed_min_raw <- c("municipio", "cduf", "cep", "endereco", "estoque")
  geocode_keep <- c(
    "id", "endereco", "numlograd", "cep", "bairro", "municipio_7", "uf_dom",
    "estoque", "identificad_m", "municipio", "matrizfilial"
  )

  params <- paste0(
    "sd_threshold_km=", sd_threshold_km,
    ";accept_single_point_cep=", accept_single_point_cep,
    ";cep_distance_crs=", cep_distance_crs
  )

  for (year in years) {
    started_at <- Sys.time()
    project_log(verbose, "First stage: ", filename_prefix, " ", year)

    input_name <- build_template(input_csv_template, prefix = filename_prefix, year = year)
    output_name <- build_template(output_rds_template, prefix = filename_prefix, year = year)

    file_in <- file.path(input_csv_dir, input_name)
    rds_file <- file.path(pathname_out, output_name)

    if (!file.exists(file_in)) {
      skipped[[as.character(year)]] <- "missing_input_file"
      project_log(verbose, "Missing input file: ", file_in)

      append_run_log(log_file, make_log_row(
        run_id = run_id,
        stage = "first_stage",
        region = filename_prefix,
        year = year,
        status = "missing_input_file",
        message = "Input CSV was not found.",
        input_file = file_in,
        output_file = rds_file,
        started_at = started_at,
        ended_at = Sys.time(),
        params = params
      ))

      next
    }

    if (file.exists(rds_file) && !isTRUE(overwrite)) {
      msg <- paste0("Output already exists. Set overwrite = TRUE to replace: ", rds_file)
      skipped[[as.character(year)]] <- "output_exists"

      append_run_log(log_file, make_log_row(
        run_id = run_id,
        stage = "first_stage",
        region = filename_prefix,
        year = year,
        status = "output_exists",
        message = msg,
        input_file = file_in,
        output_file = rds_file,
        started_at = started_at,
        ended_at = Sys.time(),
        params = params
      ))

      if (isTRUE(continue_on_error)) next
      stop(msg, call. = FALSE)
    }

    run_one_year <- function() {
      read_cols <- if (is.null(select_cols)) {
        NULL
      } else {
        unique(c(
          select_cols, needed_min_raw, "id", "bairro", "numlograd",
          "identificad_m", "matrizfilial", "clascnae20"
        ))
      }

      dt <- data.table::fread(file_in, encoding = encoding, select = read_cols, showProgress = FALSE)
      input_rows <- nrow(dt)

      require_columns(dt, needed_min_raw, paste0("input ", year))

      add_col_if_missing(dt, "bairro", NA_character_)
      add_col_if_missing(dt, "numlograd", NA)

      dt[, municipio_7 := ibge6_to_7(municipio)]
      dt <- add_sigla_from_uf(dt, "cduf", "uf_dom")
      dt <- change_cep_99999999_to_na(dt, "cep", "municipio")

      if (!is.null(filter_municipio_7)) dt <- dt[municipio_7 %in% unlist(filter_municipio_7)]

      if (!is.null(filter_cnae)) {
        require_columns(dt, "clascnae20", paste0("input ", year))
        dt <- dt[clascnae20 %in% unlist(filter_cnae)]
      }

      rows_after_filters <- nrow(dt)

      require_columns(dt, geocode_keep, paste0("geocoding input ", year))
      dt <- dt[, ..geocode_keep]

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

      geocoding_result <- main_geocodificacao(
        dt = dt,
        campos = campos,
        campos_pdr = campos_pdr,
        sd_threshold_km = sd_threshold_km,
        accept_single_point_cep = accept_single_point_cep,
        cep_distance_crs = cep_distance_crs,
        verbose = verbose,
        return_diagnostics = TRUE
      )

      dt_final <- geocoding_result$dt_final
      cep_diagnostics <- geocoding_result$cep_diagnostics

      saveRDS(dt_final, rds_file)

      summary <- summarise_first_stage_output(dt_final, cep_diagnostics)

      list(
        path = rds_file,
        input_rows = input_rows,
        rows_after_filters = rows_after_filters,
        summary = summary
      )
    }

    if (isTRUE(continue_on_error)) {
      out <- tryCatch(run_one_year(), error = function(e) e)
      if (inherits(out, "error")) {
        skipped[[as.character(year)]] <- conditionMessage(out)
        project_log(verbose, "Error in ", filename_prefix, " ", year, ": ", conditionMessage(out))

        append_run_log(log_file, make_log_row(
          run_id = run_id,
          stage = "first_stage",
          region = filename_prefix,
          year = year,
          status = "error",
          message = conditionMessage(out),
          input_file = file_in,
          output_file = rds_file,
          started_at = started_at,
          ended_at = Sys.time(),
          params = params
        ))

        next
      }

      rds_paths[[as.character(year)]] <- out$path

      append_run_log(log_file, make_log_row(
        run_id = run_id,
        stage = "first_stage",
        region = filename_prefix,
        year = year,
        status = "success",
        input_file = file_in,
        output_file = out$path,
        started_at = started_at,
        ended_at = Sys.time(),
        input_rows = out$input_rows,
        rows_after_filters = out$rows_after_filters,
        output_rows = out$summary$output_rows,
        accepted_rows = out$summary$accepted_rows,
        rejected_rows = out$summary$rejected_rows,
        class0_rows = out$summary$class0_rows,
        class1_rows = out$summary$class1_rows,
        class2_rows = out$summary$class2_rows,
        cep_rows = out$summary$cep_rows,
        cep_accepted = out$summary$cep_accepted,
        cep_rejected = out$summary$cep_rejected,
        cep_single_point_accepted = out$summary$cep_single_point_accepted,
        cep_single_point_rejected = out$summary$cep_single_point_rejected,
        geometry_nonmissing = out$summary$geometry_nonmissing,
        params = params
      ))
    } else {
      out <- tryCatch(run_one_year(), error = function(e) {
        append_run_log(log_file, make_log_row(
          run_id = run_id,
          stage = "first_stage",
          region = filename_prefix,
          year = year,
          status = "error",
          message = conditionMessage(e),
          input_file = file_in,
          output_file = rds_file,
          started_at = started_at,
          ended_at = Sys.time(),
          params = params
        ))
        stop(e)
      })

      rds_paths[[as.character(year)]] <- out$path

      append_run_log(log_file, make_log_row(
        run_id = run_id,
        stage = "first_stage",
        region = filename_prefix,
        year = year,
        status = "success",
        input_file = file_in,
        output_file = out$path,
        started_at = started_at,
        ended_at = Sys.time(),
        input_rows = out$input_rows,
        rows_after_filters = out$rows_after_filters,
        output_rows = out$summary$output_rows,
        accepted_rows = out$summary$accepted_rows,
        rejected_rows = out$summary$rejected_rows,
        class0_rows = out$summary$class0_rows,
        class1_rows = out$summary$class1_rows,
        class2_rows = out$summary$class2_rows,
        cep_rows = out$summary$cep_rows,
        cep_accepted = out$summary$cep_accepted,
        cep_rejected = out$summary$cep_rejected,
        cep_single_point_accepted = out$summary$cep_single_point_accepted,
        cep_single_point_rejected = out$summary$cep_single_point_rejected,
        geometry_nonmissing = out$summary$geometry_nonmissing,
        params = params
      ))
    }
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
