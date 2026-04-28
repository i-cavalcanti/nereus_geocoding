PROJECT_ROOT <- normalizePath(".", winslash = "/", mustWork = TRUE)
setwd(PROJECT_ROOT)

if (file.exists("renv/activate.R")) source("renv/activate.R")

source("config/config_example.R")
source("scripts/_paths.R")
source("R/00_load.R")

raw_csv_dir <- resolve_project_path(config$raw_csv_dir)
first_stage_out <- resolve_project_path(config$first_stage_out)
final_out <- resolve_project_path(config$final_out)
log_file <- resolve_project_path(config$log_file)
run_id <- if (is.null(config$run_id)) make_run_id("run") else config$run_id

for (region in config$regions) {
  main_second_stage_geocoding(
    pathname_in_rds = first_stage_out,
    pathname_in_prev_csv = raw_csv_dir,
    out_dir_rds = final_out,
    filename_prefix = region,
    diff_threshold = config$diff_threshold,
    geo_threshold_m = config$geo_threshold_m,
    input_rds_pattern = config$first_stage_rds_pattern,
    prev_csv_template = config$prev_csv_template,
    final_rds_template = config$final_rds_template,
    overwrite_rds = config$overwrite,
    verbose = config$verbose,
    encoding = config$encoding,
    run_id = run_id,
    log_file = log_file
  )
}
