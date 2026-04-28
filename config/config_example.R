# Machine-specific paths -------------------------------------------------------
# These paths may point outside the repository. Keep all machine-specific paths
# here so scripts and functions remain portable.

data_root <- "D:/Arq-Azzoni/RAIS/rais-geocoding/data"
raw_csv_dir <- file.path(data_root, "raw", "rais_temp")
first_stage_out <- "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/geocoded_rds_br"
final_out <- "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/rds_por_ano"
log_dir <- "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/logs"
log_file <- file.path(log_dir, "run_log.csv")

# File-name templates ---------------------------------------------------------
# Available placeholders: {prefix}, {year}.
# Change these if your source files use a different naming convention.

input_csv_template <- "{prefix}_estb_{year}_limpo.csv"
prev_csv_template <- input_csv_template
first_stage_rds_template <- "dt_f_{prefix}_{year}.rds"
first_stage_rds_pattern <- "^dt_f_{prefix}_(\\d{4})\\.rds$"
final_rds_template <- "{prefix}_estb_{year}_limpo_corr.rds"

# Distance CRS by region ------------------------------------------------------
# These CRS values are used only for distance calculations in meters.
# EPSG:5880 is SIRGAS 2000 / Brazil Polyconic and is suitable as a broad
# Brazil-wide fallback. Regional UTM CRS values can be used for smaller areas.

region_distance_crs <- c(
  sp = 31983,
  centrooeste = 5880,
  norte = 5880,
  nordeste = 5880,
  sul = 31982,
  mgesrj = 31983,
  ni = 5880
)

default_distance_crs <- 5880

config <- list(
  data_root = data_root,
  raw_csv_dir = raw_csv_dir,
  first_stage_out = first_stage_out,
  final_out = final_out,
  log_dir = log_dir,
  log_file = log_file,
  run_id = NULL,
  input_csv_template = input_csv_template,
  prev_csv_template = prev_csv_template,
  first_stage_rds_template = first_stage_rds_template,
  first_stage_rds_pattern = first_stage_rds_pattern,
  final_rds_template = final_rds_template,
  region_distance_crs = region_distance_crs,
  default_distance_crs = default_distance_crs,
  regions = c("sp", "centrooeste", "norte", "nordeste", "sul", "mgesrj", "ni"),
  years = 2010:2024,
  encoding = "Latin-1",
  sd_threshold_km = 0.3,
  accept_single_point_cep = TRUE,
  diff_threshold = 0.2,
  geo_threshold_m = 1000,
  filter_municipio_7 = NULL,
  filter_cnae = NULL,
  overwrite = FALSE,
  continue_on_error = FALSE,
  verbose = TRUE
)
