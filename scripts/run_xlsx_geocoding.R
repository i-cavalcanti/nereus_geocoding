# Runs the nereus first-stage geocoding pipeline on an Excel file of
# establishments. Produces a CSV in the same format as the standard
# first-stage outputs (one row per establishment, same columns).
#
# Usage: open in RStudio and source, or run from the project root with:
#   Rscript scripts/run_xlsx_geocoding.R

# ---- Project root & function library ----------------------------------------
if (!interactive()) {
  # sys.frame(1)$ofile does not exist under Rscript; commandArgs is reliable.
  args     <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) == 0L) {
    stop(
      "Cannot determine script path.",
      " Run as: Rscript scripts/run_xlsx_geocoding.R"
    )
  }
  script_path <- sub("--file=", "", file_arg[1L])
  proj_root   <- normalizePath(file.path(dirname(script_path), ".."))
} else {
  proj_root <- normalizePath(
    file.path(dirname(rstudioapi::getActiveDocumentContext()$path), ".."),
    mustWork = TRUE
  )
}
setwd(proj_root)
source("R/00_load.R")

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Install readxl first: install.packages('readxl')")
}

# ---- Parameters -------------------------------------------------------------
input_xlsx <- paste0(
  "D:/Users/ivan.cavalcanti/Documents/Projects/rais_vinculos/data/",
  "nao_encontradas_setores_cnpj_2023_com_endereco",
  " - final - geolocalizados.xlsx"
)
output_csv <- sub("\\.xlsx$", "_geocodebr.csv", input_xlsx, ignore.case = TRUE)

sd_threshold_km         <- 0.3
accept_single_point_cep <- TRUE
distance_crs            <- 5880  # SIRGAS 2000 / Brazil Polyconic

# ---- Read input -------------------------------------------------------------
cat("Reading Excel:", input_xlsx, "\n")
raw <- data.table::as.data.table(readxl::read_excel(input_xlsx))
cat("Rows read:", nrow(raw), "\n\n")

# ---- Column mapping ---------------------------------------------------------
# Concatenate tipo_logradouro + logradouro into the single 'endereco' field
# the pipeline expects (e.g. "FAZENDA SAO MARTINHO").
build_endereco <- function(tipo, logr) {
  tipo     <- ifelse(is.na(tipo) | trimws(tipo) == "", "", trimws(tipo))
  logr     <- ifelse(is.na(logr) | trimws(logr) == "", "", trimws(logr))
  combined <- trimws(paste(tipo, logr))
  ifelse(combined == "", NA_character_, combined)
}

# CEPs stored as numeric in Excel lose leading zeros (e.g. 01310100 becomes
# 1310100). Pad back to 8 digits before any string comparison.
pad_cep <- function(x) {
  xi <- suppressWarnings(as.integer(x))
  ifelse(is.na(xi), NA_character_, sprintf("%08d", xi))
}

dt_input <- raw[, .(
  id            = identificad_m,
  identificad_m = identificad_m,
  municipio     = as.character(municipio),
  cep           = pad_cep(cep),
  endereco      = build_endereco(tipo_logradouro, logradouro),
  numlograd     = as.character(numero),
  bairro        = bairro,
  uf_dom        = uf,
  estoque       = estoque,
  matrizfilial  = matrizfilial
)]

# ---- Standardize (mirrors standardize_first_stage_input) --------------------
add_col_if_missing(dt_input, "bairro",    NA_character_)
add_col_if_missing(dt_input, "numlograd", NA_character_)

dt_input[, municipio_7 := ibge6_to_7(municipio)]

# Replace CEP 99999999 with NA (municipio 412625 is the documented exception)
change_cep_99999999_to_na(dt_input, "cep", "municipio")

# The pipeline requires unique id values; warn and deduplicate if needed.
if (data.table::uniqueN(dt_input$id) != nrow(dt_input)) {
  n_dup <- nrow(dt_input) - data.table::uniqueN(dt_input$id)
  warning(
    n_dup, " duplicate identificad_m values found.",
    " Keeping first occurrence."
  )
  dt_input <- dt_input[!duplicated(id)]
}

cat("Rows prepared for geocoding:", nrow(dt_input), "\n\n")

# ---- Subset to pipeline input columns ---------------------------------------
keep_cols <- c(
  "id", "endereco", "numlograd", "cep", "bairro",
  "municipio_7", "uf_dom", "estoque", "identificad_m",
  "municipio", "matrizfilial"
)
dt_geo <- dt_input[, ..keep_cols]

# ---- Address field mappings (mirrors make_first_stage_address_fields) --------
campos <- geocodebr::definir_campos(
  logradouro = "endereco",
  numero     = "numlograd",
  cep        = "cep",
  localidade = "bairro",
  municipio  = "municipio_7",
  estado     = "uf_dom"
)

campos_pdr <- enderecobr::correspondencia_campos(
  logradouro = "endereco",
  numero     = "numlograd",
  cep        = "cep",
  bairro     = "bairro",
  municipio  = "municipio_7",
  estado     = "uf_dom"
)

# ---- Run geocoding + quality filter ----------------------------------------
cat("Running geocoding pipeline (this may take several minutes)...\n")
t0 <- proc.time()

result <- main_geocodificacao(
  dt                      = dt_geo,
  campos                  = campos,
  campos_pdr              = campos_pdr,
  sd_threshold_km         = sd_threshold_km,
  accept_single_point_cep = accept_single_point_cep,
  cep_distance_crs        = distance_crs,
  verbose                 = TRUE,
  return_diagnostics      = FALSE
)

elapsed <- (proc.time() - t0)[["elapsed"]]
cat(sprintf(
  "\nGeocoding complete in %.1f seconds (%.1f min)\n",
  elapsed, elapsed / 60
))
cat("Output rows    :", nrow(result), "\n")
cat("Accepted       :", sum(result$aceito == 1L, na.rm = TRUE), "\n")
cat("Rejected       :", sum(result$aceito == 0L, na.rm = TRUE), "\n")
cat("  Class 0 (address match):", sum(result$classe == 0L, na.rm = TRUE), "\n")
cat("  Class 1 (CEP-level)    :", sum(result$classe == 1L, na.rm = TRUE), "\n")
cat("  Class 2 (low prec.)    :", sum(result$classe == 2L, na.rm = TRUE), "\n")
cat("  NA class               :", sum(is.na(result$classe)), "\n\n")

# ---- Extract coordinates from geometry -------------------------------------
cat("Extracting coordinates...\n")
coords_mat <- do.call(rbind, lapply(result$geometry, function(g) {
  if (is.null(g) || length(g) == 0L) return(c(NA_real_, NA_real_))
  co <- tryCatch(sf::st_coordinates(g), error = function(e) NULL)
  if (is.null(co) || nrow(co) == 0L) return(c(NA_real_, NA_real_))
  c(co[1L, 1L], co[1L, 2L])  # longitude, latitude (SIRGAS 2000)
}))

data.table::set(result, j = "longitude", value = coords_mat[, 1L])
data.table::set(result, j = "latitude",  value = coords_mat[, 2L])
result[, geometry := NULL]

# ---- Join geocoding results back to input columns --------------------------
dt_final <- merge(
  dt_input,
  result,
  by   = "id",
  all.x = TRUE,
  sort  = FALSE
)

# Reorder: input identification fields, then address, then geocoding results
col_order <- c(
  "id", "identificad_m", "municipio", "municipio_7", "uf_dom",
  "cep", "cep_padr",
  "endereco", "numlograd", "bairro",
  "estoque", "matrizfilial",
  "best_step", "endereco_best", "endereco_fonte",
  "precisao_best", "best_score", "classe", "aceito",
  "longitude", "latitude"
)
col_order <- col_order[col_order %in% names(dt_final)]
data.table::setcolorder(dt_final, col_order)

# ---- Save CSV ---------------------------------------------------------------
cat("Saving CSV:", output_csv, "\n")
data.table::fwrite(dt_final, output_csv, encoding = "UTF-8", bom = TRUE)
cat("Done. Rows written:", nrow(dt_final), "\n")
