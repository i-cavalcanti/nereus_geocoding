source("scripts/general-functions.R")
source("scripts/data-standardization.R")
source("scripts/metrics-functions.R")
source("logradouro_num_string.R")
source("geocoding_first_stage.R")
source("geocoding_second_stage.R")
source("geocodificar_enderecos.R")
source("exec.R")

required_packages <- c("enderecobr","geocodebr", "data.table", "sf")

check_and_install_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)


regions <- c("sp","centrooeste", "norte", "nordeste", "sul", "mgesrj", "ni")

for (region in regions) {
  out <- main_first_stage_geocoding(
    pathname_in  = paste0("D:/Arq-Azzoni/RAIS/rais-geocoding/data"),
    pathname_out = paste0("D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/geocoded_rds_br"),
    years = 2002:2024,
    filename_prefix = region,
    sd_threshold_km = 0.3,
    filter_municipio_7 = NULL,
    #filter_municipio_7 = c("3525904","3543402","3529005","3534708"),
    filter_cnae = NULL,
    #filter_cnae = c(56112, 56201, 56121, 47211, 47296, 10911, 47121, 47237, 47229),
    verbose = TRUE
  )
  rm(out)
  gc()
}

out <- main_first_stage_geocoding(
  pathname_in  = "D:/Arq-Azzoni/RAIS/rais-geocoding/data",
  pathname_out = "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/geocoded_rd_test",
  years = 2019:2019,
  filename_prefix = "sp",
  sd_threshold_km = 0.3,
  #filter_municipio_7 = NULL,
  filter_municipio_7 = c("3525904","3543402","3529005","3534708"),
  #filter_cnae = NULL,
  filter_cnae = c(56112, 56201, 56121, 47211, 47296, 10911, 47121, 47237, 47229),
  verbose = TRUE,
  debug_on_error = TRUE
)


# main_second_stage_geocoding(
#   pathname_out_prev = "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding",
#   out_dir_csv       = "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/csv_por_ano",
#   diff_threshold = 0.2,
#   geo_threshold_m = 1000,
#   filename_prefix = "sp",
#   out_suffix = "_limpo_corr.csv",
#   overwrite_csv = TRUE,
#   verbose = TRUE
# )

main_second_stage_geocoding(
  pathname_out_prev = "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding",
  out_dir_csv       = "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/rds_por_ano",
  pathname_in_first_stage = "D:/Arq-Azzoni/RAIS/rais-geocoding/data",
  diff_threshold = 0.2,
  geo_threshold_m = 1000,
  filename_prefix = "sp",
  out_suffix = "_limpo_corr.csv",
  overwrite_csv = TRUE,
  verbose = TRUE
)

# Tenho de passar adiante a coluna cep