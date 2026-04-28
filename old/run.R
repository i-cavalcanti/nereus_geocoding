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
    pathname_in  = "D:/Arq-Azzoni/RAIS/rais-geocoding/data",
    pathname_out = "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/geocoded_rds_br",
    years = 2010:2024,
    filename_prefix = region,
    sd_threshold_km = 0.3,
    filter_municipio_7 = NULL,
    #filter_municipio_7 = c("3525904","3543402","3529005","3534708"),
    filter_cnae = obj_num,
    #filter_cnae = c(56112, 56201, 56121, 47211, 47296, 10911, 47121, 47237, 47229),
    verbose = TRUE,
    debug_on_error = TRUE
  )
}


for (region in regions) {
  main_second_stage_geocoding(
    pathname_in_rds      = "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/geocoded_rds_br",
    pathname_in_prev_csv = "D:/Arq-Azzoni/RAIS/rais-geocoding/data/raw/rais_temp",
    out_dir_rds          = "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/rds_por_ano",
    diff_threshold       = 0.2,
    geo_threshold_m      = 1000,
    filename_prefix      = region,
    input_rds_pattern    = "^dt_f_{prefix}_(\\d{4})\\.rds$",
    prev_csv_template    = "{prefix}_estb_{year}_limpo.csv",
    out_suffix           = "_limpo_corr.rds",
    overwrite_rds        = TRUE,
    verbose              = TRUE
  )
}

