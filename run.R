source("scripts/general-functions.R")
source("scripts/data-standardization.R")
source("geocoding_first_stage.R")
source("geocoding_second_stage.R")
source("exec.R")

required_packages <- c("enderecobr","geocodebr", "data.table", "sf")

check_and_install_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)



out <- main_first_stage_geocoding(
  pathname_in  = "D:/Arq-Azzoni/RAIS/rais-geocoding/data",
  pathname_out = "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding",
  years = 2017:2002,
  filename_prefix = "sp",
  sd_threshold_km = 0.3,
  filter_municipio_7 = NULL,
  verbose = TRUE
)


main_second_stage_geocoding(
  pathname_out_prev = "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding",
  out_dir_csv       = "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/csv_por_ano",
  diff_threshold = 0.3,
  filename_prefix = "sp",
  out_suffix = "_limpo_corr.csv",
  overwrite_csv = TRUE,
  verbose = TRUE
)

#Adicionar a regiao nos rsd salvos na primeira etapa