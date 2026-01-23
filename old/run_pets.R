source("scripts/general-functions.R")
source("scripts/data-standardization.R")
source("geocoding_first_stage.R")
source("geocoding_second_stage.R")

required_packages <- c("enderecobr","geocodebr", "data.table", "sf")

check_and_install_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)


######### PETS #########

dt <- as.data.table(readRDS("D:\\Bases\\PETLOVER\\Clinicas_RAIS.rds"))
dt[, municipio_7 := ibge6_to_7(municipio)]
dt <- change_cep_99999999_to_na(dt, "cep", "municipio")
dt <- add_col_if_missing(dt, "numlograd", NA) 

campos <- correspondencia_campos(
  logradouro = "endereco",
  numero = "numlograd",
  cep = "cep",
  bairro = "bairro",
  municipio = "municipio_7",
  estado = "uf"
)


res <- main_geocodificacao(
  dt     = dt,
  campos = campos,
  var_col = "estoque",
  sd_threshold_km = 0.5
)

dt_first_stage  <- res$dt_f
stats <- res$stats

t <- head(dt_first_stage, 1000)
