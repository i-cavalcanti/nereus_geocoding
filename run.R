source("scripts/general-functions.R")
source("scripts/data-standardization.R")
source("geocoding_first_stage.R")

required_packages <- c("enderecobr","geocodebr", "data.table", "sf")

check_and_install_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)

######### RAIS #########

dt <- data.table(fread("D:\\Arq-Azzoni\\RAIS\\rais-geocoding\\data\\raw\\rais_temp\\sp_estb_2023_limpo.csv"))


dt[, municipio_7 := ibge6_to_7(municipio)]
dt <- add_sigla_from_uf(dt, "cduf", "uf_dom")
dt <- change_cep_99999999_to_na(dt, "cep", "municipio")
dt <- add_col_if_missing(dt, "numlograd", NA) # An update would be: rerun geocoding with number extraction from address field 


campos <- correspondencia_campos(
  logradouro = "endereco",
  numero = "numlograd",
  cep = "cep",
  bairro = "bairro",
  municipio = "municipio_7",
  estado = "uf_dom"
)

res <- main_geocodificacao(
  dt     = dt,
  campos = campos,
  var_col = "estoque",
  sd_threshold_km = 0.5
)

dt_first_stage  <- res$dt_f
stats <- res$stats




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
