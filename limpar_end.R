source("scripts/general-functions.R")
source("scripts/data-standardization.R")
source("filtro_numero.R")

required_packages <- c("enderecobr")

check_and_install_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)


dt <- as.data.table(readRDS("D:\\Arq-Azzoni\\UrbanSprawl\\Bases_dados\\RAIS_estab\\Niteroi\\rais_niteroi_2009_2023.rds"))

dt[, municipio_7 := ibge6_to_7(municipio)]
dt <- add_sigla_from_uf(dt, "cduf", "uf_dom")
dt <- change_cep_99999999_to_na(dt, "cep", "municipio")
dt[, numlograd:= NA]



campos <- correspondencia_campos(
  logradouro = "endereco",
  numero = "numlograd",
  cep = "cep",
  bairro = "bairro",
  municipio = "municipio_7",
  estado = "uf_dom"
)

dt <- padronizar_enderecos(dt, campos_do_endereco = campos)
dt[ numero_padr == "S/N", numero_padr := NA]
t <- head(dt, 1000)
View(t)

dt2 <- logradouro_num_string(
  dt,
  endereco_col = "logradouro_padr",
  num_col = "numero_padr",
  complemento_col = "complemento",
  endereco_update_mode = "cut_when_missing_num"
)
t <- head(dt2, 1000)
View(t)


