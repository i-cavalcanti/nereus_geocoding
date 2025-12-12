source("scripts/general-functions.R")

required_packages <- c("gerocodebr", "data.table", "sf")

check_and_install_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)


campos <- geocodebr::definir_campos(
  logradouro = "endereco_limpo",
  numero = "numlograd_novo",
  cep = "cep_padr",
  localidade = "bairro_padr",
  municipio = "municipio_padr",
  estado = "estado_padr"
  )

# Segundo passo: geolocalizar
dt3 <- geocodebr::geocode(
  enderecos = dt2,
  campos_endereco = campos,
  resultado_completo = FALSE,
  resolver_empates = TRUE,
  resultado_sf = TRUE,
  verboso = FALSE
  )

t <- head(dt3, 1000)
View(t)
