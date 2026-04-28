source("scripts/general-functions.R")
source("scripts/data-standardization.R")
source("geocoding_first_stage.R")
source("geocoding_second_stage.R")
source("exec.R")

required_packages <- c("enderecobr","geocodebr", "data.table", "sf", "arrow")

check_and_install_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)


path = "D:/Pessoal/Lucas Lopes/Projetos/crimes_sp/base_consolidada_2.0.parquet"

dt <- as.data.table(read_parquet(path))
t<-head(dt, 1000)
names(dt)

dt[, codigo_ibge := as.character(codigo_ibge)]
dt[, uf_dom := "SP"]
dt[, cep := NA]



campos <- correspondencia_campos(
    logradouro = "logradouro",
    numero     = "numero_logradouro",
    cep        = "cep",
    bairro     = "bairro",
    municipio  = "codigo_ibge",
    estado     = "uf_dom"
)


dt4 <- geocodificar_enderecos(
dt                 = dt,
campos_do_endereco = campos,
classe_col         = "classe",
operation = "always_cut_on_stopword"
)
   

t <- head(dt3, 1000)
summary(dt4$classe)
dt4[, .N, by = classe][order(-N)]
dt3[, .N, by = classe][, pct := 100 * N / sum(N)][order(-pct)]


path = "D:/Pessoal/Lucas Lopes/Projetos/crimes_sp/base_consolidada_3.rds"
saveRDS(dt3, file = path)
