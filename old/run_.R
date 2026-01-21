source("scripts/general-functions.R")
source("scripts/data-standardization.R")
source("geocoding_first_stage.R")
source("geocoding_second_stage.R")

required_packages <- c("enderecobr","geocodebr", "data.table", "sf")

check_and_install_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)

######### RAIS #########

dt <- data.table(fread("D:\\Arq-Azzoni\\RAIS\\rais-geocoding\\data\\raw\\rais_temp\\sp_estb_2023_limpo.csv"))
t<- head(dt, 1000)

cols_list <- as.list(names(dt))

dt[, municipio_7 := ibge6_to_7(municipio)]
dt <- add_sigla_from_uf(dt, "cduf", "uf_dom")
dt <- change_cep_99999999_to_na(dt, "cep", "municipio")
dt <- add_col_if_missing(dt, "numlograd", NA) # An update would be: rerun geocoding with number extraction from address field 
t<-head(dt, 1000)
dt_filtrado <- dt[municipio_7 == "3525904"]

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


t <- head(dt_first_stage, 1000)

res <- main_multiyear(
  dt_first_stage,
  diff_threshold = 0.3
)

dt_final <- res$dt_final
stats    <- res$stats

t <- head(dt_final, 10000)

print(stats)




##### Stats #####



perc_aceito <- dt_final[
  ,
  .N,
  by = aceito_updated
][
  ,
  `:=`(
    prop = N / sum(N),
    perc = 100 * N / sum(N)
  )
]

print(perc_aceito)


prop_aceito_estoque <- dt_final[
  ,
  .(estoque_total = sum(estoque, na.rm = TRUE)),
  by = aceito_updated
][
  ,
  `:=`(
    prop_estoque = estoque_total / sum(estoque_total),
    perc_estoque = 100 * estoque_total / sum(estoque_total)
  )
]

print(prop_aceito_estoque)



######### Cycle #########

rodar_multiyear_geocoding <- function(
  pathname_in,
  region = "sp",
  years = 2023:2018,
  encoding = "Latin-1",
  sd_threshold_km = 0.3,
  verbose = TRUE
) {
  stopifnot(
    dir.exists(pathname_in),
    is.character(region), length(region) == 1L,
    is.numeric(years), length(years) >= 1L,
    is.numeric(sd_threshold_km), length(sd_threshold_km) == 1L
  )
  
  log <- function(...) if (isTRUE(verbose)) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))
  
  dt_list <- vector("list", length(years))
  stats_list <- vector("list", length(years))
  names(dt_list) <- as.character(years)
  names(stats_list) <- as.character(years)
  
  for (year in years) {
    log("Ano: ", year)
    
    filename <- paste0(region, "_estb_", year, "_limpo")
    log("Arquivo: ", filename, ".csv")
    
    file_in <- file.path(pathname_in, "raw", "rais_temp", paste0(filename, ".csv"))
    stopifnot(file.exists(file_in))
    
    dt <- data.table::fread(file_in, encoding = encoding)
    log("Linhas lidas: ", nrow(dt))
    
    # Pré-processamentos
    dt[, municipio_7 := ibge6_to_7(municipio)]
    dt <- add_sigla_from_uf(dt, "cduf", "uf_dom")
    dt <- change_cep_99999999_to_na(dt, "cep", "municipio")
    dt <- add_col_if_missing(dt, "numlograd", NA)
    dt <- dt[municipio_7 == "3525904"] # Filtering for testing purposes only - Jundiaí
    
    campos <- correspondencia_campos(
      logradouro = "endereco",
      numero     = "numlograd",
      cep        = "cep",
      bairro     = "bairro",
      municipio  = "municipio_7",
      estado     = "uf_dom"
    )
    
    # Geocodificação
    res <- main_geocodificacao(
      dt             = dt,
      campos         = campos,
      var_col        = "estoque",
      sd_threshold_km = sd_threshold_km
    )
    
    dt_list[[as.character(year)]] <- res$dt_f
    stats_list[[as.character(year)]] <- res$stats
    
    log("Concluído: ", year, " | dt_f linhas: ", nrow(res$dt_f))
    
    rm(dt, res)
    gc()
  }
  
  list(
    dt_list    = dt_list,
    stats_list = stats_list
  )
}


pathname_in <- "D:\\Arq-Azzoni\\RAIS\\rais-geocoding\\data\\"
region <- "sp"
years <- 2023:2018

out <- rodar_multiyear_geocoding(
  pathname_in = pathname_in,
  region = region,
  years = years,
  sd_threshold_km = 0.3,
  verbose = TRUE
)

dt_list <- out$dt_list
stats_list <- out$stats_list  



dt_all <- rbindlist(dt_list, use.names = TRUE, fill = TRUE, idcol = "year")
dt_all[, year := as.integer(year)]

t<- head(dt_all, 1000)



perc_aceito <- dt_all[
  ,
  .N,
  by = aceito
][
  ,
  `:=`(
    prop = N / sum(N),
    perc = 100 * N / sum(N)
  )
]

print(perc_aceito)


prop_aceito_estoque <- dt_all[
  ,
  .(estoque_total = sum(estoque, na.rm = TRUE)),
  by = aceito
][
  ,
  `:=`(
    prop_estoque = estoque_total / sum(estoque_total),
    perc_estoque = 100 * estoque_total / sum(estoque_total)
  )
]

print(prop_aceito_estoque)



res <- main_multiyear(
  dt_all,
  diff_threshold = 0.3
)

dt_final <- res$dt_final
stats    <- res$stats



perc_aceito <- dt_final[
  ,
  .N,
  by = aceito_updated
][
  ,
  `:=`(
    prop = N / sum(N),
    perc = 100 * N / sum(N)
  )
]

print(perc_aceito)


prop_aceito_estoque <- dt_final[
  ,
  .(estoque_total = sum(estoque, na.rm = TRUE)),
  by = aceito_updated
][
  ,
  `:=`(
    prop_estoque = estoque_total / sum(estoque_total),
    perc_estoque = 100 * estoque_total / sum(estoque_total)
  )
]

print(prop_aceito_estoque)
























######### Manual multi-year run #########

pathname_in <- "D:\\Arq-Azzoni\\RAIS\\rais-geocoding\\data\\"
region <- "sp"
years <- c(2023:2002)
dt_list <- list()
stats_list <- list()
for (year in years){
  print(year)
  filename <- paste0(region,"_estb_",year,"_limpo")
  print(filename)
  dt <- fread(paste0(pathname_in,"raw\\rais_temp\\", filename, ".csv"), encoding = "Latin-1")
  
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
  estado = "uf_dom")

  res <- main_geocodificacao(
  dt     = dt,
  campos = campos,
  var_col = "estoque",
  sd_threshold_km = 0.3)

  dt <- res$dt_f
  stats <- res$stats

  dt_list[[as.character(year)]] <- dt
  stats_list[[as.character(year)]] <- stats
  rm(dt,dt_out, result)
  gc()
 }



table_sp <- rbindlist(
  lapply(names(dt_list), function(y) {
    dt_list[[y]][, year := as.integer(y)]  # add year as a column
  }),
  use.names = TRUE,
  fill = TRUE
)
