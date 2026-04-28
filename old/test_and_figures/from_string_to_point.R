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

setDT(dt3)

dt3[, classe :=
      fifelse(precisao %in% c("numero", "numero_aproximado", "logradouro"), 0,
      fifelse(precisao == "cep", 1, 2))]



dt_cep <- dt3[classe == 1]

#Filtro ceps

# CEPs únicos
ceps <- unique(dt_cep$cep_padr)


#### 2. Geocodificar CEPs únicos ####
df_ceps <- geocodebr::busca_por_cep(
  cep          = ceps,
  resultado_sf = TRUE,
  verboso      = FALSE
)

#### 3. Distância máxima entre pontos por CEP ####
lst <- split(df_ceps, df_ceps$cep)

sd_by_cep <- rbindlist(
  lapply(names(lst), function(z) {
    x <- lst[[z]]
    n <- nrow(x)
    
    if (n < 2L) {
      return(data.table(
        cep = z,
        mean_dist_m = NA_real_,
        sd_dist_m   = NA_real_
      ))
    }
    
    # centróide
    centroid <- st_centroid(st_union(x))
    
    # distância dos pontos ao centróide
    d <- st_distance(x, centroid)
    
    data.table(
      cep          = z,
      mean_dist_m  = mean(as.numeric(d)),
      sd_dist_m    = sd(as.numeric(d))
    )
  })
)

sd_by_cep[, `:=`(
  mean_dist_km = mean_dist_m / 1000,
  sd_dist_km   = sd_dist_m / 1000
)]


#### 4. Resumo por CEP ####
df_ceps_dt <- as.data.table(df_ceps)

summary_ceps <- df_ceps_dt[, .(
  n_points = .N,
  single   = .N == 1L
), by = cep]

final_tbl <- merge(
  sd_by_cep,
  summary_ceps,
  by = "cep",
  all.x = TRUE
)
setorder(final_tbl, cep)

# limiar ajustável (ex.: 500 metros)
sd_threshold_km <- 0.5

#Se os pontos de um CEP costumam ficar, em média, 
#a menos de ~500 m de variação em relação ao centróide,
#o CEP é considerado consistente.

final_tbl[, aceito := fifelse(
  single == TRUE | sd_dist_km <= sd_threshold_km,
  1L, 0L
)]

ceps_aceitos <- final_tbl[aceito == 1, cep]

rm(df_ceps_dt, lst, sd_by_cep, summary_ceps, final_tbl)

#Filtro endereços aceitos

dt3[, aceito := fifelse(
  classe == 0L |
  (classe == 1L & cep_padr %in% ceps_aceitos),
  1L, 0L
)]

descritivas <- dt3[, .N, by = aceito][
  , `:=`(
    proporcao  = N / sum(N),
    percentual = 100 * N / sum(N)
  )
]

print(descritivas)


