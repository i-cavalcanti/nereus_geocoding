#### filtro_cep.R ####
source("scripts/general-functions.R")
required_packages <- c("geocodebr", "data.table", "sf")

check_and_install_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)

# CEPs únicos
ceps <- unique(dt_cep$cep_padr)


#### 2. Geocodificar CEPs únicos ####
df_ceps <- geocodebr::busca_por_cep(
  cep          = ceps,
  resultado_sf = TRUE,
  verboso      = FALSE
)

#### 3. Distância máxima entre pontos por CEP ####
# lst <- split(df_ceps, df_ceps$cep)

# max_dist_by_cep <- rbindlist(
#   lapply(names(lst), function(z) {
#     x <- lst[[z]]
#     n <- nrow(x)
    
#     if (n < 2L) {
#       return(data.table(cep = z, max_dist_m = NA_real_))
#     }
    
#     d <- st_distance(x)
#     data.table(
#       cep        = z,
#       max_dist_m = as.numeric(max(d))
#     )
#   })
# )

# max_dist_by_cep[, max_dist_km := max_dist_m / 1000]

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


#### 5. Tabela final ####
# final_tbl <- merge(
#   max_dist_by_cep,
#   summary_ceps,
#   by = "cep",
#   all.x = TRUE
# )

final_tbl <- merge(
  sd_by_cep,
  summary_ceps,
  by = "cep",
  all.x = TRUE
)
setorder(final_tbl, cep)

rm(df_ceps_dt, lst, max_dist_by_cep, summary_ceps)

# final_tbl[, aceito := fifelse(single == TRUE | max_dist_km <= 1, 1L, 0L)]
# a<-final_tbl[aceito != 1]

# limiar ajustável (ex.: 500 metros)
sd_threshold_km <- 0.5

final_tbl[, aceito := fifelse(
  single == TRUE | sd_dist_km <= sd_threshold_km,
  1L, 0L
)]

nao_aceitos <- final_tbl[aceito != 1]
aceitos <- final_tbl[aceito == 1]
ceps_aceitos <- final_tbl[aceito == 1, cep]

df_ceps <- as.data.table(df_ceps)
d <- df_ceps[cep == "24110-105"]
d_f <- dt_f[cep_padr == "24110-105"]

#Se os pontos de um CEP costumam ficar, em média, 
#a menos de ~500 m de variação em relação ao centróide,
#o CEP é considerado consistente.