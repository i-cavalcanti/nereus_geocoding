library(data.table)
library(geocodebr)
library(sf)


# CEPs únicos
ceps <- unique(dt_f$cep_padr)


#### 2. Geocodificar CEPs únicos ####
df_ceps <- geocodebr::busca_por_cep(
  cep          = ceps,
  resultado_sf = TRUE,
  verboso      = FALSE
)

#### 3. Distância máxima entre pontos por CEP ####
lst <- split(df_ceps, df_ceps$cep)

max_dist_by_cep <- rbindlist(
  lapply(names(lst), function(z) {
    x <- lst[[z]]
    n <- nrow(x)
    
    if (n < 2L) {
      return(data.table(cep = z, max_dist_m = NA_real_))
    }
    
    d <- st_distance(x)
    data.table(
      cep        = z,
      max_dist_m = as.numeric(max(d))
    )
  })
)

max_dist_by_cep[, max_dist_km := max_dist_m / 1000]


#### 4. Resumo por CEP ####
df_ceps_dt <- as.data.table(df_ceps)

summary_ceps <- df_ceps_dt[, .(
  n_points = .N,
  single   = .N == 1L
), by = cep]


#### 5. Tabela final ####
final_tbl <- merge(
  max_dist_by_cep,
  summary_ceps,
  by = "cep",
  all.x = TRUE
)

setorder(final_tbl, cep)
rm(df_ceps_dt, lst, max_dist_by_cep, summary_ceps)

final_tbl


df_ceps <- as.data.table(df_ceps)
d <- df_ceps[cep == "24110-105"]
d_f <- dt_f[cep_padr == "24110-105"]

