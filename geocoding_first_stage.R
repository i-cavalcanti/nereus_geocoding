source("scripts/general-functions.R")
source("scripts/data-standardization.R")
source("filtro_numero_fast.R")

required_packages <- c("enderecobr","geocodebr", "data.table", "sf")

check_and_install_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)


dt <- as.data.table(readRDS("D:\\Arq-Azzoni\\UrbanSprawl\\Bases_dados\\RAIS_estab\\Niteroi\\rais_niteroi_2009_2023.rds"))
dt <- data.table(fread("D:\\Arq-Azzoni\\RAIS\\rais-geocoding\\data\\raw\\rais_temp\\sp_estb_2023_limpo.csv"))

t <- head(dt, 1000)

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



dt2 <- logradouro_num_string_fast(
  dt,
  endereco_col = "logradouro_padr",
  num_col = "numero_padr",
  complemento_col = "complemento",
  endereco_update_mode = "cut_when_missing_num"
)



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

t <- head(dt3, 1000)
View(t)


dt_cep <- dt3[classe == 1]

#Filtro ceps

# CEPs únicos
ceps <- unique(dt_cep$cep_padr)
ceps <- dt_cep[!is.na(cep_padr), unique(cep_padr)]


#### 2. Geocodificar CEPs únicos ####
df_ceps <- geocodebr::busca_por_cep(
  cep          = ceps,
  resultado_sf = TRUE,
  verboso      = FALSE
)

#### 3. Distância máxima entre pontos por CEP ####
df_ceps_sf <- st_transform(df_ceps, 31983) # SIRGAS / UTM 23S (RJ)
coords <- st_coordinates(df_ceps_sf)

df_ceps <- as.data.table(df_ceps)
df_ceps[, `:=`(
  x = coords[, 1],
  y = coords[, 2]
)]

sd_by_cep <- df_ceps[
  , {
      if (.N < 2L) {
        list(mean_dist_m = NA_real_, sd_dist_m = NA_real_)
      } else {
        cx <- mean(x)
        cy <- mean(y)
        d  <- sqrt((x - cx)^2 + (y - cy)^2)
        list(
          mean_dist_m = mean(d),
          sd_dist_m   = sd(d)
        )
      }
    },
  by = cep
]

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

rm(df_ceps_dt, sd_by_cep, summary_ceps, final_tbl, df_ceps, coords)
#gc()

#Filtro endereços aceitos

ceps_ok <- unique(data.table(cep_padr = ceps_aceitos))
setkey(ceps_ok, cep_padr)

dt3[, cep_ok := 0L]

dt3[
  ceps_ok,
  cep_ok := 1L,
  on = "cep_padr",
  nomatch = 0L
]

dt3[, aceito := fcase(
  classe == 0L, 1L,
  classe == 1L & cep_ok == 1L, 1L,
  default = 0L
)]

dt3[, cep_ok := NULL]

t <- head(dt3, 1000)


### Descritivas por n rows

tab <- dt3[, .N, by = .(classe, aceito)]

print(tab)

tab_stats <- dt3[
  , .N, by = .(classe, aceito)
][
  , `:=`(
    prop  = N / sum(N),
    perc  = 100 * N / sum(N)
  ),
  by = classe
]

print(tab_stats)

descritivas <- dt3[, .N, by = aceito][
  , `:=`(
    proporcao  = N / sum(N),
    percentual = 100 * N / sum(N)
  )
]

print(descritivas)


### Descritivas por estoque


stats_estoque <- dt3[
  , .(estoque_total = sum(estoque, na.rm = TRUE)),
  by = .(classe, aceito)
][
  , `:=`(
      prop = estoque_total / sum(estoque_total),
      perc = 100 * estoque_total / sum(estoque_total)
    ),
  by = classe
]

print(stats_estoque)


### Descritivas gerais 
stats_aceito <- dt3[
  , .(
      n_linhas      = .N,
      estoque_total = sum(estoque, na.rm = TRUE)
    ),
  by = aceito
][
  , `:=`(
      prop_linhas  = n_linhas / sum(n_linhas),
      perc_linhas  = 100 * n_linhas / sum(n_linhas),
      prop_estoque = estoque_total / sum(estoque_total),
      perc_estoque = 100 * estoque_total / sum(estoque_total)
    )
]

print(stats_aceito)


####### Testar precisao CEPS únicos ########



dt_single_cep <- df_ceps[
  , if (.N == 1L) .SD,
  by = cep
]
class(dt_single_cep$geometry)


dt_ref_sf   <- st_as_sf(dt_single_cep)
dt_outro_sf <- st_as_sf(dt_cep)

# garantir CRS igual
st_crs(dt_outro_sf) <- st_crs(dt_ref_sf)

setDT(dt_outro_sf)
setDT(dt_ref_sf)

class(dt_outro_sf)

dt_outro_filt <- dt_outro_sf[
  dt_ref_sf,
  on = c("cep_padr" = "cep")
]

dt_outro_filt_sf <- st_as_sf(dt_outro_filt)

dt_outro_filt_sf$dist_m <-
  as.numeric(
    st_distance(
      dt_outro_filt_sf$geometry,
      dt_outro_filt_sf$i.geometry,
      by_element = TRUE
    )
  )
summary(dt_outro_filt_sf$dist_m, na.rm = TRUE)

d <- dt_outro_filt_sf[order(-dt_outro_filt_sf$dist_m), ]

st_crs(dt_outro_filt_sf)
st_is_longlat(dt_outro_filt_sf)
sf::sf_use_s2()

# Para Niteroi a distancia máxima foi de ~400m



dt_filtrado <- dt3[aceito == 0]
t <- head(dt_filtrado, 1000)
View(t)

unique(dt3[aceito == 0, precisao])
dt3[precisao == "numero_aproximado" & classe != 0, .N, by = precisao]
