source("scripts/general-functions.R")
source("scripts/data-standardization.R")
source("logradouro_num_string.R")
source("geocoding_first_stage.R")
source("geocoding_second_stage.R")


required_packages <- c("enderecobr","geocodebr", "data.table", "sf")

check_and_install_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)


# ---- parâmetros de teste ----
pathname_in  <- "D:/Arq-Azzoni/RAIS/rais-geocoding/data"   # ajuste
year         <- 2007
encoding     <- "Latin-1"
filename_prefix <- "sp"

# ---- montar caminho e ler ----
filename <- paste0(filename_prefix, "_estb_", year, "_limpo")
file_in  <- file.path(pathname_in, "raw", "rais_temp", paste0(filename, ".csv"))

dt <- fread(file_in, encoding = encoding)

cat("Linhas lidas:", nrow(dt), "\n")
cat("Colunas:", ncol(dt), "\n")
print(names(dt))

t<-head(dt, 1000)

dt[, municipio_7 := ibge6_to_7(municipio)]
dt <- add_sigla_from_uf(dt, "cduf", "uf_dom")
dt <- change_cep_99999999_to_na(dt, "cep", "municipio")

if (!("bairro" %in% names(dt)))    dt[, bairro := NA_character_]
if (!("numlograd" %in% names(dt))) dt[, numlograd := NA]

campos <- correspondencia_campos(
      logradouro = "endereco",
      numero     = "numlograd",
      cep        = "cep",
      bairro     = "bairro",
      municipio  = "municipio_7",
      estado     = "uf_dom"
    )

dt <- padronizar_enderecos(
    dt,
    campos_do_endereco = campos
  )
  

dt[numero_padr == "S/N", numero_padr := NA]

dt2 <- logradouro_num_string_fast(
    dt,
    endereco_col = "logradouro_padr",
    num_col = "numero_padr",
    complemento_col = "complemento",
    endereco_update_mode ="cut_when_missing_num"
  )

t<-head(dt2, 1000)


campos <- geocodebr::definir_campos(
    logradouro  = "endereco_limpo",
    numero      = "numlograd_novo",
    cep         = "cep_padr",
    localidade  = "bairro_padr",
    municipio   = "municipio_padr",
    estado      = "estado_padr"
  )
  

dt3 <- geocodebr::geocode(
enderecos            = dt2,
campos_endereco      = campos,
resultado_completo   = FALSE,
resolver_empates     = TRUE,
resultado_sf         = TRUE,
verboso              = FALSE
)


t<-head(dt3, 1000)

setDT(dt3)  # garante que é data.table (mesmo se era data.frame)

tab_precisao <- dt3[, .(N = .N), by = precisao][order(-N)]
tab_precisao[, prop := N / sum(N)]
tab_precisao

classe_col <- "classe"
aceito_col <- "aceito"

dt3[, (classe_col) := data.table::fifelse(
precisao %in% c("numero", "numero_aproximado", "logradouro"), 0L,
data.table::fifelse(precisao == "cep", 1L, 2L)
)]

dt_cep <- dt3[get(classe_col) == 1L]

res <- filtrar_ceps_consistentes(
dt_ceps         = dt_cep,
sd_threshold_km = 0.3
)

ceps_aceitos <- res$ceps_aceitos


 
dt3 <- filtrar_enderecos_aceitos(
dt           = dt3,
ceps_aceitos = ceps_aceitos,
classe_col   = classe_col,
aceito_col   = aceito_col
)


tab_aceito <- dt3[, .(N = .N), by = aceito][order(-N)]
tab_aceito[, prop := N / sum(N)]
tab_aceito[, perc := round(100 * prop, 2)]

tab_aceito



#######################################
montar_rds_paths_da_pasta <- function(
  rds_dir,
  pattern = "^dt_f_(\\d{4})\\.rds$"
) {
  stopifnot(dir.exists(rds_dir))
  files <- list.files(rds_dir, pattern = "\\.rds$", full.names = TRUE)

  ok  <- grepl(pattern, basename(files))
  files <- files[ok]
  yrs <- sub(pattern, "\\1", basename(files))

  # lista nomeada por ano
  out <- as.list(files)
  names(out) <- yrs
  out
}


main_second_stage_geocoding <- function(
  pathname_out_prev,
  out_dir_csv,
  rds_subdir = "geocoded_rds",
  rds_pattern = "^dt_f_(\\d{4})\\.rds$",
  diff_threshold = 0.3,
  year_col = "year",
  keep_cols = NULL,
  # padrão do nome do CSV final por ano
  filename_prefix = "sp",
  out_suffix = "_limpo_corr.csv",
  geo_threshold_m = 1000,
  overwrite_csv = FALSE,
  verbose = TRUE
) {
  log <- function(...) if (isTRUE(verbose)) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))
  dir.create(out_dir_csv, recursive = TRUE, showWarnings = FALSE)

  rds_dir <- file.path(pathname_out_prev, rds_subdir)
  if (!dir.exists(rds_dir)) stop("Pasta RDS não encontrada: ", rds_dir)

  rds_paths <- montar_rds_paths_da_pasta(rds_dir, pattern = rds_pattern)
  if (length(rds_paths) == 0L) stop("Nenhum RDS encontrado em ", rds_dir, " com padrão ", rds_pattern)

  years <- sort(as.integer(names(rds_paths)))

  # opcional: limpar outputs antigos
  if (isTRUE(overwrite_csv)) {
    for (yy in years) {
      f <- file.path(out_dir_csv, paste0(filename_prefix, "_estb_", yy, out_suffix))
      if (file.exists(f)) unlink(f)
    }
  }

  log("Lendo e empilhando anos: ", paste(years, collapse = ", "))

  partes <- vector("list", length(rds_paths))
  k <- 0L

  for (y in names(rds_paths)) {
    p <- rds_paths[[y]]
    if (is.null(p) || !nzchar(p) || !file.exists(p)) next

    log("Lendo ano ", y, ": ", p)
    dt_year <- readRDS(p)

    # reduz colunas para RAM (opcional)
    if (!is.null(keep_cols)) {
      keep <- intersect(keep_cols, names(dt_year))
      dt_year <- dt_year[, ..keep]
    }

    # garante coluna year
    if (!(year_col %in% names(dt_year))) {
      dt_year[, (year_col) := as.integer(y)]
    } else {
      dt_year[, (year_col) := as.integer(get(year_col))]
    }

    k <- k + 1L
    partes[[k]] <- dt_year
    rm(dt_year); gc()
  }

  if (k == 0L) stop("Nenhum RDS válido foi lido.")

  dt_all <- rbindlist(partes[seq_len(k)], use.names = TRUE, fill = TRUE)
  rm(partes); gc()

  log("dt_all montado. Linhas: ", nrow(dt_all), " | Colunas: ", ncol(dt_all))
  return(dt_all)
}

dt_all <- main_second_stage_geocoding(
  pathname_out_prev = "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding",
  out_dir_csv       = "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/csv_por_ano",
  diff_threshold = 0.2,
  geo_threshold_m = 1000,
  filename_prefix = "sp",
  out_suffix = "_limpo_corr.csv",
  overwrite_csv = TRUE,
  verbose = TRUE
)
names(dt_all)

dt_all[, sort(unique(year))]
t <- head(dt_all, 1000)


id_col         = "identificad_m"
endereco_col   = "endereco_limpo"
score_col      = "score_precisao"
municipio_col        = "municipio"
matrizfilial_col     = "matrizfilial"
geometry_col         = "geometry"
validated_col        = "aceito"


needed_cols <- unique(c(
    id_col,
    endereco_col,
    "geometry",
    "tipo_resultado",
    "estoque",
    "municipio",
    "matrizfilial",
    "aceito" ,
    "year",
    "municipio_7",
    "id"        # usado para carregar ref_validated
  ))

dt <- dt_all[, ..needed_cols]   # mantém só o necessário (sem cópia profunda de geometry)
  
criar_score_precisao <- function(
  dt,
  precisao_col,
  score_col = "score_precisao"
) {
  
  stopifnot(
    data.table::is.data.table(dt),
    is.character(precisao_col),
    length(precisao_col) == 1L,
    precisao_col %in% names(dt)
  )
  
  # Ordem definida (mais preciso -> menos preciso)
  ordem_precisao <- c(
    "dn01","dn02","dn03","dn04",
    "pn01","pn02","pn03","pn04",
    "da01","da02","da03","da04",
    "pa01","pa02","pa03","pa04",
    "dl01","dl02","dl03","dl04",
    "pl01","pl02","pl03","pl04",
    "dc01","dc02",
    "db01",
    "dm01"
  )
  
  mapa_score <- data.table::data.table(
    precisao = ordem_precisao,
    score    = seq_along(ordem_precisao)
  )
  
  # join eficiente
  dt[
    mapa_score,
    (score_col) := i.score,
    on = setNames("precisao", precisao_col)
  ]
  
  # alertar valores não mapeados
  if (any(is.na(dt[[score_col]]) & !is.na(dt[[precisao_col]]))) {
    warning("Existem valores de precisão não mapeados para score.")
  }
  
  dt[]
}

dt <- criar_score_precisao(
    dt,
    precisao_col = "tipo_resultado",
    score_col    = "score_precisao"
  )

t <- head(dt, 1000)
dt[, .N, by = year][order(year)]
names(dt)

na_perc <- function(dt) {
  stopifnot(is.data.table(dt))
  data.table(
    col = names(dt),
    na_perc = 100 * vapply(dt, function(x) mean(is.na(x)), numeric(1))
  )[order(-na_perc)]
}

na_perc(dt)

#################

dt2 <- imputar_endereco_geometry_por_score_fast(
  dt,
  id_col = "identificad_m",
  endereco_col = "endereco_limpo",
  score_col = "score_precisao",
  municipio_col = "municipio",
  matrizfilial_col = "matrizfilial",
  geometry_col = "geometry",
  validated_col = "aceito",
  diff_threshold = 0.3,
  geo_threshold_m = 2000,
  verbose = TRUE
)
names(dt_all)

dt3 <- filtrar_ids_estoque_zero(
    dt          = dt2,
    id_col      = "identificad_m",
    estoque_col = "estoque"
  )


t <- head(dt2, 1000)

dt2[, .(
  N = .N,
  aceito_updated_na = sum(is.na(aceito_updated)),
  aceito_updated_notna = sum(!is.na(aceito_updated))
)]


tab_aceito_updated <- dt3[
  ,
  .(N = .N),
  by = aceito_updated
][
  ,
  `:=`(
    prop = N / sum(N),
    perc = 100 * N / sum(N)
  )
][order(aceito_updated)]

tab_aceito_updated

tab_aceito_updated_year <- dt3[
  ,
  .(N = .N),
  by = .(year, aceito_updated)
][
  ,
  `:=`(
    prop = N / sum(N),
    perc = 100 * N / sum(N)
  ),
  by = year
][order(year, aceito_updated)]

tab_aceito_updated_year

tab_estoque_aceito_updated <- dt3[
  ,
  .(estoque_total = sum(estoque, na.rm = TRUE)),
  by = aceito_updated
][
  ,
  `:=`(
    prop_estoque = estoque_total / sum(estoque_total),
    perc_estoque = 100 * estoque_total / sum(estoque_total)
  )
][order(aceito_updated)]

tab_estoque_aceito_updated

#################
dt <- readRDS("D:\\Arq-Azzoni\\UrbanSprawl\\Bases_dados\\RAIS_estab\\temp_geocoding\\geocoded_rds\\dt_f_2002.rds")
t<-head(dt, 1000)

#################

 data.table::setorderv(dt, cols = c(id_col, score_col), order = c(1, 1), na.last = TRUE)

ref <- dt[
    ,
    .SD[1],
    by = id_col,
    .SDcols = c(endereco_col, score_col, geometry_col, validated_col, municipio_col, matrizfilial_col, "year")
  ]


tr <- head(ref, 1000)

data.table::setnames(
    ref,
    old = c(endereco_col, score_col, geometry_col, validated_col, municipio_col, matrizfilial_col),
    new = c("ref_endereco", "ref_score", "ref_geom", "ref_validated", "ref_municipio", "ref_matrizfilial")
  )

dt[
    ref,
    `:=`(
      ref_endereco     = i.ref_endereco,
      ref_score        = i.ref_score,
      ref_geom         = i.ref_geom,
      ref_validated    = i.ref_validated,
      ref_municipio    = i.ref_municipio,
      ref_matrizfilial = i.ref_matrizfilial
    ),
    on = id_col
  ]

cand_idx <- dt[
    !is.na(ref_municipio) & !is.na(get(municipio_col)) & get(municipio_col) == ref_municipio &
      !is.na(ref_matrizfilial) & !is.na(get(matrizfilial_col)) & get(matrizfilial_col) == ref_matrizfilial &
      !is.na(ref_score) & !is.na(get(score_col)) & get(score_col) > ref_score,
    which = TRUE
  ]


  if (length(cand_idx) == 0L) {
    dt[, c("ref_endereco","ref_score","ref_geom","ref_validated","ref_municipio","ref_matrizfilial") := NULL]
    log("Sem candidatos. Fim em ", round(as.numeric(difftime(Sys.time(), t_all, "secs")), 2), "s")
    return(dt[])
  }

ref_addr <- dt$ref_endereco[cand_idx]
x_addr   <- dt[[endereco_col]][cand_idx]

ok_str <- !is.na(ref_addr) & !is.na(x_addr)

diffs <- rep(NA_real_, length(cand_idx))
if (any(ok_str)) {
  d <- stringdist::stringdist(ref_addr[ok_str], x_addr[ok_str], method = "lv")
  denom <- pmax(nchar(ref_addr[ok_str]), nchar(x_addr[ok_str]))
  diffs[ok_str] <- d / denom
}

cond_string <- !is.na(diffs) & diffs <= 0.2

need_geo_local <- which(!cond_string)
dist_m <- rep(NA_real_, length(cand_idx))
n_geo_calc <- 0L
if (length(need_geo_local) > 0L) {
  idx2 <- cand_idx[need_geo_local]

  g_crs <- sf::st_crs(dt[[geometry_col]])
  is_longlat <- isTRUE(sf::st_is_longlat(g_crs))

  g1 <- dt[[geometry_col]][idx2]
  g2 <- dt$ref_geom[idx2]

  ok_geom <- !is.na(g1) & !is.na(g2)

  if (any(ok_geom)) {
    if (!is_longlat) {
      c1 <- sf::st_coordinates(g1[ok_geom])
      c2 <- sf::st_coordinates(g2[ok_geom])
      dist_m_tmp <- sqrt((c1[,1] - c2[,1])^2 + (c1[,2] - c2[,2])^2)
    } else {
      sf::sf_use_s2(TRUE)
      dist_m_tmp <- as.numeric(sf::st_distance(g1[ok_geom], g2[ok_geom], by_element = TRUE))
    }

    dist_m[need_geo_local[ok_geom]] <- dist_m_tmp
    n_geo_calc <- sum(ok_geom)
  }
}

cond_geo <- !is.na(dist_m) & dist_m <= 1000

imputar <- cond_string | cond_geo
n_imputar <- sum(imputar, na.rm = TRUE)

endereco_updated_col = "endereco_updated"
geometry_updated_col = "geometry_updated"
score_updated_col = "score_precisao_updated"
validated_updated_col = "aceito_updated"


if (n_imputar > 0L) {
  idx_upd <- cand_idx[imputar]

  dt[idx_upd, imputada := 1L]
  dt[idx_upd, diff := diffs[imputar]]
  dt[idx_upd, dist_m := dist_m[imputar]]

  dt[idx_upd, (endereco_updated_col) := ref_endereco]
  dt[idx_upd, (geometry_updated_col) := ref_geom]
  dt[idx_upd, (score_updated_col) := ref_score]
  dt[idx_upd, (validated_updated_col) := ref_validated]
}

t <- head(dt3, 1000)


#################################

csv_dir <- "D:\\Arq-Azzoni\\UrbanSprawl\\Bases_dados\\RAIS_estab\\temp_geocoding\\csv_por_ano"
pattern <- "^sp_estb_(\\d{4})_limpo_corr.*\\.csv$"   # ajusta se precisar

# Extrai year do nome do arquivo
get_year <- function(fn) {
  y <- sub(pattern, "\\1", fn)
  y <- suppressWarnings(as.integer(y))
  if (is.na(y)) NA_integer_ else y
}

files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
files <- files[grepl(pattern, basename(files))]

stopifnot(length(files) > 0)

res <- rbindlist(lapply(files, function(f) {
  bn <- basename(f)
  year <- get_year(bn)

  # lê só o identificador (bem mais rápido)
  dt <- fread(f, select = "identificad_m")

  data.table(
    file = bn,
    year = year,
    n_ids_unicos = uniqueN(dt$identificad_m)
  )
}), use.names = TRUE, fill = TRUE)

setorder(res, year, file)

# imprime por arquivo
print(res)

# média total (média de n_ids_unicos entre arquivos)
media_total <- res[, mean(n_ids_unicos, na.rm = TRUE)]
cat("\nMédia total de identificad_m únicos (entre arquivos): ", round(media_total, 2), "\n", sep = "")


rds_dir <- "D:\\Arq-Azzoni\\UrbanSprawl\\Bases_dados\\RAIS_estab\\temp_geocoding\\geocoded_rds"
pattern <- "^dt_f_(\\d{4})\\.rds$"

get_year <- function(fn) {
  y <- sub(pattern, "\\1", fn)
  y <- suppressWarnings(as.integer(y))
  if (is.na(y)) NA_integer_ else y
}

files <- list.files(rds_dir, pattern = "\\.rds$", full.names = TRUE)
files <- files[grepl(pattern, basename(files))]

stopifnot(length(files) > 0)

res <- rbindlist(lapply(files, function(f) {
  bn <- basename(f)
  year <- get_year(bn)

  dt <- readRDS(f)
  setDT(dt)

  # pega a coluna (sem copiar muito)
  if (!("identificad_m" %in% names(dt))) stop("Arquivo sem identificad_m: ", bn)

  data.table(
    file = bn,
    year = year,
    n_ids_unicos = uniqueN(dt$identificad_m)
  )
}), use.names = TRUE, fill = TRUE)

setorder(res, year, file)

print(res)

media_total <- res[, mean(n_ids_unicos, na.rm = TRUE)]
cat("\nMédia total de identificad_m únicos (entre arquivos): ", round(media_total, 2), "\n", sep = "")





######################


t <- head(dt2, 1000)
View(t)

tab_perc_aceito <- function(dt, col = "aceito") {
  stopifnot(is.data.table(dt), col %in% names(dt))

  dt[
    ,
    .(N = .N),
    by = .(valor = get(col))
  ][
    ,
    `:=`(
      prop = N / sum(N),
      perc = 100 * N / sum(N)
    )
  ][order(valor)]
}

# uso
tab_perc_aceito(dt2, "imputada")


tab_perc_estoque_por_aceito <- function(dt, col_aceito = "aceito", col_estoque = "estoque") {
  stopifnot(is.data.table(dt), col_aceito %in% names(dt), col_estoque %in% names(dt))

  dt[
    ,
    .(estoque_total = sum(get(col_estoque), na.rm = TRUE)),
    by = .(aceito = get(col_aceito))
  ][
    ,
    `:=`(
      prop_estoque = estoque_total / sum(estoque_total),
      perc_estoque = 100 * estoque_total / sum(estoque_total)
    )
  ][order(aceito)]
}

# uso
tab_perc_estoque_por_aceito(dt2, "aceito", "estoque")



pct <- dt3[
  imputada == 1L,
  100 * mean(aceito == 0L & aceito_updated == 1L, na.rm = TRUE)
]

pct


dt3[
  imputada == 1L,
  .(
    N_imputadas = .N,
    N_flip_0_to_1 = sum(aceito == 0L & aceito_updated == 1L, na.rm = TRUE),
    perc_flip_0_to_1 = 100 * mean(aceito == 0L & aceito_updated == 1L, na.rm = TRUE)
  )
]


dt3[, .(
  N_total   = .N,
  N_na      = sum(is.na(diff)),
  N_not_na  = sum(!is.na(diff)),
  perc_na   = 100 * mean(is.na(diff)),
  perc_not  = 100 * mean(!is.na(diff))
)]

dt3[, .(imputadas = sum(imputada == 1L), total = .N)]


dt3[imputada == 1L, .(
  N = .N,
  dist_m_not_na = sum(!is.na(dist_m)),
  diff_not_na   = sum(!is.na(dist_m))
)]

dt3[imputada == 1L & is.na(diff),
    .(geom_na = sum(is.na(geometry)),
      geom_ref_na = sum(is.na(geometry_updated))),  # ou ref_geom se você ainda tiver no meio do processo
    ]


dt2[imputada == 1L, .(
  N            = .N,
  dist_m_not_na= sum(!is.na(dist_m)),
  diff_not_na  = sum(!is.na(diff)),
  ambos_not_na = sum(!is.na(dist_m) & !is.na(diff)),
  ambos_na     = sum(is.na(dist_m) & is.na(diff))
)]

dt2[imputada==1L, .(
  por_geo    = 100*mean(!is.na(dist_m)),
  por_string = 100*mean(!is.na(diff))
)]

dt2[imputada==1L & !is.na(dist_m), .(
  n = .N,
  p50 = quantile(dist_m, 0.50),
  p90 = quantile(dist_m, 0.90),
  p99 = quantile(dist_m, 0.99),
  max = max(dist_m)
)]

sf::st_crs(dt2$geometry)
sf::st_is_longlat(dt2$geometry)

dt2[, .(
  N_total    = .N,
  N_imputada = sum(imputada == 1L),
  perc_imputada = 100 * mean(imputada == 1L)
)]

tab_aceito_updated <- dt3[
  ,
  .(N = .N),
  by = .(aceito_updated)
][
  ,
  `:=`(
    prop = N / sum(N),
    perc = 100 * N / sum(N)
  )
][order(aceito_updated)]

tab_aceito_updated

tab_aceito_updated_estoque <- dt3[
  ,
  .(estoque_total = sum(estoque, na.rm = TRUE)),
  by = .(aceito_updated)
][
  ,
  `:=`(
    prop_estoque = estoque_total / sum(estoque_total),
    perc_estoque = 100 * estoque_total / sum(estoque_total)
  )
][order(aceito_updated)]

tab_aceito_updated_estoque


dt2[aceito_updated == 1L & aceito != 1L, .(N = .N)]
dt2[aceito_updated == 1L & aceito != 1L][1:20]

dt2[imputada==1L & is.na(diff), .(N=.N, dist_m_not_na=sum(!is.na(dist_m)), min_dist=min(dist_m, na.rm=TRUE))]


dt3[, .(
  N_total        = .N,
  N_0_to_1       = sum(aceito == 0L & aceito_updated == 1L, na.rm = TRUE),
  perc_0_to_1    = 100 * mean(aceito == 0L & aceito_updated == 1L, na.rm = TRUE)
)]

dt3[imputada == 1L, .(
  N_imputadas     = .N,
  N_0_to_1        = sum(aceito == 0L & aceito_updated == 1L, na.rm = TRUE),
  perc_0_to_1_imp = 100 * mean(aceito == 0L & aceito_updated == 1L, na.rm = TRUE)
)]





x <- dt2[imputada==1L & is.na(diff)][1:200]  # pega uma amostra (até 200)
d <- st_distance(x$geometry, x$geometry_updated, by_element = TRUE)
summary(as.numeric(d))

dt2[imputada==1L, sum(!is.na(dist_m))]
dt2[imputada==1L & is.na(diff), .(N=.N, dist_m_not_na=sum(!is.na(dist_m)))]


dt3[, .(
  N_total        = .N,
  N_0_to_1       = sum(aceito == 0L & aceito_updated == 1L, na.rm = TRUE),
  perc_0_to_1    = 100 * mean(aceito == 0L & aceito_updated == 1L, na.rm = TRUE)
)]


t <- head(dt3, 1000)


stats_id <- dt[, .(
  n_linhas    = .N,
  n_score_na  = sum(is.na(score_precisao)),
  mean_score  = mean(score_precisao, na.rm = TRUE),
  var_score   = if (.N - sum(is.na(score_precisao)) >= 2L)
                  var(score_precisao, na.rm = TRUE)
                else
                  NA_real_,
  sd_score    = if (.N - sum(is.na(score_precisao)) >= 2L)
                  sd(score_precisao, na.rm = TRUE)
                else
                  NA_real_
), by = identificad_m]

stats_id

stats_id[, .(
  n_ids = .N,
  mean_da_media = mean(mean_score, na.rm = TRUE),
  p50_media = quantile(mean_score, 0.50, na.rm = TRUE),
  mean_da_var = mean(var_score, na.rm = TRUE),
  p50_var = quantile(var_score, 0.50, na.rm = TRUE)
)]

p95 <- stats_id[, quantile(var_score, 0.95, na.rm = TRUE)]
p99 <- stats_id[, quantile(var_score, 0.99, na.rm = TRUE)]

# % acima do P95 e P99
stats_id[, .(
  p95 = p95,
  perc_acima_p95 = 100 * mean(var_score > p95, na.rm = TRUE),
  p99 = p99,
  perc_acima_p99 = 100 * mean(var_score > p99, na.rm = TRUE)
)]
