library(arrow)
library(dplyr)
library(data.table)

# 1) Dataset parquet que você vai "passar adiante" (sem cnpj_completo)
# ajuste para sua pasta final (a que termina com _share_sem_cnpj)
parq_dir <- "D:/Bases/ReceitaFederal/DadosPublicosCNPJ/Sócios - Agosto 2025/out_por_estado_parquet_20260306_094702_masked"


ds <- open_dataset(parq_dir, format = "parquet")

# Só SP + colunas para anexar (join pela chave identificad_m)
ds_sp <- ds %>%
  filter(uf == "SP") %>%
  select(identificad_m, cnpj_completo_m, endereco, numlograd, bairro, cep, uf, cd_uf, municipio_7) %>%
  distinct(identificad_m, .keep_all = TRUE)

# =========================================================
# 2) Seu dt do outro projeto (em memória)
#    - contém identificad_m
#    - pode conter geometria sfc (sf)
# =========================================================
stopifnot("identificad_m" %chin% names(dt_mun))

dt2 <- copy(dt_mun)

# normaliza chave (boa prática)
dt2[, identificad_m := tolower(trimws(as.character(identificad_m)))]
dt2[, row_id := .I]  # chave para recolocar geometria

# detecta colunas sf (sfc)
geo_cols <- names(dt2)[vapply(dt2, inherits, logical(1), what = "sfc")]

# salva geo à parte (se existir)
geo_keep <- if (length(geo_cols) > 0) dt2[, c("row_id", geo_cols), with = FALSE] else NULL

# remove geo para Arrow (Arrow não aceita sfc)
dt2_arrow <- if (length(geo_cols) > 0) dt2[, setdiff(names(dt2), geo_cols), with = FALSE] else dt2

tab_dt <- arrow_table(dt2_arrow)

# =========================================================
# 3) JOIN (left join) por identificad_m
# =========================================================
joined <- tab_dt %>%
  left_join(ds_sp, by = "identificad_m") %>%
  collect()

DT_merged <- as.data.table(joined)

# recoloca geometria preservando a ordem
if (!is.null(geo_keep)) {
  setkey(geo_keep, row_id)
  DT_merged <- geo_keep[DT_merged, on = "row_id"]
}

DT_merged[, row_id := NULL]

# =========================================================
# 4) Teste de match
# =========================================================
DT_merged[, .(
  n_total = .N,
  n_match = sum(!is.na(endereco)),
  match_rate = round(100 * mean(!is.na(endereco)), 2)
)]




library(arrow)
library(dplyr)
library(data.table)

parq_dir <- "D:/Bases/ReceitaFederal/DadosPublicosCNPJ/Sócios - Agosto 2025/out_por_estado_parquet_20260306_094702_masked"
ds <- open_dataset(parq_dir, format = "parquet")

# Lookup BRASIL TODO (sem filtro SP) + colunas para anexar
# (sem tolower/trimws aqui dentro!)
ds_all <- ds %>%
  select(
    identificad_m,
    cnpj_completo_m, endereco, numlograd, bairro, cep, uf, cd_uf, municipio_7
  ) %>%
  rename(
    est_cnpj_completo_m = cnpj_completo_m,
    est_endereco        = endereco,
    est_numlograd       = numlograd,
    est_bairro          = bairro,
    est_cep             = cep,
    est_uf              = uf,
    est_cd_uf           = cd_uf,
    est_municipio_7     = municipio_7
  ) %>%
  distinct(identificad_m, .keep_all = TRUE)

# =========================================================
# 2) Seu dt do outro projeto (em memória) - contém identificad_m
# =========================================================
stopifnot("identificad_m" %chin% names(dt_mun))

dt2 <- copy(dt_mun)
dt2[, identificad_m := tolower(trimws(as.character(identificad_m)))]
dt2[, row_id := .I]

# remove geometria sfc para Arrow
geo_cols <- names(dt2)[vapply(dt2, inherits, logical(1), what = "sfc")]
geo_keep <- if (length(geo_cols) > 0) dt2[, c("row_id", geo_cols), with = FALSE] else NULL
dt2_arrow <- if (length(geo_cols) > 0) dt2[, setdiff(names(dt2), geo_cols), with = FALSE] else dt2

tab_dt <- arrow_table(dt2_arrow)

# =========================================================
# 3) JOIN por identificad_m
# =========================================================
joined <- tab_dt %>%
  left_join(ds_all, by = "identificad_m") %>%
  collect()

DT_merged <- as.data.table(joined)

# recoloca geometria
if (!is.null(geo_keep)) {
  setkey(geo_keep, row_id)
  DT_merged <- geo_keep[DT_merged, on = "row_id"]
}
DT_merged[, row_id := NULL]

# =========================================================
# 4) Teste de match
# =========================================================
DT_merged[, .(
  n_total    = .N,
  n_match    = sum(!is.na(est_endereco)),
  match_rate = round(100 * mean(!is.na(est_endereco)), 4)
)]

# Onde estão os matches (UF do estabelecimento no CNPJ)
DT_merged[!is.na(est_endereco), .N, by = est_uf][order(-N)]
