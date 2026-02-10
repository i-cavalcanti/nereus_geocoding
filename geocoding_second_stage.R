source("scripts/general-functions.R")
source("scripts/metrics-functions.R")

required_packages <- c("stringdist")

check_and_install_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)


filtrar_ids_estoque_zero <- function(
  dt,
  id_col,
  estoque_col
) {
  stopifnot(data.table::is.data.table(dt))
  stopifnot(id_col %in% names(dt))
  stopifnot(estoque_col %in% names(dt))
  
  # Identifica IDs cujo estoque é sempre zero (ou NA)
  ids_estoque_zero <- dt[
    ,
    .(estoque_todo_zero = all(fifelse(
      is.na(get(estoque_col)), 
      TRUE, 
      get(estoque_col) == 0
    ))),
    by = id_col
  ][
    estoque_todo_zero == TRUE,
    get(id_col)
  ]
  
  # Filtra o data.table original
  dt_filtrado <- dt[!get(id_col) %in% ids_estoque_zero]
  
  dt_filtrado
}


# criar_score_precisao <- function(
#   dt,
#   precisao_col,
#   score_col = "score_precisao"
# ) {
  
#   stopifnot(
#     data.table::is.data.table(dt),
#     is.character(precisao_col),
#     length(precisao_col) == 1L,
#     precisao_col %in% names(dt)
#   )
  
#   # Ordem definida (mais preciso -> menos preciso)
#   ordem_precisao <- c(
#     "dn01","dn02","dn03","dn04",
#     "pn01","pn02","pn03","pn04",
#     "da01","da02","da03","da04",
#     "pa01","pa02","pa03","pa04",
#     "dl01","dl02","dl03","dl04",
#     "pl01","pl02","pl03","pl04",
#     "dc01","dc02",
#     "db01",
#     "dm01"
#   )
  
#   mapa_score <- data.table::data.table(
#     precisao = ordem_precisao,
#     score    = seq_along(ordem_precisao)
#   )
  
#   # join eficiente
#   dt[
#     mapa_score,
#     (score_col) := i.score,
#     on = setNames("precisao", precisao_col)
#   ]
  
#   # alertar valores não mapeados
#   if (any(is.na(dt[[score_col]]) & !is.na(dt[[precisao_col]]))) {
#     warning("Existem valores de precisão não mapeados para score.")
#   }
  
#   dt[]
# }

string_diff <- function(
  x,
  y,
  method = "lv"
) {
  
  # Casos triviais
  if (is.na(x) || is.na(y)) return(NA_real_)
  if (x == y) return(0)
  
  # Distância bruta
  d <- stringdist::stringdist(
    x,
    y,
    method = method
  )
  
  # Normalização (0–1)
  d / max(nchar(x), nchar(y))
}

imputar_endereco_geometry_por_score_fast <- function(
  dt,
  id_col,
  endereco_col,
  score_col,
  municipio_col = "municipio",
  matrizfilial_col = "matrizfilial",
  geometry_col = "geometry",
  validated_col = "aceito",
  endereco_updated_col = "endereco_updated",
  geometry_updated_col = "geometry_updated",
  score_updated_col = "score_precisao_updated",
  validated_updated_col = "aceito_updated",
  diff_threshold = 0.2,
  geo_threshold_m = 1000,   # 1 km em metros (se CRS for projetado em metros)
  copy_dt = TRUE,
  string_method = "lv",
  verbose = TRUE
) {
  stopifnot(
    data.table::is.data.table(dt),
    is.character(id_col), length(id_col) == 1L, id_col %in% names(dt),
    is.character(endereco_col), length(endereco_col) == 1L, endereco_col %in% names(dt),
    is.character(score_col), length(score_col) == 1L, score_col %in% names(dt),
    is.character(municipio_col), length(municipio_col) == 1L, municipio_col %in% names(dt),
    is.character(matrizfilial_col), length(matrizfilial_col) == 1L, matrizfilial_col %in% names(dt),
    is.character(geometry_col), length(geometry_col) == 1L, geometry_col %in% names(dt),
    is.character(validated_col), length(validated_col) == 1L, validated_col %in% names(dt),
    is.numeric(diff_threshold), length(diff_threshold) == 1L,
    is.numeric(geo_threshold_m), length(geo_threshold_m) == 1L
  )

  if (!requireNamespace("sf", quietly = TRUE)) stop("Pacote 'sf' não instalado.")
  if (!requireNamespace("stringdist", quietly = TRUE)) stop("Pacote 'stringdist' não instalado.")

  log <- function(...) if (isTRUE(verbose)) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))
  t_all <- Sys.time()

  log("Início: n=", nrow(dt), " | ids=", data.table::uniqueN(dt[[id_col]]))

  # 0) copy
  t0 <- Sys.time()
  if (isTRUE(copy_dt)) dt <- data.table::copy(dt)
  log("Etapa 0 (copy_dt=", copy_dt, ") concluída em ", round(as.numeric(difftime(Sys.time(), t0, "secs")), 2), "s")

  # checagem básica de geometry
  g0 <- dt[[geometry_col]]
  if (!inherits(g0, "sfc")) {
    stop("A coluna '", geometry_col, "' precisa ser do tipo sf/sfc (POINT). Atualmente: ", paste(class(g0), collapse = ", "))
  }

  # 1) inicialização (NUNCA deixa imputada/aceito_updated NA)
  t0 <- Sys.time()
  dt[, imputada := 0L]
  dt[, diff := NA_real_]
  dt[, dist_m := NA_real_]

  dt[, (endereco_updated_col) := get(endereco_col)]
  dt[, (geometry_updated_col) := get(geometry_col)]
  dt[, (score_updated_col) := get(score_col)]
  dt[, (validated_updated_col) := get(validated_col)]

  # garante integer 0/1 em aceito_updated quando aceito for 0/1
  if (is.integer(dt[[validated_col]]) || is.numeric(dt[[validated_col]])) {
    dt[, (validated_updated_col) := as.integer(get(validated_col))]
  }

  log("Etapa 1 (init cols) em ", round(as.numeric(difftime(Sys.time(), t0, "secs")), 2), "s")

  # 2) ordenar + referência por id (1 linha por id)
  t0 <- Sys.time()
  data.table::setorderv(dt, cols = c(id_col, score_col), order = c(1, 1), na.last = TRUE)

  ref <- dt[
    ,
    .SD[1],
    by = id_col,
    .SDcols = c(endereco_col, score_col, geometry_col, validated_col, municipio_col, matrizfilial_col)
  ]

  data.table::setnames(
    ref,
    old = c(endereco_col, score_col, geometry_col, validated_col, municipio_col, matrizfilial_col),
    new = c("ref_endereco", "ref_score", "ref_geom", "ref_validated", "ref_municipio", "ref_matrizfilial")
  )
  log("Etapa 2 (sort + ref por id) em ", round(as.numeric(difftime(Sys.time(), t0, "secs")), 2), "s | ref_ids=", nrow(ref))

  # 3) join ref -> dt
  t0 <- Sys.time()
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
  log("Etapa 3 (join ref) em ", round(as.numeric(difftime(Sys.time(), t0, "secs")), 2), "s")

  # 4) pré-filtro barato (candidatos)
  # OBS: do jeito que está, se ref_matrizfilial for NA, NUNCA entra como candidato.
  # (se você quiser permitir NA==NA, precisa mudar essa regra)
  t0 <- Sys.time()
  cand_idx <- dt[
    !is.na(ref_municipio) & !is.na(get(municipio_col)) & get(municipio_col) == ref_municipio &
      !is.na(ref_matrizfilial) & !is.na(get(matrizfilial_col)) & get(matrizfilial_col) == ref_matrizfilial &
      !is.na(ref_score) & !is.na(get(score_col)) & get(score_col) > ref_score,
    which = TRUE
  ]
  log("Etapa 4 (candidatos) em ", round(as.numeric(difftime(Sys.time(), t0, "secs")), 2),
      "s | cand=", length(cand_idx))

  if (length(cand_idx) == 0L) {
    dt[, c("ref_endereco","ref_score","ref_geom","ref_validated","ref_municipio","ref_matrizfilial") := NULL]
    log("Sem candidatos. Fim em ", round(as.numeric(difftime(Sys.time(), t_all, "secs")), 2), "s")
    return(dt[])
  }

  # 5) stringdist vetorizado (pairwise)
  t0 <- Sys.time()
  ref_addr <- dt$ref_endereco[cand_idx]
  x_addr   <- dt[[endereco_col]][cand_idx]

  ok_str <- !is.na(ref_addr) & !is.na(x_addr)

  diff_vec <- rep(NA_real_, length(cand_idx))
  if (any(ok_str)) {
    d <- stringdist::stringdist(ref_addr[ok_str], x_addr[ok_str], method = string_method)
    denom <- pmax(nchar(ref_addr[ok_str]), nchar(x_addr[ok_str]))
    diff_vec[ok_str] <- d / denom
  }

  cond_string <- !is.na(diff_vec) & diff_vec <= diff_threshold
  log("Etapa 5 (stringdist) em ", round(as.numeric(difftime(Sys.time(), t0, "secs")), 2),
      "s | passou_string=", sum(cond_string, na.rm = TRUE))

  # 6) distância geométrica só onde string falhou (sempre by_element=TRUE)
  t0 <- Sys.time()
  need_geo_local <- which(!cond_string)
  dist_vec <- rep(NA_real_, length(cand_idx))
  n_geo_calc <- 0L

  if (length(need_geo_local) > 0L) {
    idx2 <- cand_idx[need_geo_local]
    g1 <- dt[[geometry_col]][idx2]
    g2 <- dt$ref_geom[idx2]

    # para sfc, is.na funciona, mas geometria EMPTY pode não virar NA.
    # aqui tratamos NA; se houver EMPTY, st_distance pode dar NA/0 dependendo do caso.
    ok_geom <- !is.na(g1) & !is.na(g2)

    if (any(ok_geom)) {
      d <- sf::st_distance(g1[ok_geom], g2[ok_geom], by_element = TRUE)
      dist_vec[need_geo_local[ok_geom]] <- as.numeric(d)
      n_geo_calc <- sum(ok_geom)
    }
  }

  cond_geo <- !is.na(dist_vec) & dist_vec <= geo_threshold_m
  log("Etapa 6 (distância geo) em ", round(as.numeric(difftime(Sys.time(), t0, "secs")), 2),
      "s | geo_calculado=", n_geo_calc, " | passou_geo=", sum(cond_geo, na.rm = TRUE))

  # 7) aplicar imputação (separando fontes) + logs consistentes
  t0 <- Sys.time()

  imputar_string <- cond_string
  imputar_geo    <- (!cond_string) & cond_geo
  imputar        <- imputar_string | imputar_geo

  n_imputar <- sum(imputar, na.rm = TRUE)
  n_imp_string <- sum(imputar_string, na.rm = TRUE)
  n_imp_geo <- sum(imputar_geo, na.rm = TRUE)

  if (n_imputar > 0L) {
    idx_upd <- cand_idx[imputar]

    dt[idx_upd, imputada := 1L]
    dt[idx_upd, (endereco_updated_col) := ref_endereco]
    dt[idx_upd, (geometry_updated_col) := ref_geom]
    dt[idx_upd, (score_updated_col) := ref_score]
    dt[idx_upd, (validated_updated_col) := ref_validated]

    # diff só para string
    if (n_imp_string > 0L) {
      idx_s <- cand_idx[imputar_string]
      data.table::set(dt, idx_s, "diff", diff_vec[imputar_string])
    }

    # dist_m só para geo
    if (n_imp_geo > 0L) {
      idx_g <- cand_idx[imputar_geo]
      data.table::set(dt, idx_g, "dist_m", dist_vec[imputar_geo])
    }
  }

  log("Etapa 7 (update) em ", round(as.numeric(difftime(Sys.time(), t0, "secs")), 2),
      "s | imputadas=", n_imputar,
      " | por_string=", n_imp_string,
      " | por_geo=", n_imp_geo)

  # 8) limpeza
  t0 <- Sys.time()
  dt[, c("ref_endereco","ref_score","ref_geom","ref_validated","ref_municipio","ref_matrizfilial") := NULL]
  log("Etapa 8 (cleanup) em ", round(as.numeric(difftime(Sys.time(), t0, "secs")), 2), "s")

  log("Fim: imputadas=", dt[imputada == 1L, .N],
      " | tempo_total=", round(as.numeric(difftime(Sys.time(), t_all, "secs")), 2), "s")

  dt[]
}


calcular_stats_imputacao <- function(
  dt,
  imputada_col = "imputada",
  estoque_col  = "estoque"
) {
  
  stopifnot(
    data.table::is.data.table(dt),
    imputada_col %in% names(dt),
    estoque_col  %in% names(dt)
  )
  
  dt[
    ,
    .(
      n_linhas      = .N,
      estoque_total = sum(get(estoque_col), na.rm = TRUE)
    ),
    by = imputada_col
  ][
    ,
    `:=`(
      prop_linhas  = n_linhas / sum(n_linhas),
      perc_linhas  = 100 * n_linhas / sum(n_linhas),
      prop_estoque = estoque_total / sum(estoque_total),
      perc_estoque = 100 * estoque_total / sum(estoque_total)
    )
  ]
}


main_multiyear <- function(
  dt_first_stage,
  id_col          = "identificad_m",
  endereco_col    = "endereco_best",
  diff_threshold  = 0.2,
  geo_threshold_m = 1000,
  verbose         = TRUE
) {

  log <- function(...) {
    if (isTRUE(verbose)) {
      message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))
    }
  }

  log("Iniciando main_multiyear()")
  log("id_col: ", id_col, " | endereco_col: ", endereco_col)

  stopifnot(
    data.table::is.data.table(dt_first_stage),
    id_col        %in% names(dt_first_stage),
    endereco_col  %in% names(dt_first_stage),
    "geometry"        %in% names(dt_first_stage),
    "precisao_best"   %in% names(dt_first_stage),
    "estoque"         %in% names(dt_first_stage),
    is.numeric(diff_threshold),
    length(diff_threshold) == 1L,
    is.numeric(geo_threshold_m),
    length(geo_threshold_m) == 1L
  )

  # ----------------------------
  # 0) REDUZIR COLUNAS (antes de tudo)
  # ----------------------------
  needed_cols <- unique(c(
    id_col,
    endereco_col,
    "geometry",
    "precisao_best",
    "estoque",
    "municipio",
    "matrizfilial",
    "aceito",
    "year",
    "municipio_7",
    "id"        # usado para carregar ref_validated
  ))

  missing_needed <- setdiff(needed_cols, names(dt_first_stage))
  if (length(missing_needed) > 0L) {
    stop("Faltam colunas necessárias no dt_first_stage: ", paste(missing_needed, collapse = ", "))
  }

  cols_before <- names(dt_first_stage)
  dt <- dt_first_stage[, ..needed_cols]
  log("Etapa 0 (seleção colunas): ", length(cols_before), " -> ", length(names(dt)), " colunas")
  log("Mantidas: ", paste(names(dt), collapse = ", "))

  dropped <- setdiff(cols_before, names(dt))
  if (length(dropped) > 0L) log("Removidas: ", paste(dropped, collapse = ", "))

  # ----------------------------
  # 1) Criar score de precisão
  # ----------------------------
  log("Etapa 1: Criando score de precisão (score_precisao)")
  dt <- criar_score_precisao(
    dt,
    precisao_col = "precisao_best",
    score_col    = "score_precisao"
  )
  log("Score criado. Linhas: ", nrow(dt))

  # ----------------------------
  # 2) Imputar endereço e geometria (FAST)
  # ----------------------------
  log("Etapa 2: Iniciando imputação (string + distância)")
  dt <- imputar_endereco_geometry_por_score_fast(
    dt,
    id_col               = id_col,
    endereco_col         = endereco_col,
    score_col            = "score_precisao",
    municipio_col        = "municipio",
    matrizfilial_col     = "matrizfilial",
    geometry_col         = "geometry",
    validated_col        = "aceito",
    geometry_updated_col = "geometry_updated",
    score_updated_col    = "score_precisao_updated",
    diff_threshold       = diff_threshold,
    geo_threshold_m      = geo_threshold_m,
    copy_dt              = FALSE,
    verbose              = verbose
  )

  n_imputadas <- dt[imputada == 1L, .N]
  log("Imputação concluída. Registros imputados: ", n_imputadas)

  # ----------------------------
  # 3) Calcular stats de imputação
  # ----------------------------
  log("Etapa 3: Calculando estatísticas de imputação")

  tab_aceito_updated <- dt[
    , .(N = .N),
    by = aceito_updated
  ][
    , `:=`(
      prop = N / sum(N),
      perc = 100 * N / sum(N)
    )
  ][order(aceito_updated)]

  print(tab_aceito_updated)

  tab_estoque_aceito_updated <- dt[
    , .(estoque_total = sum(estoque, na.rm = TRUE)),
    by = aceito_updated
  ][
    , `:=`(
      prop_estoque = estoque_total / sum(estoque_total),
      perc_estoque = 100 * estoque_total / sum(estoque_total)
    )
  ][order(aceito_updated)]

  print(tab_estoque_aceito_updated)

  stats <- calcular_stats_imputacao(
    dt,
    imputada_col = "imputada",
    estoque_col  = "estoque"
  )
  log("Estatísticas calculadas")

  # ----------------------------
  # 4) Filtrar IDs com estoque zero
  # ----------------------------
  log("Etapa 4: Filtrando IDs com estoque total igual a zero")
  n_ids_before <- data.table::uniqueN(dt[[id_col]])

  dt <- filtrar_ids_estoque_zero(
    dt          = dt,
    id_col      = id_col,
    estoque_col = "estoque"
  )

  n_ids_after <- data.table::uniqueN(dt[[id_col]])
  log("Filtro concluído. IDs removidos: ", n_ids_before - n_ids_after)

  # --- NOVO: manter apenas geometry_updated (remover geometry)
  if ("geometry_updated" %in% names(dt)) {
    dt[, geometry := NULL]
    # opcional: garantir que geometry_updated é sfc
    stopifnot(inherits(dt[["geometry_updated"]], "sfc"))
  } else {
    stop("Coluna 'geometry_updated' não existe no dt final.")
  }

  log("Processamento finalizado com sucesso")

  list(
    dt_final = dt,
    stats    = stats
  )
}

# main_multiyear <- function(
#   dt_first_stage,
#   id_col         = "identificad_m",
#   endereco_col   = "endereco_limpo",
#   diff_threshold = 0.2,
#   geo_threshold_m = 1000,
#   verbose        = TRUE
# ) {

#   log <- function(...) {
#     if (isTRUE(verbose)) {
#       message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))
#     }
#   }

#   log("Iniciando main_multiyear()")
#   log("id_col: ", id_col, " | endereco_col: ", endereco_col)

#   stopifnot(
#     data.table::is.data.table(dt_first_stage),
#     id_col       %in% names(dt_first_stage),
#     endereco_col %in% names(dt_first_stage),
#     "geometry"        %in% names(dt_first_stage),
#     "tipo_resultado"  %in% names(dt_first_stage),
#     "estoque"         %in% names(dt_first_stage),
#     is.numeric(diff_threshold),
#     length(diff_threshold) == 1L,
#     is.numeric(geo_threshold_m),
#     length(geo_threshold_m) == 1L
#   )

#   # ----------------------------
#   # 0) REDUZIR COLUNAS (antes de tudo)
#   # ----------------------------
#   needed_cols <- unique(c(
#     id_col,
#     endereco_col,
#     "geometry",
#     "tipo_resultado",
#     "estoque",
#     "municipio",
#     "matrizfilial",
#     "aceito" ,
#     "year",
#     "municipio_7",
#     "id"        # usado para carregar ref_validated
#   ))

#   missing_needed <- setdiff(needed_cols, names(dt_first_stage))
#   if (length(missing_needed) > 0L) {
#     stop("Faltam colunas necessárias no dt_first_stage: ", paste(missing_needed, collapse = ", "))
#   }

#   cols_before <- names(dt_first_stage)
#   dt <- dt_first_stage[, ..needed_cols]   # mantém só o necessário (sem cópia profunda de geometry)
#   log("Etapa 0 (seleção colunas): ", length(cols_before), " -> ", length(names(dt)), " colunas")
#   log("Mantidas: ", paste(names(dt), collapse = ", "))

#   dropped <- setdiff(cols_before, names(dt))
#   if (length(dropped) > 0L) log("Removidas: ", paste(dropped, collapse = ", "))

#   # ----------------------------
#   # 1) Criar score de precisão
#   # ----------------------------
#   log("Etapa 1: Criando score de precisão (score_precisao)")
#   dt <- criar_score_precisao(
#     dt,
#     precisao_col = "tipo_resultado",
#     score_col    = "score_precisao"
#   )
#   log("Score criado. Linhas: ", nrow(dt))

#   # ----------------------------
#   # 2) Imputar endereço e geometria (FAST)
#   # ----------------------------
#   log("Etapa 2: Iniciando imputação (string + distância)")
#   dt <- imputar_endereco_geometry_por_score_fast(
#     dt,
#     id_col               = id_col,
#     endereco_col         = endereco_col,
#     score_col            = "score_precisao",
#     municipio_col        = "municipio",
#     matrizfilial_col     = "matrizfilial",
#     geometry_col         = "geometry",
#     validated_col        = "aceito",
#     geometry_updated_col = "geometry_updated",
#     score_updated_col    = "score_precisao_updated",
#     diff_threshold       = diff_threshold,
#     geo_threshold_m      = geo_threshold_m,
#     copy_dt              = FALSE,     # evita duplicar base grande (já estamos em dt reduzido)
#     verbose              = verbose
#   )

#   n_imputadas <- dt[imputada == 1L, .N]
#   log("Imputação concluída. Registros imputados: ", n_imputadas)

#   # ----------------------------
#   # 3) Calcular stats de imputação
#   # ----------------------------
#   log("Etapa 3: Calculando estatísticas de imputação")

#   tab_aceito_updated <- dt[
#   ,
#   .(N = .N),
#   by = aceito_updated
#   ][
#     ,
#     `:=`(
#       prop = N / sum(N),
#       perc = 100 * N / sum(N)
#     )
#   ][order(aceito_updated)]

#   print(tab_aceito_updated)

#   tab_estoque_aceito_updated <- dt[
#   ,
#   .(estoque_total = sum(estoque, na.rm = TRUE)),
#   by = aceito_updated
#   ][
#   ,
#   `:=`(
#     prop_estoque = estoque_total / sum(estoque_total),
#     perc_estoque = 100 * estoque_total / sum(estoque_total)
#   )
#   ][order(aceito_updated)]

#   print(tab_estoque_aceito_updated)



#   stats <- calcular_stats_imputacao(
#     dt,
#     imputada_col = "imputada",
#     estoque_col  = "estoque"
#   )
#   log("Estatísticas calculadas")

#   # ----------------------------
#   # 4) Filtrar IDs com estoque zero
#   # ----------------------------
#   log("Etapa 4: Filtrando IDs com estoque total igual a zero")
#   n_ids_before <- data.table::uniqueN(dt[[id_col]])

#   dt <- filtrar_ids_estoque_zero(
#     dt          = dt,
#     id_col      = id_col,
#     estoque_col = "estoque"
#   )

#   n_ids_after <- data.table::uniqueN(dt[[id_col]])
#   log("Filtro concluído. IDs removidos: ", n_ids_before - n_ids_after)

#   log("Processamento finalizado com sucesso")

#   list(
#     dt_final = dt,
#     stats    = stats
#   )
# }




