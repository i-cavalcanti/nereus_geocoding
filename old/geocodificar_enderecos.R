#source("scripts/metrics-functions.R")

geocode_step <- function(dt_in, campos, step_tag,
                         id_key = "id",
                         keep_geometry = FALSE,
                         resultado_completo = FALSE,
                         resolver_empates = TRUE,
                         verboso = FALSE,
                         precisao_col = "tipo_resultado") {

  stopifnot(data.table::is.data.table(dt_in))
  if (!(id_key %in% names(dt_in))) stop("geocode_step: falta '", id_key, "' no input.")

  # ---- garantir que 'id' é uma chave válida
  stopifnot(id_key %in% names(dt_in))
  stopifnot(!anyNA(dt_in[[id_key]]))

  if (data.table::uniqueN(dt_in[[id_key]]) != nrow(dt_in)) {
    stop(sprintf("'%s' não é único (uniqueN(%s) != nrow(dt_in)).", id_key, id_key))
  }

  dt_local <- dt_in

  # remove colunas começando com "." (evita quebra no SQL)
  dot_cols <- grep("^\\.", names(dt_local), value = TRUE)
  if (length(dot_cols) > 0L) dt_local[, (dot_cols) := NULL]

  g <- geocodebr::geocode(
    enderecos          = dt_local,
    campos_endereco    = campos,
    resultado_completo = resultado_completo,
    resolver_empates   = resolver_empates,
    resultado_sf       = isTRUE(keep_geometry),
    verboso            = verboso
  )
  g <- data.table::as.data.table(g)

  if (!(id_key %in% names(g))) stop("geocode_step: '", id_key, "' não veio no retorno do geocode.")
  if (!(precisao_col %in% names(g))) stop("geocode_step: '", precisao_col, "' não veio no retorno do geocode.")

  score_col <- paste0("score_precisao_", step_tag)
  g <- criar_score_precisao(g, precisao_col = precisao_col, score_col = score_col)

  prec_out <- paste0("precisao_", step_tag)
  data.table::setnames(g, precisao_col, prec_out)

  out_cols <- c(id_key, prec_out, score_col)

  if (isTRUE(keep_geometry)) {
    if (!("geometry" %in% names(g))) stop("geocode_step: geometry não veio (resultado_sf=TRUE?).")
    geom_out <- paste0("geometry_", step_tag)
    data.table::setnames(g, "geometry", geom_out)
    out_cols <- c(out_cols, geom_out)
  }

  g[, ..out_cols]
}


add_cols_by_id <- function(dt, dt_1, cols, key = "id") {
  stopifnot(is.data.table(dt), is.data.table(dt_1))
  stopifnot(is.character(cols), length(cols) >= 1)

  if (!key %chin% names(dt))  stop(sprintf("Key '%s' not in dt", key))
  if (!key %chin% names(dt_1)) stop(sprintf("Key '%s' not in dt_1", key))

  missing_cols <- setdiff(cols, names(dt_1))
  if (length(missing_cols)) stop("cols not found in dt_1: ", paste(missing_cols, collapse = ", "))

  # If dt_1 has duplicate ids, take the first match per id (common safe default)
  rhs <- unique(dt_1[, c(key, cols), with = FALSE], by = key)

  # Add/update columns in dt by reference (keeps only one id column)
  dt[rhs, (cols) := mget(paste0("i.", cols)), on = key]

  invisible(dt)
}

geocode_pipeline <- function(dt, campos, campos_pdr
                              #,use_numeric_stopwords = TRUE
                              #,max_digit_edits = 1L
                              ) {
  stopifnot(data.table::is.data.table(dt))
  stopifnot("id" %in% names(dt))
  stopifnot(!anyNA(dt$id))
  stopifnot(data.table::uniqueN(dt$id) == nrow(dt))
  #stopifnot(is.logical(use_numeric_stopwords), length(use_numeric_stopwords) == 1, !is.na(use_numeric_stopwords))
  #stopifnot(is.numeric(max_digit_edits), length(max_digit_edits) == 1, max_digit_edits >= 0)


  #max_digit_edits <- as.integer(max_digit_edits)

  # -----------------------------
  # Helpers
  # -----------------------------
  build_endereco_from_campos <- function(dt_sub, campos,
                                        keys = c("logradouro", "numero", "localidade",
                                                "municipio", "estado", "cep")) {
    if (is.null(campos) || is.null(names(campos))) return(rep(NA_character_, nrow(dt_sub)))

    use_keys <- intersect(keys, names(campos))
    cols <- unname(campos[use_keys])
    cols <- cols[cols %chin% names(dt_sub)]
    if (!length(cols)) return(rep(NA_character_, nrow(dt_sub)))

    parts <- lapply(cols, function(cc) {
      x <- as.character(dt_sub[[cc]])
      x[is.na(x) | !nzchar(trimws(x))] <- NA_character_
      x
    })

    # paste row-wise without introducing "NA"
    rr <- data.table::as.data.table(parts)
    out <- rr[, do.call(paste, c(.SD, sep = ", "))]
    out <- gsub("\\s*,\\s*(NA|NaN|null)\\b", "", out, ignore.case = TRUE)
    out <- gsub("^(\\s*,\\s*)+|(\\s*,\\s*)+$", "", out)
    out[!nzchar(trimws(out))] <- NA_character_
    out
  }

  make_lns <- function(dt_base, mode) {
    logradouro_num_string_fast(
      dt_base,
      endereco_col = "logradouro_padr",
      num_col = "numero_padr",
      complemento_col = "complemento",
      #municipio_col     = "municipio_7",
      #cep_col           = "cep_padr",
      #use_numeric_stopwords = use_numeric_stopwords,
      #max_digit_edits   = max_digit_edits,
      endereco_update_mode = mode
    )
  }

  # roda um step (sem/ com geometry)
  run_step <- function(step_name, dt_step, campos_step, keep_geometry = FALSE) {
    geocode_step(dt_step, campos_step, step_name, keep_geometry = keep_geometry)
  }

  # -----------------------------
  # Base padronizada (uma vez)
  # -----------------------------
  dt_pad <- padronizar_enderecos(dt, campos_do_endereco = campos_pdr)
  if ("numero_padr" %in% names(dt_pad)) dt_pad[numero_padr == "S/N", numero_padr := NA]

  stopifnot(all(c("logradouro_padr","numero_padr","cep_padr","municipio_7") %in% names(dt_pad)))

  campos_pad <- geocodebr::definir_campos(
    logradouro = "logradouro_padr",
    numero     = "numero_padr",
    cep        = "cep_padr",
    localidade = "bairro_padr",
    municipio  = "municipio_padr",
    estado     = "estado_padr"
  )

  campos_lns <- geocodebr::definir_campos(
    logradouro = "endereco_limpo",
    numero     = "numlograd_novo",
    cep        = "cep_padr",
    localidade = "bairro_padr",
    municipio  = "municipio_padr",
    estado     = "estado_padr"
  )

  # -----------------------------
  # Definição funcional dos steps
  # -----------------------------
  steps <- list(
    raw   = list(prep = function() dt,     campos = campos),
    pad   = list(prep = function() dt_pad, campos = campos_pad),
    lns_0 = list(prep = function() make_lns(dt_pad, "no_cut"),               campos = campos_lns),
    lns_1 = list(prep = function() make_lns(dt_pad, "cut_when_missing_num"), campos = campos_lns),
    lns_2 = list(prep = function() make_lns(dt_pad, "always_cut_on_stopword"), campos = campos_lns)
  )

  step_names <- names(steps)

  
  # -----------------------------
  # Passo 1: 5x SEM geometry (wide)
  # -----------------------------
  gs <- lapply(step_names, function(s) {
    st <- steps[[s]]
    dt_step <- st$prep()
    out <- run_step(s, dt_step, st$campos, keep_geometry = FALSE)
    data.table::setkey(out, id)
    rm(dt_step); gc()
    out
  })
  names(gs) <- step_names

  dt_out <- Reduce(function(x, y) y[x], gs)
  rm(gs); gc()

  # -----------------------------
  # best/worst/diff + best_step
  # -----------------------------
  score_cols <- grep("^score_precisao", names(dt_out), value = TRUE)
  stopifnot(length(score_cols) > 0)

  dt_out[, best_score  := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = score_cols]
  dt_out[, worst_score := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = score_cols]
  dt_out[is.infinite(best_score),  best_score  := NA_real_]
  dt_out[is.infinite(worst_score), worst_score := NA_real_]
  dt_out[, diff := worst_score - best_score]

  m <- as.matrix(dt_out[, ..score_cols])
  m2 <- m
  m2[is.na(m2)] <- Inf
  all_na <- rowSums(is.na(m)) == ncol(m)

  idx <- max.col(-m2, ties.method = "last")
  dt_out[, best_step := sub("^score_precisao_", "", score_cols[idx])]
  dt_out[all_na, best_step := NA_character_]

  # -----------------------------
  # precisao_best
  # -----------------------------
  dt_out[, precisao_best := NA_character_]
  for (s in step_names) {
    pc <- paste0("precisao_", s)
    if (pc %in% names(dt_out)) dt_out[best_step == s, precisao_best := get(pc)]
  }

  # -----------------------------
  # endereco_best (do step vencedor)
  # -----------------------------
  dt_out[, endereco_best := NA_character_]
  dt_out[, endereco_fonte := NA_character_]

  # RAW: tenta compor a partir do "campos" (logradouro, numero, etc.)
  ids_raw <- dt_out[best_step == "raw", id]
  if (length(ids_raw)) {
    rhs <- dt[id %in% ids_raw, .(id, endereco_best = build_endereco_from_campos(.SD, campos)),
              .SDcols = names(dt)]
    data.table::setkey(rhs, id)
    dt_out[rhs, endereco_best := i.endereco_best, on = "id"]
    dt_out[best_step == "raw", endereco_fonte := "raw_composto"]
  }

  # PAD: logradouro_padr
  ids_pad <- dt_out[best_step == "pad", id]
  if (length(ids_pad)) {
    rhs <- dt_pad[id %in% ids_pad, .(id, endereco_best = logradouro_padr)]
    data.table::setkey(rhs, id)
    dt_out[rhs, endereco_best := i.endereco_best, on = "id"]
    dt_out[best_step == "pad", endereco_fonte := "logradouro_padr"]
  }

  # LNS: recria apenas para os ids do vencedor e pega endereco_limpo
  fill_lns_addr <- function(step_name, mode) {
    ids <- dt_out[best_step == step_name, id]
    if (!length(ids)) return(invisible(NULL))

    dt_sub <- make_lns(dt_pad[id %in% ids], mode)
    if (!("endereco_limpo" %chin% names(dt_sub))) stop("Não achei 'endereco_limpo' para step ", step_name)

    rhs <- dt_sub[, .(id, endereco_best = endereco_limpo)]
    data.table::setkey(rhs, id)
    dt_out[rhs, endereco_best := i.endereco_best, on = "id"]
    dt_out[best_step == step_name, endereco_fonte := paste0("endereco_limpo:", step_name)]

    rm(dt_sub); gc()
    invisible(NULL)
  }

  fill_lns_addr("lns_0", "no_cut")
  fill_lns_addr("lns_1", "cut_when_missing_num")
  fill_lns_addr("lns_2", "always_cut_on_stopword")

  # -----------------------------
  # Passo 2: 1x COM geometry (por grupos)
  # -----------------------------
  fetch_geom_for_step <- function(step_name) {
    ids <- dt_out[best_step == step_name, id]
    if (!length(ids)) return(NULL)

    st <- steps[[step_name]]
    dt_step <- st$prep()
    dt_step <- dt_step[id %in% ids]

    gg <- run_step(step_name, dt_step, st$campos, keep_geometry = TRUE)
    rm(dt_step); gc()
    gg
  }

  dt_out[, geometry := NULL]

  for (s in step_names) {
    gg <- fetch_geom_for_step(s)
    if (is.null(gg)) next

    data.table::setkey(gg, id)
    geom_col <- paste0("geometry_", s)
    if (!(geom_col %in% names(gg))) stop("Não achei ", geom_col, " no retorno do step ", s)

    data.table::setnames(gg, geom_col, "geom_tmp")
    dt_out[gg, geometry := i.geom_tmp, on = "id"]

    rm(gg); gc()
  }

  # -----------------------------
  # Extras + retorno
  # -----------------------------
  add_cols_by_id(dt_out, dt_pad, key = "id", cols = c("cep_padr"))
  rm(dt_pad); gc()

  dt_out[, c(score_cols, "worst_score") := NULL]

  dt_out[, .(
    id, cep_padr, best_step,
    endereco_best, endereco_fonte,
    precisao_best, best_score, diff,
    geometry
  )]
}

# update 1- added cep
# geocode_pipeline <- function(dt, campos, campos_pdr) {
#   stopifnot(data.table::is.data.table(dt))
#   stopifnot("id" %in% names(dt))
#   stopifnot(!anyNA(dt$id))
#   stopifnot(data.table::uniqueN(dt$id) == nrow(dt))

#   # 1) 5x SEM geometry
#   g_raw <- geocode_step(dt, campos, "raw", keep_geometry = FALSE)

#   dt_pad <- padronizar_enderecos(dt, campos_do_endereco = campos_pdr)
#   if ("numero_padr" %in% names(dt_pad)) dt_pad[numero_padr == "S/N", numero_padr := NA]

#   campos_pad <- geocodebr::definir_campos(
#     logradouro = "logradouro_padr",
#     numero     = "numero_padr",
#     cep        = "cep_padr",
#     localidade = "bairro_padr",
#     municipio  = "municipio_padr",
#     estado     = "estado_padr"
#   )
#   g_pad <- geocode_step(dt_pad, campos_pad, "pad", keep_geometry = FALSE)

#   campos_lns <- geocodebr::definir_campos(
#     logradouro = "endereco_limpo",
#     numero     = "numlograd_novo",
#     cep        = "cep_padr",
#     localidade = "bairro_padr",
#     municipio  = "municipio_padr",
#     estado     = "estado_padr"
#   )

#   dt_lns <- logradouro_num_string_fast(
#     dt_pad,
#     endereco_col = "logradouro_padr",
#     num_col = "numero_padr",
#     complemento_col = "complemento",
#     endereco_update_mode = "no_cut"
#   )
#   g_lns_0 <- geocode_step(dt_lns, campos_lns, "lns_0", keep_geometry = FALSE)
#   rm(dt_lns); gc()

#   dt_lns <- logradouro_num_string_fast(
#     dt_pad,
#     endereco_col = "logradouro_padr",
#     num_col = "numero_padr",
#     complemento_col = "complemento",
#     endereco_update_mode = "cut_when_missing_num"
#   )
#   g_lns_1 <- geocode_step(dt_lns, campos_lns, "lns_1", keep_geometry = FALSE)
#   rm(dt_lns); gc()

#   dt_lns <- logradouro_num_string_fast(
#     dt_pad,
#     endereco_col = "logradouro_padr",
#     num_col = "numero_padr",
#     complemento_col = "complemento",
#     endereco_update_mode = "always_cut_on_stopword"
#   )
#   g_lns_2 <- geocode_step(dt_lns, campos_lns, "lns_2", keep_geometry = FALSE)
#   rm(dt_lns); gc()

#   # merge wide por id
#   data.table::setkey(g_raw, id)
#   data.table::setkey(g_pad, id)
#   data.table::setkey(g_lns_0, id)
#   data.table::setkey(g_lns_1, id)
#   data.table::setkey(g_lns_2, id)

#   dt_out <- Reduce(function(x, y) y[x], list(g_raw, g_pad, g_lns_0, g_lns_1, g_lns_2))
#   rm(g_raw, g_pad, g_lns_0, g_lns_1, g_lns_2); gc()

#   # best/worst/diff + best_step (empate -> última)
#   score_cols <- grep("^score_precisao", names(dt_out), value = TRUE)
#   stopifnot(length(score_cols) > 0)

#   dt_out[, best_score  := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = score_cols]
#   dt_out[, worst_score := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = score_cols]
#   dt_out[is.infinite(best_score),  best_score  := NA_real_]
#   dt_out[is.infinite(worst_score), worst_score := NA_real_]
#   dt_out[, diff := worst_score - best_score]

#   m <- as.matrix(dt_out[, ..score_cols])
#   m2 <- m
#   m2[is.na(m2)] <- Inf
#   all_na <- rowSums(is.na(m)) == ncol(m)

#   idx <- max.col(-m2, ties.method = "last")
#   dt_out[, best_step := sub("^score_precisao_", "", score_cols[idx])]
#   dt_out[all_na, best_step := NA_character_]

#   # ---- precisao_best: pega a precisao_<step vencedor>
#   dt_out[, precisao_best := NA_character_]

#   steps <- c("raw", "pad", "lns_0", "lns_1", "lns_2")
#   for (s in steps) {
#   pc <- paste0("precisao_", s)
#   if (pc %in% names(dt_out)) {
#     dt_out[best_step == s, precisao_best := get(pc)]
#     }
#   }


#   # 2) 1x COM geometry (por grupos do best_step)
#   fetch_geom_for_step <- function(step_name) {
#     ids <- dt_out[best_step == step_name, id]
#     if (length(ids) == 0L) return(NULL)

#     if (step_name == "raw") {
#       dt_sub <- dt[id %in% ids]
#       campos_use <- campos

#     } else if (step_name == "pad") {
#       dt_sub <- dt_pad[id %in% ids]
#       campos_use <- campos_pad

#     } else if (step_name %in% c("lns_0", "lns_1", "lns_2")) {
#       mode <- switch(step_name,
#         lns_0 = "no_cut",
#         lns_1 = "cut_when_missing_num",
#         lns_2 = "always_cut_on_stopword"
#       )

#       dt_sub <- dt_pad[id %in% ids]
#       dt_sub <- logradouro_num_string_fast(
#         dt_sub,
#         endereco_col = "logradouro_padr",
#         num_col = "numero_padr",
#         complemento_col = "complemento",
#         endereco_update_mode = mode
#       )
#       campos_use <- campos_lns

#     } else {
#       stop("Step desconhecido: ", step_name)
#     }

#     geocode_step(dt_sub, campos_use, step_name, keep_geometry = TRUE)
#   }

#   dt_out[, geometry := NULL]

#   for (s in c("raw", "pad", "lns_0", "lns_1", "lns_2")) {
#     gg <- fetch_geom_for_step(s)
#     if (is.null(gg)) next

#     data.table::setkey(gg, id)
#     geom_col <- paste0("geometry_", s)
#     if (!(geom_col %in% names(gg))) stop("Não achei ", geom_col, " no retorno do step ", s)

#     data.table::setnames(gg, geom_col, "geom_tmp")
#     dt_out[gg, geometry := i.geom_tmp, on = "id"]

#     rm(gg); gc()
#   }
#   print(names(dt_out))
#   print(names(dt_pad))
#   add_cols_by_id(dt_out, dt_pad, key="id", cols=c("cep_padr"))
  
#   rm(dt_pad); gc()

# #   # devolver só o essencial
# #   dt_out[, c(score_cols, "worst_score") := NULL]
# #   dt_out[, .(id, best_step, best_score, diff, geometry)]

#   # devolver só o essencial
#   dt_out[, c(score_cols, "worst_score") := NULL]
#   dt_res <- dt_out[, .(id, cep_padr, best_step, precisao_best, best_score, diff, geometry)]
#   return(dt_res)

# }
  # Raw - cep no address line 
  # geocode_pipeline <- function(dt, campos, campos_pdr) {
  #   stopifnot(data.table::is.data.table(dt))
  #   stopifnot("id" %in% names(dt))
  #   stopifnot(!anyNA(dt$id))
  #   stopifnot(data.table::uniqueN(dt$id) == nrow(dt))

  #   # 1) 5x SEM geometry
  #   g_raw <- geocode_step(dt, campos, "raw", keep_geometry = FALSE)

  #   dt_pad <- padronizar_enderecos(dt, campos_do_endereco = campos_pdr)
  #   if ("numero_padr" %in% names(dt_pad)) dt_pad[numero_padr == "S/N", numero_padr := NA]

  #   campos_pad <- geocodebr::definir_campos(
  #     logradouro = "logradouro_padr",
  #     numero     = "numero_padr",
  #     cep        = "cep_padr",
  #     localidade = "bairro_padr",
  #     municipio  = "municipio_padr",
  #     estado     = "estado_padr"
  #   )
  #   g_pad <- geocode_step(dt_pad, campos_pad, "pad", keep_geometry = FALSE)

  #   campos_lns <- geocodebr::definir_campos(
  #     logradouro = "endereco_limpo",
  #     numero     = "numlograd_novo",
  #     cep        = "cep_padr",
  #     localidade = "bairro_padr",
  #     municipio  = "municipio_padr",
  #     estado     = "estado_padr"
  #   )

  #   dt_lns <- logradouro_num_string_fast(
  #     dt_pad,
  #     endereco_col = "logradouro_padr",
  #     num_col = "numero_padr",
  #     complemento_col = "complemento",
  #     endereco_update_mode = "no_cut"
  #   )
  #   g_lns_0 <- geocode_step(dt_lns, campos_lns, "lns_0", keep_geometry = FALSE)
  #   rm(dt_lns); gc()

  #   dt_lns <- logradouro_num_string_fast(
  #     dt_pad,
  #     endereco_col = "logradouro_padr",
  #     num_col = "numero_padr",
  #     complemento_col = "complemento",
  #     endereco_update_mode = "cut_when_missing_num"
  #   )
  #   g_lns_1 <- geocode_step(dt_lns, campos_lns, "lns_1", keep_geometry = FALSE)
  #   rm(dt_lns); gc()

  #   dt_lns <- logradouro_num_string_fast(
  #     dt_pad,
  #     endereco_col = "logradouro_padr",
  #     num_col = "numero_padr",
  #     complemento_col = "complemento",
  #     endereco_update_mode = "always_cut_on_stopword"
  #   )
  #   g_lns_2 <- geocode_step(dt_lns, campos_lns, "lns_2", keep_geometry = FALSE)
  #   rm(dt_lns); gc()

  #   # merge wide por id
  #   data.table::setkey(g_raw, id)
  #   data.table::setkey(g_pad, id)
  #   data.table::setkey(g_lns_0, id)
  #   data.table::setkey(g_lns_1, id)
  #   data.table::setkey(g_lns_2, id)

  #   dt_out <- Reduce(function(x, y) y[x], list(g_raw, g_pad, g_lns_0, g_lns_1, g_lns_2))
  #   rm(g_raw, g_pad, g_lns_0, g_lns_1, g_lns_2); gc()

  #   # best/worst/diff + best_step (empate -> última)
  #   score_cols <- grep("^score_precisao", names(dt_out), value = TRUE)
  #   stopifnot(length(score_cols) > 0)

  #   dt_out[, best_score  := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = score_cols]
  #   dt_out[, worst_score := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = score_cols]
  #   dt_out[is.infinite(best_score),  best_score  := NA_real_]
  #   dt_out[is.infinite(worst_score), worst_score := NA_real_]
  #   dt_out[, diff := worst_score - best_score]

  #   m <- as.matrix(dt_out[, ..score_cols])
  #   m2 <- m
  #   m2[is.na(m2)] <- Inf
  #   all_na <- rowSums(is.na(m)) == ncol(m)

  #   idx <- max.col(-m2, ties.method = "last")
  #   dt_out[, best_step := sub("^score_precisao_", "", score_cols[idx])]
  #   dt_out[all_na, best_step := NA_character_]

  #   # ---- precisao_best: pega a precisao_<step vencedor>
  #   dt_out[, precisao_best := NA_character_]

  #   steps <- c("raw", "pad", "lns_0", "lns_1", "lns_2")
  #   for (s in steps) {
  #   pc <- paste0("precisao_", s)
  #   if (pc %in% names(dt_out)) {
  #     dt_out[best_step == s, precisao_best := get(pc)]
  #     }
  #   }

  #   # 2) 1x COM geometry (por grupos do best_step)
  #   fetch_geom_for_step <- function(step_name) {
  #     ids <- dt_out[best_step == step_name, id]
  #     if (length(ids) == 0L) return(NULL)

  #     if (step_name == "raw") {
  #       dt_sub <- dt[id %in% ids]
  #       campos_use <- campos

  #     } else if (step_name == "pad") {
  #       dt_sub <- dt_pad[id %in% ids]
  #       campos_use <- campos_pad

  #     } else if (step_name %in% c("lns_0", "lns_1", "lns_2")) {
  #       mode <- switch(step_name,
  #         lns_0 = "no_cut",
  #         lns_1 = "cut_when_missing_num",
  #         lns_2 = "always_cut_on_stopword"
  #       )

  #       dt_sub <- dt_pad[id %in% ids]
  #       dt_sub <- logradouro_num_string_fast(
  #         dt_sub,
  #         endereco_col = "logradouro_padr",
  #         num_col = "numero_padr",
  #         complemento_col = "complemento",
  #         endereco_update_mode = mode
  #       )
  #       campos_use <- campos_lns

  #     } else {
  #       stop("Step desconhecido: ", step_name)
  #     }

  #     geocode_step(dt_sub, campos_use, step_name, keep_geometry = TRUE)
  #   }

  #   dt_out[, geometry := NULL]

  #   for (s in c("raw", "pad", "lns_0", "lns_1", "lns_2")) {
  #     gg <- fetch_geom_for_step(s)
  #     if (is.null(gg)) next

  #     data.table::setkey(gg, id)
  #     geom_col <- paste0("geometry_", s)
  #     if (!(geom_col %in% names(gg))) stop("Não achei ", geom_col, " no retorno do step ", s)

  #     data.table::setnames(gg, geom_col, "geom_tmp")
  #     dt_out[gg, geometry := i.geom_tmp, on = "id"]

  #     rm(gg); gc()
  #   }

  #   rm(dt_pad); gc()

  # #   # devolver só o essencial
  # #   dt_out[, c(score_cols, "worst_score") := NULL]
  # #   dt_out[, .(id, best_step, best_score, diff, geometry)]

  #   # devolver só o essencial
  #   dt_out[, c(score_cols, "worst_score") := NULL]
  #   dt_out[, .(id, best_step, precisao_best, best_score, diff, geometry)]

  # }
