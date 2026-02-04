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


geocode_pipeline <- function(dt, campos, campos_pdr) {
  stopifnot(data.table::is.data.table(dt))
  stopifnot("id" %in% names(dt))
  stopifnot(!anyNA(dt$id))
  stopifnot(data.table::uniqueN(dt$id) == nrow(dt))

  # 1) 5x SEM geometry
  g_raw <- geocode_step(dt, campos, "raw", keep_geometry = FALSE)

  dt_pad <- padronizar_enderecos(dt, campos_do_endereco = campos_pdr)
  if ("numero_padr" %in% names(dt_pad)) dt_pad[numero_padr == "S/N", numero_padr := NA]

  campos_pad <- geocodebr::definir_campos(
    logradouro = "logradouro_padr",
    numero     = "numero_padr",
    cep        = "cep_padr",
    localidade = "bairro_padr",
    municipio  = "municipio_padr",
    estado     = "estado_padr"
  )
  g_pad <- geocode_step(dt_pad, campos_pad, "pad", keep_geometry = FALSE)

  campos_lns <- geocodebr::definir_campos(
    logradouro = "endereco_limpo",
    numero     = "numlograd_novo",
    cep        = "cep_padr",
    localidade = "bairro_padr",
    municipio  = "municipio_padr",
    estado     = "estado_padr"
  )

  dt_lns <- logradouro_num_string_fast(
    dt_pad,
    endereco_col = "logradouro_padr",
    num_col = "numero_padr",
    complemento_col = "complemento",
    endereco_update_mode = "no_cut"
  )
  g_lns_0 <- geocode_step(dt_lns, campos_lns, "lns_0", keep_geometry = FALSE)
  rm(dt_lns); gc()

  dt_lns <- logradouro_num_string_fast(
    dt_pad,
    endereco_col = "logradouro_padr",
    num_col = "numero_padr",
    complemento_col = "complemento",
    endereco_update_mode = "cut_when_missing_num"
  )
  g_lns_1 <- geocode_step(dt_lns, campos_lns, "lns_1", keep_geometry = FALSE)
  rm(dt_lns); gc()

  dt_lns <- logradouro_num_string_fast(
    dt_pad,
    endereco_col = "logradouro_padr",
    num_col = "numero_padr",
    complemento_col = "complemento",
    endereco_update_mode = "always_cut_on_stopword"
  )
  g_lns_2 <- geocode_step(dt_lns, campos_lns, "lns_2", keep_geometry = FALSE)
  rm(dt_lns); gc()

  # merge wide por id
  data.table::setkey(g_raw, id)
  data.table::setkey(g_pad, id)
  data.table::setkey(g_lns_0, id)
  data.table::setkey(g_lns_1, id)
  data.table::setkey(g_lns_2, id)

  dt_out <- Reduce(function(x, y) y[x], list(g_raw, g_pad, g_lns_0, g_lns_1, g_lns_2))
  rm(g_raw, g_pad, g_lns_0, g_lns_1, g_lns_2); gc()

  # best/worst/diff + best_step (empate -> última)
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

  # ---- precisao_best: pega a precisao_<step vencedor>
  dt_out[, precisao_best := NA_character_]

  steps <- c("raw", "pad", "lns_0", "lns_1", "lns_2")
  for (s in steps) {
  pc <- paste0("precisao_", s)
  if (pc %in% names(dt_out)) {
    dt_out[best_step == s, precisao_best := get(pc)]
    }
  }

  # 2) 1x COM geometry (por grupos do best_step)
  fetch_geom_for_step <- function(step_name) {
    ids <- dt_out[best_step == step_name, id]
    if (length(ids) == 0L) return(NULL)

    if (step_name == "raw") {
      dt_sub <- dt[id %in% ids]
      campos_use <- campos

    } else if (step_name == "pad") {
      dt_sub <- dt_pad[id %in% ids]
      campos_use <- campos_pad

    } else if (step_name %in% c("lns_0", "lns_1", "lns_2")) {
      mode <- switch(step_name,
        lns_0 = "no_cut",
        lns_1 = "cut_when_missing_num",
        lns_2 = "always_cut_on_stopword"
      )

      dt_sub <- dt_pad[id %in% ids]
      dt_sub <- logradouro_num_string_fast(
        dt_sub,
        endereco_col = "logradouro_padr",
        num_col = "numero_padr",
        complemento_col = "complemento",
        endereco_update_mode = mode
      )
      campos_use <- campos_lns

    } else {
      stop("Step desconhecido: ", step_name)
    }

    geocode_step(dt_sub, campos_use, step_name, keep_geometry = TRUE)
  }

  dt_out[, geometry := NULL]

  for (s in c("raw", "pad", "lns_0", "lns_1", "lns_2")) {
    gg <- fetch_geom_for_step(s)
    if (is.null(gg)) next

    data.table::setkey(gg, id)
    geom_col <- paste0("geometry_", s)
    if (!(geom_col %in% names(gg))) stop("Não achei ", geom_col, " no retorno do step ", s)

    data.table::setnames(gg, geom_col, "geom_tmp")
    dt_out[gg, geometry := i.geom_tmp, on = "id"]

    rm(gg); gc()
  }

  rm(dt_pad); gc()

#   # devolver só o essencial
#   dt_out[, c(score_cols, "worst_score") := NULL]
#   dt_out[, .(id, best_step, best_score, diff, geometry)]

  # devolver só o essencial
  dt_out[, c(score_cols, "worst_score") := NULL]
  dt_out[, .(id, best_step, precisao_best, best_score, diff, geometry)]

}
