geocode_step <- function(
  dt_in,
  campos,
  step_tag,
  id_key = "id",
  keep_geometry = FALSE,
  resultado_completo = FALSE,
  resolver_empates = TRUE,
  verboso = FALSE,
  precisao_col = "tipo_resultado"
) {
  stopifnot(data.table::is.data.table(dt_in))
  require_columns(dt_in, id_key, "dt_in")
  if (anyNA(dt_in[[id_key]])) stop("Missing id values in geocode input.", call. = FALSE)
  if (data.table::uniqueN(dt_in[[id_key]]) != nrow(dt_in)) stop("id_key must uniquely identify rows.", call. = FALSE)

  dt_local <- data.table::copy(dt_in)
  dot_cols <- grep("^\\.", names(dt_local), value = TRUE)
  if (length(dot_cols) > 0L) dt_local[, (dot_cols) := NULL]

  g <- geocodebr::geocode(
    enderecos = dt_local,
    campos_endereco = campos,
    resultado_completo = resultado_completo,
    resolver_empates = resolver_empates,
    resultado_sf = isTRUE(keep_geometry),
    verboso = verboso
  )
  g <- data.table::as.data.table(g)

  require_columns(g, c(id_key, precisao_col), "geocodebr result")
  g <- criar_score_precisao(g, precisao_col = precisao_col, score_col = paste0("score_precisao_", step_tag))

  precision_out <- paste0("precisao_", step_tag)
  data.table::setnames(g, precisao_col, precision_out)
  out_cols <- c(id_key, precision_out, paste0("score_precisao_", step_tag))

  if (isTRUE(keep_geometry)) {
    require_columns(g, "geometry", "geocodebr result")
    geometry_out <- paste0("geometry_", step_tag)
    data.table::setnames(g, "geometry", geometry_out)
    out_cols <- c(out_cols, geometry_out)
  }

  g[, ..out_cols]
}

add_cols_by_id <- function(dt, rhs, cols, key = "id") {
  stopifnot(data.table::is.data.table(dt), data.table::is.data.table(rhs))
  require_columns(dt, key, "dt")
  require_columns(rhs, c(key, cols), "rhs")

  rhs_unique <- unique(rhs[, c(key, cols), with = FALSE], by = key)
  dt[rhs_unique, (cols) := mget(paste0("i.", cols)), on = key]
  invisible(dt)
}

build_endereco_from_campos <- function(dt_sub, campos, keys = c("logradouro", "numero", "localidade", "municipio", "estado", "cep")) {
  if (is.null(campos) || is.null(names(campos))) return(rep(NA_character_, nrow(dt_sub)))

  cols <- unname(campos[intersect(keys, names(campos))])
  cols <- cols[cols %in% names(dt_sub)]
  if (length(cols) == 0L) return(rep(NA_character_, nrow(dt_sub)))

  parts <- lapply(cols, function(col) {
    x <- as.character(dt_sub[[col]])
    x[is.na(x) | !nzchar(trimws(x))] <- NA_character_
    x
  })

  rr <- data.table::as.data.table(parts)
  out <- rr[, do.call(paste, c(.SD, sep = ", "))]
  out <- gsub("\\s*,\\s*(NA|NaN|null)\\b", "", out, ignore.case = TRUE)
  out <- gsub("^(\\s*,\\s*)+|(\\s*,\\s*)+$", "", out)
  out[!nzchar(trimws(out))] <- NA_character_
  out
}

geocode_pipeline <- function(dt, campos, campos_pdr, id_key = "id") {
  stopifnot(data.table::is.data.table(dt))
  require_columns(dt, id_key, "dt")
  if (anyNA(dt[[id_key]])) stop("Missing id values.", call. = FALSE)
  if (data.table::uniqueN(dt[[id_key]]) != nrow(dt)) stop("id_key must uniquely identify rows.", call. = FALSE)

  dt_pad <- enderecobr::padronizar_enderecos(dt, campos_do_endereco = campos_pdr)
  if ("numero_padr" %in% names(dt_pad)) dt_pad[numero_padr == "S/N", numero_padr := NA]
  require_columns(dt_pad, c("logradouro_padr", "numero_padr", "cep_padr", "bairro_padr", "municipio_padr", "estado_padr"), "standardized data")

  campos_pad <- geocodebr::definir_campos(
    logradouro = "logradouro_padr",
    numero = "numero_padr",
    cep = "cep_padr",
    localidade = "bairro_padr",
    municipio = "municipio_padr",
    estado = "estado_padr"
  )

  campos_lns <- geocodebr::definir_campos(
    logradouro = "endereco_limpo",
    numero = "numlograd_novo",
    cep = "cep_padr",
    localidade = "bairro_padr",
    municipio = "municipio_padr",
    estado = "estado_padr"
  )

  make_lns <- function(dt_base, mode) {
    logradouro_num_string_fast(
      data.table::copy(dt_base),
      endereco_col = "logradouro_padr",
      num_col = "numero_padr",
      endereco_update_mode = mode
    )
  }

  steps <- list(
    raw = list(prep = function() dt, campos = campos),
    pad = list(prep = function() dt_pad, campos = campos_pad),
    lns_0 = list(prep = function() make_lns(dt_pad, "no_cut"), campos = campos_lns),
    lns_1 = list(prep = function() make_lns(dt_pad, "cut_when_missing_num"), campos = campos_lns),
    lns_2 = list(prep = function() make_lns(dt_pad, "always_cut_on_stopword"), campos = campos_lns)
  )

  step_names <- names(steps)
  geocoded <- lapply(step_names, function(step_name) {
    step <- steps[[step_name]]
    out <- geocode_step(step$prep(), step$campos, step_name, id_key = id_key, keep_geometry = FALSE)
    data.table::setkeyv(out, id_key)
    out
  })
  names(geocoded) <- step_names

  dt_out <- Reduce(function(x, y) merge(x, y, by = id_key, all = TRUE), geocoded)
  score_cols <- paste0("score_precisao_", step_names)
  require_columns(dt_out, score_cols, "geocoding summary")

  dt_out[, best_score := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = score_cols]
  dt_out[is.infinite(best_score), best_score := NA_real_]

  scores <- as.matrix(dt_out[, ..score_cols])
  scores_for_rank <- scores
  scores_for_rank[is.na(scores_for_rank)] <- Inf
  all_na <- rowSums(is.na(scores)) == ncol(scores)
  best_idx <- max.col(-scores_for_rank, ties.method = "last")

  dt_out[, best_step := step_names[best_idx]]
  dt_out[all_na, best_step := NA_character_]
  dt_out[, precisao_best := NA_character_]

  for (step_name in step_names) {
    precision_col <- paste0("precisao_", step_name)
    dt_out[best_step == step_name, precisao_best := get(precision_col)]
  }

  dt_out[, endereco_best := NA_character_]
  dt_out[, endereco_fonte := NA_character_]

  raw_ids <- dt_out[best_step == "raw", get(id_key)]
  if (length(raw_ids) > 0L) {
    rhs <- dt[get(id_key) %in% raw_ids, .(id_tmp = get(id_key), endereco_best = build_endereco_from_campos(.SD, campos)), .SDcols = names(dt)]
    data.table::setnames(rhs, "id_tmp", id_key)
    dt_out[rhs, endereco_best := i.endereco_best, on = id_key]
    dt_out[best_step == "raw", endereco_fonte := "raw_composto"]
  }

  pad_ids <- dt_out[best_step == "pad", get(id_key)]
  if (length(pad_ids) > 0L) {
    rhs <- dt_pad[get(id_key) %in% pad_ids, .(id_tmp = get(id_key), endereco_best = logradouro_padr)]
    data.table::setnames(rhs, "id_tmp", id_key)
    dt_out[rhs, endereco_best := i.endereco_best, on = id_key]
    dt_out[best_step == "pad", endereco_fonte := "logradouro_padr"]
  }

  fill_lns_address <- function(step_name, mode) {
    ids <- dt_out[best_step == step_name, get(id_key)]
    if (length(ids) == 0L) return(invisible(NULL))

    rhs <- make_lns(dt_pad[get(id_key) %in% ids], mode)[, .(id_tmp = get(id_key), endereco_best = endereco_limpo)]
    data.table::setnames(rhs, "id_tmp", id_key)
    dt_out[rhs, endereco_best := i.endereco_best, on = id_key]
    dt_out[best_step == step_name, endereco_fonte := paste0("endereco_limpo:", step_name)]
    invisible(NULL)
  }

  fill_lns_address("lns_0", "no_cut")
  fill_lns_address("lns_1", "cut_when_missing_num")
  fill_lns_address("lns_2", "always_cut_on_stopword")

  dt_out[, geometry := NULL]
  for (step_name in step_names) {
    ids <- dt_out[best_step == step_name, get(id_key)]
    if (length(ids) == 0L) next

    step <- steps[[step_name]]
    dt_step <- step$prep()[get(id_key) %in% ids]
    geometry_step <- geocode_step(dt_step, step$campos, step_name, id_key = id_key, keep_geometry = TRUE)
    geometry_col <- paste0("geometry_", step_name)
    data.table::setnames(geometry_step, geometry_col, "geom_tmp")
    dt_out[geometry_step, geometry := i.geom_tmp, on = id_key]
  }

  add_cols_by_id(dt_out, dt_pad, key = id_key, cols = "cep_padr")
  dt_out[, (score_cols) := NULL]

  dt_out[, .(
    id = get(id_key), cep_padr, best_step, endereco_best, endereco_fonte,
    precisao_best, best_score, geometry
  )]
}
