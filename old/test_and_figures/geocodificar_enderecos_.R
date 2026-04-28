source("scripts/metrics-functions.R")

# ---- helper: extrair campo de vetor nomeado OU lista
get_campo <- function(campos, nome) {
  if (is.list(campos) && !is.null(campos[[nome]])) return(campos[[nome]])
  if (!is.null(names(campos)) && nome %in% names(campos)) return(unname(campos[[nome]]))
  stop("campos_do_endereco não tem o campo '", nome, "'.")
}

# ---- helper: geocode de uma etapa (leve), com opção de trazer geometry
geocode_step <- function(dt, campos, step_tag,
                         keep_cols = c("id"),
                         keep_geometry = FALSE,
                         resultado_completo = FALSE,
                         resolver_empates = TRUE,
                         resultado_sf = TRUE,
                         verboso = FALSE) {

  stopifnot(data.table::is.data.table(dt))

  # NÃO use colunas com prefixo "." dentro do geocodebr (quebra no SQL)
  # Garanta uma coluna row_id "safe"
  if (!(".row_id" %in% names(dt)) && !("row_id" %in% names(dt))) {
    stop("geocode_step: falta .row_id (ou row_id) na tabela.")
  }

  dt_local <- data.table::copy(dt)

  if (".row_id" %in% names(dt_local)) {
    # criar row_id e DROPAR .row_id antes do geocode
    dt_local[, row_id := .row_id]
    dt_local[, .row_id := NULL]
  }
  # se já tiver row_id, usa como está

  g <- geocodebr::geocode(
    enderecos          = dt_local,
    campos_endereco    = campos,
    resultado_completo = resultado_completo,
    resolver_empates   = resolver_empates,
    resultado_sf       = resultado_sf,
    verboso            = verboso
  )

  g <- data.table::as.data.table(g)

  # garantir que row_id voltou
  if (!("row_id" %in% names(g))) {
    stop("geocode_step: coluna 'row_id' não veio no retorno do geocode. ",
         "Verifique se ela estava presente no input.")
  }

  if (!("precisao" %in% names(g))) stop("geocode_step: coluna 'precisao' não veio do geocode.")

  score_col <- paste0("score_precisao_", step_tag)
  g <- criar_score_precisao(g, precisao_col = precisao_col, score_col = score_col)

  prec_col <- paste0("precisao_", step_tag)
  data.table::setnames(g, "precisao", prec_col)

  out_cols <- unique(c("row_id", intersect(keep_cols, names(g)), prec_col, score_col))

  if (isTRUE(keep_geometry)) {
    if (!("geometry" %in% names(g))) stop("geocode_step: geometry não veio (resultado_sf=TRUE?).")
    geom_col <- paste0("geometry_", step_tag)
    data.table::setnames(g, "geometry", geom_col)
    out_cols <- c(out_cols, geom_col)
  }

  g[, ..out_cols]
}

# ---- helper: escolher best_step só com scores (empate -> último)
escolher_best_step_por_score <- function(dt, steps, tie_last = TRUE) {
  stopifnot(data.table::is.data.table(dt), length(steps) >= 1L)

  s1 <- steps[1]
  dt[, score_best := get(paste0("score_precisao_", s1))]
  dt[, best_step  := s1]

  for (s in steps[-1]) {
    sc <- paste0("score_precisao_", s)
    if (tie_last) {
      dt[!is.na(get(sc)) & (is.na(score_best) | get(sc) <= score_best),
         `:=`(score_best = get(sc), best_step = s)]
    } else {
      dt[!is.na(get(sc)) & (is.na(score_best) | get(sc) < score_best),
         `:=`(score_best = get(sc), best_step = s)]
    }
  }

  dt[]
}

# ---- main: v4 RAM-friendly
geocodificar_enderecos_v4 <- function(
  dt,
  campos_do_endereco,  # vetor nomeado ou list: logradouro, numero, cep, bairro, municipio, estado
  operation_modes = c("no_cut", "cut_when_missing_num", "always_cut_on_stopword"),
  precisao_col = "tipo_resultado",
  keep_precisao_text = TRUE,
  verbose = TRUE
) {
  stopifnot(data.table::is.data.table(dt))
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Precisa do pacote data.table.")
  if (!requireNamespace("geocodebr", quietly = TRUE)) stop("Precisa do pacote geocodebr.")

  log <- function(...) if (isTRUE(verbose)) message(...)

  # ---- helper: extrair campo de vetor nomeado OU lista
  get_campo <- function(campos, nome) {
    if (is.list(campos) && !is.null(campos[[nome]])) return(campos[[nome]])
    if (!is.null(names(campos)) && nome %in% names(campos)) return(unname(campos[[nome]]))
    stop("campos_do_endereco não tem o campo '", nome, "'.")
  }

  # ---- helper: geocode de uma etapa, evitando colunas que começam com "."
  geocode_step <- function(dt_in, campos, step_tag,
                           keep_cols = c("id"),
                           keep_geometry = FALSE,
                           resultado_completo = FALSE,
                           resolver_empates = TRUE,
                           resultado_sf = TRUE,
                           verboso = FALSE) {

    stopifnot(data.table::is.data.table(dt_in))
    if (!("row_id" %in% names(dt_in))) stop("geocode_step: falta 'row_id' no input.")

    # cópia para não mexer no original
    dt_local <- data.table::copy(dt_in)

    # (opcional) remove quaisquer colunas começando com "." para evitar surpresas no SQL
    dot_cols <- grep("^\\.", names(dt_local), value = TRUE)
    if (length(dot_cols) > 0L) dt_local[, (dot_cols) := NULL]

    g <- geocodebr::geocode(
      enderecos          = dt_local,
      campos_endereco    = campos,
      resultado_completo = resultado_completo,
      resolver_empates   = resolver_empates,
      resultado_sf       = resultado_sf,
      verboso            = verboso
    )
    g <- data.table::as.data.table(g)

    if (!("row_id" %in% names(g))) stop("geocode_step: 'row_id' não veio no retorno do geocode.")
    if (!("precisao" %in% names(g))) stop("geocode_step: coluna 'precisao' não veio do geocode.")

    score_col <- paste0("score_precisao_", step_tag)
    g <- criar_score_precisao(g, precisao_col = precisao_col, score_col = score_col)

    prec_col <- paste0("precisao_", step_tag)
    data.table::setnames(g, "precisao", prec_col)

    out_cols <- unique(c("row_id", intersect(keep_cols, names(g)), prec_col, score_col))

    if (isTRUE(keep_geometry)) {
      if (!("geometry" %in% names(g))) stop("geocode_step: geometry não veio (resultado_sf=TRUE?).")
      geom_col <- paste0("geometry_", step_tag)
      data.table::setnames(g, "geometry", geom_col)
      out_cols <- c(out_cols, geom_col)
    }

    g[, ..out_cols]
  }

  # ---- helper: escolher best_step só com scores (empate -> último)
  escolher_best_step_por_score <- function(dt_in, steps, tie_last = TRUE) {
    stopifnot(data.table::is.data.table(dt_in), length(steps) >= 1L)

    s1 <- steps[1]
    dt_in[, score_best := get(paste0("score_precisao_", s1))]
    dt_in[, best_step  := s1]

    for (s in steps[-1]) {
      sc <- paste0("score_precisao_", s)
      if (tie_last) {
        dt_in[!is.na(get(sc)) & (is.na(score_best) | get(sc) <= score_best),
              `:=`(score_best = get(sc), best_step = s)]
      } else {
        dt_in[!is.na(get(sc)) & (is.na(score_best) | get(sc) < score_best),
              `:=`(score_best = get(sc), best_step = s)]
      }
    }
    dt_in[]
  }

  # ---- garantir row_id SQL-safe
  if (!("row_id" %in% names(dt))) dt[, row_id := .I]

  keep_id <- if ("id" %in% names(dt)) "id" else character(0)

  # tags para as 3 variações do logradouro_num_string_fast
  tag_map <- c(
    no_cut = "lns_nc",
    cut_when_missing_num = "lns_cmn",
    always_cut_on_stopword = "lns_acs"
  )
  stopifnot(all(operation_modes %in% names(tag_map)))

  # ============================================================
  # 1) RAW geocode (sem geometry)
  # ============================================================
  log("[raw] geocode (no geometry)")
  campos_raw <- geocodebr::definir_campos(
    logradouro = get_campo(campos_do_endereco, "logradouro"),
    numero     = get_campo(campos_do_endereco, "numero"),
    cep        = get_campo(campos_do_endereco, "cep"),
    localidade = get_campo(campos_do_endereco, "bairro"),
    municipio  = get_campo(campos_do_endereco, "municipio"),
    estado     = get_campo(campos_do_endereco, "estado")
  )
  g_raw <- geocode_step(dt, campos_raw, "raw", keep_cols = keep_id, keep_geometry = FALSE)

  # ============================================================
  # 2) PAD + geocode (sem geometry)
  # ============================================================
  log("[pad] padronizar_enderecos + geocode (no geometry)")
  cols_padr <- c("logradouro_padr","numero_padr","cep_padr","bairro_padr","municipio_padr","estado_padr")
  cols_padr <- intersect(cols_padr, names(dt))
  if (length(cols_padr) > 0L) {
    dt[, (cols_padr) := NULL]
  }
  dt_pad <- padronizar_enderecos(dt, campos_do_endereco = campos_do_endereco)

  # ajustes típicos (mantém seu comportamento)
  if ("numero_padr" %in% names(dt_pad)) dt_pad[numero_padr == "S/N", numero_padr := NA]

  # garantir row_id (padronizar pode recriar dt)
  if (!("row_id" %in% names(dt_pad))) dt_pad[, row_id := dt[["row_id"]]]

  campos_pad <- geocodebr::definir_campos(
    logradouro = "logradouro_padr",
    numero     = "numero_padr",
    cep        = "cep_padr",
    localidade = "bairro_padr",
    municipio  = "municipio_padr",
    estado     = "estado_padr"
  )
  g_pad <- geocode_step(dt_pad, campos_pad, "pad", keep_cols = keep_id, keep_geometry = FALSE)

  # ============================================================
  # 3) LNS por modo + geocode (sem geometry)
  # ============================================================
  g_list <- list(raw = g_raw, pad = g_pad)

  for (mode in operation_modes) {
    tag <- unname(tag_map[[mode]])
    log("[", tag, "] logradouro_num_string_fast(", mode, ") + geocode (no geometry)")

    dt_lns <- logradouro_num_string_fast(
      dt_pad,
      endereco_col = "logradouro_padr",
      num_col = "numero_padr",
      complemento_col = "complemento",
      endereco_update_mode = mode
    )

    if (!("row_id" %in% names(dt_lns))) dt_lns[, row_id := dt_pad[["row_id"]]]

    campos_lns <- geocodebr::definir_campos(
      logradouro = "endereco_limpo",
      numero     = "numlograd_novo",
      cep        = "cep_padr",
      localidade = "bairro_padr",
      municipio  = "municipio_padr",
      estado     = "estado_padr"
    )

    g_list[[tag]] <- geocode_step(dt_lns, campos_lns, tag, keep_cols = keep_id, keep_geometry = FALSE)

    rm(dt_lns); gc()
  }

  # ============================================================
  # 4) Montar dt_out (base = dt_pad) + anexar scores/precisões
  # ============================================================
  dt_out <- dt_pad
  data.table::setkey(dt_out, row_id)

  for (nm in names(g_list)) {
    g <- g_list[[nm]]
    data.table::setkey(g, row_id)
    dt_out <- g[dt_out]  # left join
  }

  if (!isTRUE(keep_precisao_text)) {
    prec_cols <- grep("^precisao_", names(dt_out), value = TRUE)
    if (length(prec_cols) > 0L) dt_out[, (prec_cols) := NULL]
  }

  # ============================================================
  # 5) Escolher best_step (empate -> último)
  # ============================================================
  steps <- c("raw", "pad", unname(tag_map[operation_modes]))  # ordem pedida
  dt_out <- escolher_best_step_por_score(dt_out, steps = steps, tie_last = TRUE)

  dt_out[, score_precisao := score_best]
  dt_out[, score_best := NULL]

  if (isTRUE(keep_precisao_text)) {
    dt_out[, precisao := NA_character_]
    for (s in steps) {
      pc <- paste0("precisao_", s)
      if (pc %in% names(dt_out)) {
        dt_out[best_step == s, precisao := get(pc)]
      }
    }
  }

  # ============================================================
  # 6) Buscar geometry APENAS do passo vencedor (por grupos)
  # ============================================================

  fetch_geom_for_step <- function(step_name) {
    ids <- dt_out[best_step == step_name, row_id]
    if (length(ids) == 0L) return(NULL)

    if (step_name == "raw") {
      dt_sub <- dt[row_id %in% ids]
      campos <- campos_raw
    } else if (step_name == "pad") {
      dt_sub <- dt_pad[row_id %in% ids]
      campos <- campos_pad
    } else {
      # lns_* -> descobrir mode
      mode <- names(tag_map)[match(step_name, tag_map)]
      dt_sub <- dt_pad[row_id %in% ids]
      dt_sub <- logradouro_num_string_fast(
        dt_sub,
        endereco_col = "logradouro_padr",
        num_col = "numero_padr",
        complemento_col = "complemento",
        endereco_update_mode = mode
      )
      campos <- geocodebr::definir_campos(
        logradouro = "endereco_limpo",
        numero     = "numlograd_novo",
        cep        = "cep_padr",
        localidade = "bairro_padr",
        municipio  = "municipio_padr",
        estado     = "estado_padr"
      )
    }

    geocode_step(dt_sub, campos, step_name, keep_cols = keep_id, keep_geometry = TRUE)
  }

  geom_list <- list()
  for (s in steps) {
    log("[geom] fetch geometry for step: ", s)
    gg <- fetch_geom_for_step(s)
    if (!is.null(gg)) geom_list[[s]] <- gg
    gc()
  }

  # preencher geometry final
  for (s in names(geom_list)) {
  gg <- geom_list[[s]]
  data.table::setkey(gg, row_id)

  geom_col <- paste0("geometry_", s)

  # renomeia a geometry da etapa para um nome fixo, pra ficar fácil referenciar
  if (!(geom_col %in% names(gg))) {
    stop("Não achei a coluna ", geom_col, " no gg do step ", s)
  }
  data.table::setnames(gg, geom_col, "geom_tmp")

  # join + atribuição usando a coluna do i (gg)
  dt_out[gg, geometry := i.geom_tmp]

  }

  dt_out[]
}
