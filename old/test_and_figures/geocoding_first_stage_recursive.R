source("scripts/metrics-functions.R")

geocode_step <- function(dt, campos, step_tag,
                         keep_cols = c("id"),
                         precisao_col = "tipo_resultado",
                         resultado_completo = FALSE,
                         resolver_empates = TRUE,
                         resultado_sf = TRUE,
                         verboso = FALSE) {

  stopifnot(data.table::is.data.table(dt))
  if (!requireNamespace("sf", quietly = TRUE)) stop("Pacote 'sf' não instalado.")

  # geocodebr::geocode retorna sf quando resultado_sf=TRUE
  g <- geocodebr::geocode(
    enderecos          = dt,
    campos_endereco    = campos,
    resultado_completo = resultado_completo,
    resolver_empates   = resolver_empates,
    resultado_sf       = resultado_sf,
    verboso            = verboso
  )

  g <- data.table::as.data.table(g)

  # garantir que geometry exista (quando resultado_sf=TRUE geralmente vem como sfc)
  if (!("geometry" %in% names(g))) stop("geocode_step: coluna 'geometry' não encontrada no retorno do geocode.")

  # score
  g <- criar_score_precisao(g, precisao_col = precisao_col, score_col = paste0("score_precisao_", step_tag))

  # renomear colunas do passo
  data.table::setnames(g, "precisao", paste0("precisao_", step_tag))
  data.table::setnames(g, "geometry", paste0("geometry_", step_tag))

  # manter só chaves + colunas criadas
  score_col <- paste0("score_precisao_", step_tag)
  prec_col  <- paste0("precisao_", step_tag)
  geom_col  <- paste0("geometry_", step_tag)

  cols_out <- unique(c(keep_cols, ".row_id", prec_col, score_col, geom_col))
  miss <- setdiff(cols_out, names(g))
  if (length(miss) > 0) stop("geocode_step: faltam colunas no retorno: ", paste(miss, collapse = ", "))

  g[, ..cols_out]
}


escolher_melhor_passo <- function(dt, steps, id_cols = c("id", ".row_id"),
                                 out_geom = "geometry",
                                 out_prec = "precisao",
                                 out_score = "score_precisao") {
  stopifnot(data.table::is.data.table(dt), length(steps) >= 1L)

  # inicializa com o primeiro passo
  s1 <- steps[1]
  dt[, (out_score) := get(paste0("score_precisao_", s1))]
  dt[, (out_prec)  := get(paste0("precisao_", s1))]
  dt[, (out_geom)  := get(paste0("geometry_", s1))]

  # atualiza se algum passo posterior for melhor (score menor = mais preciso)
  for (s in steps[-1]) {
    sc <- paste0("score_precisao_", s)
    pr <- paste0("precisao_", s)
    gm <- paste0("geometry_", s)

    # regra: se score do novo passo é NA, ignora;
    # se score atual é NA e novo não, troca;
    # se novo < atual, troca.
    dt[
      !is.na(get(sc)) & (is.na(get(out_score)) | get(sc) < get(out_score)),
      `:=`(
        (out_score) := get(sc),
        (out_prec)  := get(pr),
        (out_geom)  := get(gm)
      )
    ]
  }

  dt[]
}


geocodificar_enderecos_v2 <- function(
  dt,
  campos_do_endereco,
  precisao_col = "tipo_resultado",
  classe_col = "classe",
  operation = "cut_when_missing_num"
) {
  stopifnot(data.table::is.data.table(dt))

  # id por linha (use seu id se for único; senão cria)
  if (!(".row_id" %in% names(dt))) dt[, .row_id := .I]

  #### 1) Padronização ####
  dt_pad <- padronizar_enderecos(dt, campos_do_endereco = campos_do_endereco)
  dt_pad[numero_padr == "S/N", numero_padr := NA]

  # campos para o geocode no estágio "padronizado"
  # (ajuste conforme as colunas geradas por padronizar_enderecos)
  campos_pad <- geocodebr::definir_campos(
    logradouro = "logradouro_padr",
    numero     = "numero_padr",
    cep        = "cep_padr",
    localidade = "bairro_padr",
    municipio  = "municipio_padr",
    estado     = "estado_padr"
  )

  g_pad <- geocode_step(
    dt = dt_pad,
    campos = campos_pad,
    precisao_col = precisao_col,
    step_tag = "pad",
    keep_cols = intersect(names(dt_pad), c("id")) # mantém id se existir
  )

  #### 2) Separação logradouro/número + geocode novamente ####
  dt_sep <- logradouro_num_string_fast(
    dt_pad,
    endereco_col = "logradouro_padr",
    num_col = "numero_padr",
    complemento_col = "complemento",
    endereco_update_mode = operation
  )

  # campos no estágio "separado"
  campos_sep <- geocodebr::definir_campos(
    logradouro = "endereco_limpo",
    numero     = "numlograd_novo",
    cep        = "cep_padr",
    localidade = "bairro_padr",
    municipio  = "municipio_padr",
    estado     = "estado_padr"
  )

  g_sep <- geocode_step(
    dt = dt_sep,
    campos = campos_sep,
    precisao_col = precisao_col,
    step_tag = "sep",
    keep_cols = intersect(names(dt_sep), c("id"))
  )

  #### 3) Merge de resultados por linha ####
  base_cols <- unique(c(intersect(names(dt_sep), names(dt)), "id", ".row_id"))
  dt_out <- dt_sep[, ..base_cols]

  data.table::setkeyv(dt_out, ".row_id")
  data.table::setkeyv(g_pad,  ".row_id")
  data.table::setkeyv(g_sep,  ".row_id")

  dt_out <- g_pad[dt_out]
  dt_out <- g_sep[dt_out]

  #### 4) Escolher melhor geometria ####
  dt_out <- escolher_melhor_passo(dt_out, steps = c("pad", "sep"),
                                  out_geom = "geometry",
                                  out_prec = "precisao",
                                  out_score = "score_precisao")

  #### 5) Classe (você pode redefinir usando score/precisão nova) ####
  # Exemplo: mantém tua lógica antiga, mas agora baseada em 'precisao' final
  dt_out[, (classe_col) := data.table::fifelse(
    precisao %in% c("numero", "numero_aproximado", "logradouro"), 0L,
    data.table::fifelse(precisao == "cep", 1L, 2L)
  )]

  # opcional: remover geometrias intermediárias se quiser
  # dt_out[, c("geometry_pad","geometry_sep") := NULL]

  dt_out[]
}
