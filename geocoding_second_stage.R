source("scripts/general-functions.R")

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

imputar_endereco_geometry_por_score <- function(
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
  diff_threshold = 0.2
) {
  
  stopifnot(
    data.table::is.data.table(dt),
    id_col %in% names(dt),
    endereco_col %in% names(dt),
    score_col %in% names(dt),
    municipio_col %in% names(dt),
    matrizfilial_col %in% names(dt),
    geometry_col %in% names(dt),
    validated_col %in% names(dt),
    is.numeric(diff_threshold),
    length(diff_threshold) == 1L
  )
  
  dt <- data.table::copy(dt)
  
  # Inicialização
  dt[, imputada := 0L]
  dt[, diff := NA_real_]
  dt[, (endereco_updated_col) := get(endereco_col)]
  dt[, (geometry_updated_col) := get(geometry_col)]
  dt[, (score_updated_col) := get(score_col)]
  dt[, (validated_updated_col) := get(validated_col)]
  
  # Ordenar: menor score = melhor
  data.table::setorderv(
    dt,
    cols  = c(id_col, score_col),
    order = c(1, 1)
  )
  
  # Processar por id
  dt[
    ,
    {
      ref_endereco    <- get(endereco_col)[1]
      ref_score       <- get(score_col)[1]
      ref_geom        <- get(geometry_col)[1]
      ref_validated   <- get(validated_col)[1]
      ref_municipio   <- get(municipio_col)[1]
      ref_matrizfilial <- get(matrizfilial_col)[1]
      
      diffs <- vapply(
        get(endereco_col),
        function(x) string_diff(ref_endereco, x),
        numeric(1)
      )
      
      # Regra: mesmo municipio da referência
      same_municipio <- !is.na(ref_municipio) &
                        !is.na(get(municipio_col)) &
                        get(municipio_col) == ref_municipio
      
      # Regra: mesma matriz/filial da referência
      same_matrizfilial <- !is.na(ref_matrizfilial) &
                           !is.na(get(matrizfilial_col)) &
                           get(matrizfilial_col) == ref_matrizfilial
      
      idx_local <- same_municipio &
                   same_matrizfilial &
                   !is.na(diffs) &
                   diffs <= diff_threshold &
                   !is.na(get(score_col)) &
                   get(score_col) > ref_score
      
      if (any(idx_local, na.rm = TRUE)) {
        idx_global <- .I[idx_local]
        
        data.table::set(dt, idx_global, "imputada", 1L)
        data.table::set(dt, idx_global, "diff", diffs[idx_local])
        data.table::set(dt, idx_global, endereco_updated_col, ref_endereco)
        data.table::set(dt, idx_global, geometry_updated_col, ref_geom)
        data.table::set(dt, idx_global, score_updated_col, ref_score)
        data.table::set(dt, idx_global, validated_updated_col, ref_validated)
      }
      
      NULL
    },
    by = id_col
  ]
  
  dt
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
  id_col         = "identificad_m",
  endereco_col   = "endereco_limpo",
  diff_threshold = 0.3,
  verbose        = TRUE
) {
  
  log <- function(...) {
    if (isTRUE(verbose)) {
      message(
        format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"),
        " ",
        paste0(...)
      )
    }
  }
  
  log("Iniciando main_multiyear()")
  
  # ----------------------------
  # Validações iniciais
  # ----------------------------
  log("Validando estrutura do data.table")
  
  stopifnot(
    data.table::is.data.table(dt_first_stage),
    id_col       %in% names(dt_first_stage),
    endereco_col %in% names(dt_first_stage),
    "geometry"        %in% names(dt_first_stage),
    "tipo_resultado"  %in% names(dt_first_stage),
    "estoque"         %in% names(dt_first_stage),
    is.numeric(diff_threshold),
    length(diff_threshold) == 1L
  )
  
  log("Validações concluídas com sucesso")
  
  # ----------------------------
  # 1) Criar score de precisão
  # ----------------------------
  log("Criando score de precisão (score_precisao)")
  
  dt <- criar_score_precisao(
    dt_first_stage,
    precisao_col = "tipo_resultado",
    score_col    = "score_precisao"
  )
  
  log("Score criado. Linhas: ", nrow(dt))
  
  # ----------------------------
  # 2) Imputar endereço e geometria
  # ----------------------------
  log("Iniciando imputação de endereço e geometria")
  
  dt <- imputar_endereco_geometry_por_score(
    dt,
    id_col               = id_col,
    endereco_col         = endereco_col,
    score_col            = "score_precisao",
    geometry_col         = "geometry",
    geometry_updated_col = "geometry_updated",
    score_updated_col    = "score_precisao_updated",
    diff_threshold       = diff_threshold
  )
  
  n_imputadas <- dt[imputada == 1L, .N]
  log("Imputação concluída. Registros imputados: ", n_imputadas)
  
  # ----------------------------
  # 3) Calcular stats de imputação
  # ----------------------------
  log("Calculando estatísticas de imputação")
  
  stats <- calcular_stats_imputacao(
    dt,
    imputada_col = "imputada",
    estoque_col  = "estoque"
  )
  
  log("Estatísticas calculadas")
  
  # ----------------------------
  # 4) Filtrar IDs com estoque zero
  # ----------------------------
  log("Filtrando IDs com estoque total igual a zero")
  
  n_ids_before <- uniqueN(dt[[id_col]])
  
  dt <- filtrar_ids_estoque_zero(
    dt          = dt,
    id_col      = id_col,
    estoque_col = "estoque"
  )
  
  n_ids_after <- uniqueN(dt[[id_col]])
  
  log(
    "Filtro concluído. IDs removidos: ",
    n_ids_before - n_ids_after
  )
  
  log("Processamento finalizado com sucesso")
  
  # ----------------------------
  # Retorno final
  # ----------------------------
  list(
    dt_final = dt,
    stats    = stats
  )
}



########## Adicionar na lógica que para imputar o município dos dois endereços deve ser o mesmo ###########
########## Adicionar a questao da matriz filial ###########