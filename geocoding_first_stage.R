source("scripts/general-functions.R")
source("scripts/data-standardization.R")
source("logradouro_num_string.R")

required_packages <- c("enderecobr","geocodebr", "data.table", "sf")

check_and_install_packages(required_packages)
lapply(required_packages, library, character.only = TRUE)


geocodificar_enderecos <- function(
  dt,
  campos_do_endereco,
  classe_col = "classe",
  operation = "cut_when_missing_num"
) {
  
  stopifnot(
    data.table::is.data.table(dt),
    is.character(classe_col),
    length(classe_col) == 1L
  )
  
  #### 1. Padronização de endereços ####
  dt <- padronizar_enderecos(
    dt,
    campos_do_endereco = campos_do_endereco
  )
  
  #### 2. Limpeza do número ####
  dt[numero_padr == "S/N", numero_padr := NA]
  
  #### 3. Separação logradouro / número ####
  dt2 <- logradouro_num_string_fast(
    dt,
    endereco_col = "logradouro_padr",
    num_col = "numero_padr",
    complemento_col = "complemento",
    endereco_update_mode = operation
  )
  
  #### 4. Definição dos campos de geocodificação ####
  campos <- geocodebr::definir_campos(
    logradouro  = "endereco_limpo",
    numero      = "numlograd_novo",
    cep         = "cep_padr",
    localidade  = "bairro_padr",
    municipio   = "municipio_padr",
    estado      = "estado_padr"
  )
  
  #### 5. Geocodificação ####
  dt3 <- geocodebr::geocode(
    enderecos            = dt2,
    campos_endereco      = campos,
    resultado_completo   = FALSE,
    resolver_empates     = TRUE,
    resultado_sf         = TRUE,
    verboso              = FALSE
  )
  
  data.table::setDT(dt3)
  
  #### 6. Classificação da precisão ####
  dt3[, (classe_col) := data.table::fifelse(
    precisao %in% c("numero", "numero_aproximado", "logradouro"), 0L,
    data.table::fifelse(precisao == "cep", 1L, 2L)
  )]
  
  return(dt3)
}


filtrar_ceps_consistentes <- function(
  dt_ceps,
  sd_threshold_km
) {
  
  cep_col <- "cep_padr"
  
  stopifnot(
    data.table::is.data.table(dt_ceps),
    cep_col %in% names(dt_ceps),
    is.numeric(sd_threshold_km),
    length(sd_threshold_km) == 1L
  )
  
  #### 1. CEPs únicos válidos ####
  ceps <- dt_ceps[!is.na(get(cep_col)), unique(get(cep_col))]
  
  if (length(ceps) == 0L) {
    return(list(
      ceps_aceitos = character(0),
      diagnostico  = data.table::data.table()
    ))
  }
  
  #### 2. Geocodificar CEPs ####
  df_ceps <- geocodebr::busca_por_cep(
    cep          = ceps,
    resultado_sf = TRUE,
    verboso      = FALSE
  )
  
  #### 3. Cálculo das distâncias ####
  df_ceps_sf <- sf::st_transform(df_ceps, 31983) # SIRGAS / UTM 23S
  coords     <- sf::st_coordinates(df_ceps_sf)
  
  df_ceps <- data.table::as.data.table(df_ceps)
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
  summary_ceps <- df_ceps[
    , .(
      n_points = .N,
      single   = .N == 1L
    ),
    by = cep
  ]
  
  diagnostico <- merge(
    sd_by_cep,
    summary_ceps,
    by = "cep",
    all.x = TRUE
  )
  
  #### 5. Aplicar critério ####
  diagnostico[, aceito := data.table::fifelse(
    single == TRUE | sd_dist_km <= sd_threshold_km,
    1L, 0L
  )]
  
  data.table::setorder(diagnostico, cep)
  
  #### 6. Retorno ####
  list(
    ceps_aceitos = diagnostico[aceito == 1L, cep],
    diagnostico  = diagnostico
  )
}


filtrar_enderecos_aceitos <- function(
  dt,
  ceps_aceitos,
  classe_col,
  aceito_col = "aceito"
) {
  
  cep_col <- "cep_padr"
  
  stopifnot(
    data.table::is.data.table(dt),
    is.character(ceps_aceitos),
    cep_col %in% names(dt),
    is.character(classe_col),
    length(classe_col) == 1L,
    classe_col %in% names(dt),
    is.character(aceito_col),
    length(aceito_col) == 1L
  )
  
  #### 1. Tabela de CEPs aceitos ####
  ceps_ok <- unique(
    data.table::data.table(
      cep_padr = ceps_aceitos
    )
  )
  data.table::setkey(ceps_ok, cep_padr)
  
  #### 2. Flag CEP válido ####
  dt[, cep_ok := 0L]
  
  dt[
    ceps_ok,
    cep_ok := 1L,
    on = "cep_padr"
  ]
  
  #### 3. Regra de aceitação ####
  dt[, (aceito_col) := data.table::fcase(
    get(classe_col) == 0L, 1L,
    get(classe_col) == 1L & cep_ok == 1L, 1L,
    default = 0L
  )]
  
  #### 4. Limpeza ####
  dt[, cep_ok := NULL]
  
  return(dt)
}

descritivas <- function(
  dt,
  estoque_col,
  aceito_col = "aceito"
) {
  
  stopifnot(
    data.table::is.data.table(dt),
    is.character(estoque_col),
    length(estoque_col) == 1L,
    estoque_col %in% names(dt),
    is.character(aceito_col),
    length(aceito_col) == 1L,
    aceito_col %in% names(dt)
  )
  
  stats_aceito <- dt[
    , .(
      n_linhas      = .N,
      estoque_total = sum(get(estoque_col), na.rm = TRUE)
    ),
    by = get(aceito_col)
  ]
  
  data.table::setnames(stats_aceito, "get", aceito_col)
  
  stats_aceito[
    , `:=`(
      prop_linhas  = n_linhas / sum(n_linhas),
      perc_linhas  = 100 * n_linhas / sum(n_linhas),
      prop_estoque = estoque_total / sum(estoque_total),
      perc_estoque = 100 * estoque_total / sum(estoque_total)
    )
  ]
  
  return(stats_aceito)
}


main_geocodificacao <- function(
  dt,
  campos,
  var_col = "estoque",
  sd_threshold_km = 0.3,
  operation = "cut_when_missing_num"
) {
  
  #### Parâmetros estruturais ####
  classe_col <- "classe"
  aceito_col <- "aceito"
  
  #### 1. Geocodificação e classificação ####
  message("[1/5] Limpando e Geocodificando endereços - frame de ", nrow(dt), " linhas...")
  dt3 <- geocodificar_enderecos(
    dt                 = dt,
    campos_do_endereco = campos,
    classe_col         = classe_col,
    operation = operation
  )
  message("[1/5] Concluído!")
  
  #### 2. Subconjunto CEP ####
  dt_cep <- dt3[get(classe_col) == 1L]
  message("[2/5] Extraindo subset de CEPs ", nrow(dt_cep), " registros de CEP selecionados.")
  
  #### 3. Filtro de CEPs consistentes ####
  message("[3/5] Filtrando CEPs consistentes...")
  res <- filtrar_ceps_consistentes(
    dt_ceps         = dt_cep,
    sd_threshold_km = sd_threshold_km
  )
  ceps_aceitos <- res$ceps_aceitos
  message("[3/5] Concluído! ", length(ceps_aceitos), " CEPs aceitos",
          " de um total de ", length(unique(dt_cep$cep_padr)), " CEPs avaliados.")
  
  #### 4. Filtro final de endereços ####
  message("[4/5] Aplicando filtro final nos endereços...")
  dt3 <- filtrar_enderecos_aceitos(
    dt           = dt3,
    ceps_aceitos = ceps_aceitos,
    classe_col   = classe_col,
    aceito_col   = aceito_col
  )
  message("[4/5] Concluído! Endereços filtrados.")
  
  #### 5. Estatísticas descritivas ####
  message("[5/5] Calculando estatísticas descritivas...")
  stats <- descritivas(
    dt          = dt3,
    estoque_col = var_col,
    aceito_col  = aceito_col
  )
  message("[5/5] Concluído! Estatísticas calculadas.")
  
  #### 6. Retorno ####
  message("Processo finalizado com sucesso!")
  
  list(
    dt_f = dt3,
    stats = stats
  )
}
