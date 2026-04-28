ibge6_to_7 <- function(cod6) {
  cod6 <- as.character(cod6)

  # validação opcional (pode remover se quiser performance)
  if (any(nchar(cod6) != 6 | grepl("\\D", cod6))) {
    stop("Todos os códigos precisam ter exatamente 6 dígitos numéricos.")
  }

  split_digits <- strsplit(cod6, "")

  # cálculo do DV para cada elemento
  dv <- sapply(split_digits, function(digs) {
    soma <- 0
    for (i in 1:6) {
      d <- as.integer(digs[i])
      if (i %% 2 == 0) {
        d <- d * 2
        if (d > 9) d <- d - 9
      }
      soma <- soma + d
    }
    resto <- soma %% 10
    (10 - resto) %% 10
  })

  paste0(cod6, dv)
} 


add_sigla_from_uf <- function(dt, old_col, new_col) {
  
  # Tabela IBGE oficial
  uf_map <- c(
    "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA",
    "16" = "AP", "17" = "TO", "21" = "MA", "22" = "PI", "23" = "CE",
    "24" = "RN", "25" = "PB", "26" = "PE", "27" = "AL", "28" = "SE",
    "29" = "BA", "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP",
    "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS", "51" = "MT",
    "52" = "GO", "53" = "DF"
  )
  
  # Função interna para normalizar números (int, float, string etc.)
  normalize_code <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- gsub(",", ".", x)

    suppressWarnings(num <- as.numeric(x))

    # Se for número válido → formata como "33"
    ifelse(!is.na(num), sprintf("%02d", as.integer(num)), NA)
  }
  
  # Criar nova coluna no dt
  dt[, (new_col) := {
    code_norm <- normalize_code(get(old_col))
    uf_map[code_norm]   # faz a conversão
  }]
  
  return(dt)
}

change_cep_99999999_to_na <- function(dt, cep_column, municipio_column) {
  
  # cep 99999-999 is valid only for municipality of Sarandi/PR (code 4126256) 
  dt[ get(cep_column) == 99999999 & get(municipio_column) != 412625, (cep_column) := NA]
  return(dt)
}

add_col_if_missing <- function(dt, col, value = NA) {
  if (!col %in% names(dt)) {
    dt[, (col) := value]
  }
  dt
}