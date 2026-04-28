#--------------------------------------------------------------
# Pacotes necessários
#--------------------------------------------------------------
library(data.table)
library(stringr)

#--------------------------------------------------------------
# 0. Utilitário: garantir que uma coluna seja numérica
#   (mantém NAs, só converte o tipo)
#--------------------------------------------------------------
change_column_to_numeric <- function(dt, column) {
  stopifnot(data.table::is.data.table(dt))
  stopifnot(column %in% names(dt))
  
  if (!is.numeric(dt[[column]])) {
    dt[, (column) := as.numeric(get(column))]
    
    if (any(is.na(dt[[column]]))) {
      warning(sprintf("Coluna '%s' contém valores NA após a conversão.", column))
    }
  }
  
  dt
}

#--------------------------------------------------------------
# 1. Stopwords padrão
#--------------------------------------------------------------
default_stopwords <- c(
  "sala", "setor", "bloco", "quadra", "bl", "br", "lote", "lt", "qd",
 "conj", "conjunto", "casa", "grupo", "cjto", "fundos", "fund", "cjs", "salas",
  "ap", "apt", "apto", "apartamento", "sl", "zona", "andar", "fundos", "cj", "cjto", "loja",
  "galpão", "galpao", "box", "boxe", "pavilhão", "pavilhao", "pav", "pça", "prédio", "predio",
  "area", "are", "entrada", "fundo", "fundos", "subsolo", "sobreloja", "lojas", "zonas", "quadras",
  "lotes", "setores", "blocos", "andars", "apartamentos", "casaes", "casas", "conjuntos", "ponto de referencia",
  "garage", "terreo", "tereo", "térreo", "térreo", "andar térreo", "andar terreo", "parte","slj", "quiosque", "piso"
)

#--------------------------------------------------------------
# 2. Construir regex de stopwords
#--------------------------------------------------------------
build_stopwords_regex <- function(stopwords = default_stopwords) {
  paste0("\\b(", paste(stopwords, collapse = "|"), ")\\b")
}

#--------------------------------------------------------------
# FUNÇÃO AUXILIAR – extrair número aplicando TODAS as regras
#--------------------------------------------------------------
extract_num_from_address <- function(txt, stop_pos) {
  if (is.na(txt) || txt == "") return(NA_real_)
  
  # 1) Se existe stopword → usar só o trecho antes dela
  considered <- if (!is.na(stop_pos)) substr(txt, 1, stop_pos - 1) else txt
  considered <- str_squish(considered)
  if (considered == "") return(NA_real_)
  
  # 2) Se houver sequência de números consecutivos → pegar o primeiro
  seq_match <- str_match(
    considered,
    "\\b(\\d{1,5})(?:\\s+\\d{1,5})+\\b"
  )
  if (!all(is.na(seq_match))) {
    return(as.numeric(seq_match[, 2]))
  }
  
  # 3) Caso contrário → pegar o último número do trecho
  nums <- str_extract_all(considered, "\\d{1,5}")[[1]]
  if (length(nums) == 0) return(NA_real_)
  
  as.numeric(tail(nums, 1))
}

#--------------------------------------------------------------
# FUNÇÃO PRINCIPAL – VERSÃO FINAL E CORRIGIDA
#--------------------------------------------------------------
logradouro_num_string <- function(
  dt,
  endereco_col      = "endereco",
  num_col           = "numlograd",
  new_endereco_col  = "endereco_limpo",
  new_num_col       = "numlograd_novo",
  complemento_col   = "complemento",
  stopwords         = default_stopwords,
  endereco_update_mode = c("always_cut_on_stopword",
                           "cut_when_missing_num",
                           "no_cut")
) {
  endereco_update_mode <- match.arg(endereco_update_mode)
  stop_regex <- build_stopwords_regex(stopwords)
  
  stopifnot(is.data.table(dt))
  stopifnot(endereco_col %in% names(dt))
  stopifnot(num_col %in% names(dt))
  
  #----------------------------------------
  # 1) Copiar colunas originais
  #----------------------------------------
  end_orig <- dt[[endereco_col]]
  num_orig <- suppressWarnings(as.numeric(dt[[num_col]]))
  
  #----------------------------------------
  # 2) Pré-processamento do endereço
  #----------------------------------------
  end_work <- gsub("([A-Za-z])([0-9])", "\\1 \\2", end_orig)
  end_work <- gsub("([0-9])([A-Za-z])", "\\1 \\2", end_work)
  
  # remover S/N, SN, S.N, etc. em qualquer ponto do endereço
  end_work <- gsub("(?i)\\bS\\s*[/\\.-]?\\s*N\\b", " ", end_work, perl = TRUE)
  
  # remover pontos e vírgulas
  end_work <- gsub("[.,]", " ", end_work)
  
  # normalizar espaços
  end_work <- str_squish(end_work)
  
  #----------------------------------------
  # 3) Localizar primeira stopword
  #----------------------------------------
  stop_pos_orig <- str_locate(tolower(end_work), stop_regex)[, 1]
  
  #----------------------------------------
  # 4) Extrair número novo
  #----------------------------------------
  num_extracted <- mapply(
    extract_num_from_address,
    txt      = end_work,
    stop_pos = stop_pos_orig
  )
  
  #----------------------------------------
  # 5) Definir número final (num_out)
  #     - só atualiza quando original é NA ou 0
  #----------------------------------------
  needs_update <- is.na(num_orig) | num_orig == 0
  num_out <- num_orig
  num_out[needs_update & !is.na(num_extracted)] <- num_extracted[needs_update & !is.na(num_extracted)]
  num_out <- as.numeric(num_out)
  # An update would be: rerun geocoding with number extraction from address field 
#----------------------------------------
# 6) Remover o número usado do endereço (onde foi atualizado)
#    -> se houver sequência "6501 257", remove a SEQUÊNCIA inteira
#----------------------------------------
end_no_num <- end_work
idx_upd <- needs_update & !is.na(num_out)

for (i in which(idx_upd)) {
  n <- num_out[i]
  
  # padrão de sequência: ex "6501 257 300"
  pattern_seq <- paste0("\\b", n, "(?:\\s+\\d{1,5})+\\b")
  
  if (grepl(pattern_seq, end_no_num[i])) {
    # se achar "6501 257 ..." como sequência, remove tudo
    end_no_num[i] <- gsub(pattern_seq, "", end_no_num[i])
  } else {
    # caso contrário, remove só o número isolado
    end_no_num[i] <- gsub(paste0("\\b", n, "\\b"), "", end_no_num[i])
  }
}

end_no_num <- str_squish(end_no_num)
  #----------------------------------------
  # 7) Re-localizar stopword no endereço sem número
  #----------------------------------------
  stop_pos2 <- str_locate(tolower(end_no_num), stop_regex)[, 1]
  
  end_new  <- end_no_num
  comp_out <- rep(NA_character_, length(end_no_num))
  
  #----------------------------------------
  # 8) Aplicar modo de corte
  #    -> ATUALIZADO para remover TODOS os números antes da stopword
  #----------------------------------------
  if (endereco_update_mode == "always_cut_on_stopword") {
    
    idx_cut <- !is.na(stop_pos2)
    
    # trecho antes da stopword
    pre_stop <- substr(end_no_num[idx_cut], 1, stop_pos2[idx_cut] - 1)
    # remove todos os números desse trecho
    pre_stop <- gsub("\\b\\d{1,5}\\b", " ", pre_stop)
    pre_stop <- str_squish(pre_stop)
    
    end_new[idx_cut]  <- pre_stop
    comp_out[idx_cut] <- substr(end_no_num[idx_cut],
                                stop_pos2[idx_cut],
                                nchar(end_no_num[idx_cut]))
    
  } else if (endereco_update_mode == "cut_when_missing_num") {
    
    idx_cut <- !is.na(stop_pos2) & needs_update
    
    pre_stop <- substr(end_no_num[idx_cut], 1, stop_pos2[idx_cut] - 1)
    pre_stop <- gsub("\\b\\d{1,5}\\b", " ", pre_stop)
    pre_stop <- str_squish(pre_stop)
    
    end_new[idx_cut]  <- pre_stop
    comp_out[idx_cut] <- substr(end_no_num[idx_cut],
                                stop_pos2[idx_cut],
                                nchar(end_no_num[idx_cut]))
    
  } else { # "no_cut"
    # não corta, só removeu o número quando atualizado
  }
  
  end_new  <- str_squish(end_new)
  comp_out <- str_squish(comp_out)
  comp_out[comp_out == ""] <- NA_character_

  # #----------------------------------------
  # # 8b) Recolocar o número no endereco_limpo
  # #----------------------------------------
  # has_num <- !is.na(num_out) & num_out != 0
  # end_new[has_num] <- paste(end_new[has_num], num_out[has_num])
  # end_new <- str_squish(end_new)
    
  #----------------------------------------
  # 9) Escrever NOVAS colunas
  #----------------------------------------
  dt[[new_endereco_col]] <- end_new
  dt[[new_num_col]]      <- num_out
  dt[[complemento_col]]  <- comp_out
  
  dt <- change_column_to_numeric(dt, new_num_col)
  
  # Se complemento for todo NA, remove coluna
  if (all(is.na(dt[[complemento_col]]))) {
    dt[, (complemento_col) := NULL]
  }
  
  dt[]
}