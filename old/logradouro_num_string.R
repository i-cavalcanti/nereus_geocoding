library(data.table)
library(stringi)


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

# #--------------------------------------------------------------
# # FUNÇÃO AUXILIAR – extrair número aplicando TODAS as regras
# #--------------------------------------------------------------
# extract_num_from_address <- function(txt, stop_pos) {
#   if (is.na(txt) || txt == "") return(NA_real_)
  
#   # 1) Se existe stopword → usar só o trecho antes dela
#   considered <- if (!is.na(stop_pos)) substr(txt, 1, stop_pos - 1) else txt
#   considered <- str_squish(considered)
#   if (considered == "") return(NA_real_)
  
#   # 2) Se houver sequência de números consecutivos → pegar o primeiro
#   seq_match <- str_match(
#     considered,
#     "\\b(\\d{1,5})(?:\\s+\\d{1,5})+\\b"
#   )
#   if (!all(is.na(seq_match))) {
#     return(as.numeric(seq_match[, 2]))
#   }
  
#   # 3) Caso contrário → pegar o último número do trecho
#   nums <- str_extract_all(considered, "\\d{1,5}")[[1]]
#   if (length(nums) == 0) return(NA_real_)
  
#   as.numeric(tail(nums, 1))
# }

#--------------------------------------------------------------
# FUNÇÃO PRINCIPAL – VERSÃO FINAL
#--------------------------------------------------------------

logradouro_num_string_fast <- function(
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
  stopifnot(is.data.table(dt))
  
  # -------------------------------------------------
  # 1) Input vectors
  # -------------------------------------------------
  end_orig <- dt[[endereco_col]]
  num_orig <- suppressWarnings(as.numeric(dt[[num_col]]))
  needs_update <- is.na(num_orig) | num_orig == 0
  
  # -------------------------------------------------
  # 2) Preprocess address (vectorized)
  # -------------------------------------------------
  end_work <- end_orig
  end_work <- stri_replace_all_regex(end_work, "([A-Za-z])([0-9])", "$1 $2")
  end_work <- stri_replace_all_regex(end_work, "([0-9])([A-Za-z])", "$1 $2")
  end_work <- stri_replace_all_regex(end_work, "(?i)\\bS\\s*[/\\.-]?\\s*N\\b", " ")
  end_work <- stri_replace_all_regex(end_work, "[.,]", " ")
  end_work <- stri_trim_both(stri_replace_all_regex(end_work, "\\s+", " "))
  
  # -------------------------------------------------
  # 3) Stopword regex
  # -------------------------------------------------
  stop_regex <- paste0("\\b(", paste(stopwords, collapse = "|"), ")\\b")
  
  stop_pos <- stri_locate_first_regex(stri_trans_tolower(end_work), stop_regex)[,1]
  
  # -------------------------------------------------
  # 4) Truncate address at stopword (for extraction)
  # -------------------------------------------------
  end_for_num <- end_work
  has_stop <- !is.na(stop_pos)
  end_for_num[has_stop] <- substr(end_for_num[has_stop], 1, stop_pos[has_stop] - 1)
  end_for_num <- stri_trim_both(end_for_num)
  
  # -------------------------------------------------
  # 5) Extract numbers (vectorized)
  # -------------------------------------------------
  all_nums <- stri_extract_all_regex(end_for_num, "\\d{1,5}")
  
  num_extracted <- vapply(
    all_nums,
    function(x) if (length(x)) as.numeric(tail(x, 1)) else NA_real_,
    numeric(1)
  )
  
  # -------------------------------------------------
  # 6) Define final number
  # -------------------------------------------------
  num_out <- num_orig
  idx_replace <- needs_update & !is.na(num_extracted)
  num_out[idx_replace] <- num_extracted[idx_replace]
  
  # -------------------------------------------------
  # 7) Remove extracted number from address
  # -------------------------------------------------
  end_no_num <- end_work
  
  if (any(idx_replace)) {
    pat <- paste0("\\b(", num_out[idx_replace], ")\\b")
    end_no_num[idx_replace] <- stri_replace_all_regex(
      end_no_num[idx_replace],
      pat,
      " "
    )
  }
  
  end_no_num <- stri_trim_both(stri_replace_all_regex(end_no_num, "\\s+", " "))
  
  # -------------------------------------------------
  # 8) Recompute stopword position (post-clean)
  # -------------------------------------------------
  stop_pos2 <- stri_locate_first_regex(
    stri_trans_tolower(end_no_num),
    stop_regex
  )[,1]
  
  end_new  <- end_no_num
  comp_out <- rep(NA_character_, length(end_no_num))
  
  # -------------------------------------------------
  # 9) Apply cut modes
  # -------------------------------------------------
  if (endereco_update_mode != "no_cut") {
    
    idx_cut <- !is.na(stop_pos2)
    if (endereco_update_mode == "cut_when_missing_num") {
      idx_cut <- idx_cut & needs_update
    }
    
    pre <- substr(end_no_num[idx_cut], 1, stop_pos2[idx_cut] - 1)
    pre <- stri_trim_both(stri_replace_all_regex(pre, "\\b\\d{1,5}\\b", " "))
    
    end_new[idx_cut]  <- pre
    comp_out[idx_cut] <- substr(
      end_no_num[idx_cut],
      stop_pos2[idx_cut],
      nchar(end_no_num[idx_cut])
    )
  }
  
  end_new  <- stri_trim_both(end_new)
  comp_out <- stri_trim_both(comp_out)
  comp_out[comp_out == ""] <- NA_character_
  
  # -------------------------------------------------
  # 10) Write output columns
  # -------------------------------------------------
  dt[[new_endereco_col]] <- end_new
  dt[[new_num_col]]      <- as.numeric(num_out)
  
  if (!all(is.na(comp_out))) {
    dt[[complemento_col]] <- comp_out
  }
  
  dt[]
}
