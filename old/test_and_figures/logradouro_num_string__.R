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
  "garage", "terreo", "tereo", "térreo", "térreo", "andar térreo", "andar terreo", "parte", "slj", "quiosque", "piso"
)

#--------------------------------------------------------------
# FUNÇÃO PRINCIPAL – com stopwords numéricas (CEP/município)
# e parâmetro max_digit_edits
#--------------------------------------------------------------
logradouro_num_string_fast <- function(
  dt,
  endereco_col      = "endereco",
  num_col           = "numlograd",
  new_endereco_col  = "endereco_limpo",
  new_num_col       = "numlograd_novo",
  complemento_col   = "complemento",
  municipio_col     = "municipio",
  cep_col           = "cep",
  stopwords         = default_stopwords,
  use_numeric_stopwords = TRUE,
  max_digit_edits   = 1L,
  endereco_update_mode = c("always_cut_on_stopword",
                           "cut_when_missing_num",
                           "no_cut")
) {
  endereco_update_mode <- match.arg(endereco_update_mode)
  stopifnot(is.data.table(dt))

  stopifnot(is.logical(use_numeric_stopwords), length(use_numeric_stopwords) == 1, !is.na(use_numeric_stopwords))
  stopifnot(is.numeric(max_digit_edits), length(max_digit_edits) == 1, max_digit_edits >= 0)
  max_digit_edits <- as.integer(max_digit_edits)

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
  # 2.5) Preparar CEP/município limpos (digits-only)
  # -------------------------------------------------
  cep_clean <- mun_clean <- rep(NA_character_, length(end_work))

  if (use_numeric_stopwords) {
    cep_raw <- dt[[cep_col]]
    mun_raw <- dt[[municipio_col]]

    cep_clean <- stri_replace_all_regex(cep_raw, "\\D+", "")
    mun_clean <- stri_replace_all_regex(mun_raw, "\\D+", "")

    cep_clean[cep_clean == ""] <- NA_character_
    mun_clean[mun_clean == ""] <- NA_character_
  }

  # -------------------------------------------------
  # 3) Stopword regex (textual) + posição
  # -------------------------------------------------
  stop_regex <- paste0("\\b(", paste(stopwords, collapse = "|"), ")\\b")
  stop_pos_txt <- stri_locate_first_regex(stri_trans_tolower(end_work), stop_regex)[, 1]

  # -------------------------------------------------
  # 3.5) Stopword numérica (CEP/município) com adist <= max_digit_edits
  #       (pega a PRIMEIRA sequência de dígitos no texto que seja "muito similar")
  # -------------------------------------------------
  stop_pos_num <- rep(NA_integer_, length(end_work))

  if (use_numeric_stopwords) {
    locs <- stri_locate_all_regex(end_work, "\\d+")
    runs <- stri_extract_all_regex(end_work, "\\d+")

    stop_pos_num <- vapply(seq_along(end_work), function(i) {
      cand <- runs[[i]]
      if (!length(cand)) return(NA_integer_)

      targets <- c(cep_clean[i], mun_clean[i])
      targets <- targets[!is.na(targets)]
      if (!length(targets)) return(NA_integer_)

      ok <- vapply(seq_along(cand), function(j) {
        any(vapply(targets, function(tg) {
          utils::adist(cand[j], tg)[1, 1] <= max_digit_edits
        }, logical(1)), na.rm = TRUE)
      }, logical(1))

      if (!any(ok %in% TRUE)) return(NA_integer_)
      starts <- locs[[i]][ok, 1]
      min(starts, na.rm = TRUE)
    }, integer(1))
  }

  # -------------------------------------------------
  # 3.6) Stopword final (a mais cedo entre textual e numérica)
  # -------------------------------------------------
  stop_pos <- pmin(stop_pos_txt, stop_pos_num, na.rm = TRUE)
  stop_pos[is.infinite(stop_pos)] <- NA_integer_

  # -------------------------------------------------
  # 4) Truncate address at stopword (for number extraction)
  # -------------------------------------------------
  end_for_num <- end_work
  has_stop <- !is.na(stop_pos)
  end_for_num[has_stop] <- substr(end_for_num[has_stop], 1, stop_pos[has_stop] - 1)
  end_for_num <- stri_trim_both(end_for_num)

  # -------------------------------------------------
  # 5) Extract numbers (vectorized) - pega o ÚLTIMO número (1-5 dígitos)
  # -------------------------------------------------
  all_nums <- stri_extract_all_regex(end_for_num, "\\d{1,5}")
  num_extracted <- vapply(
    all_nums,
    function(x) if (length(x)) as.numeric(tail(x, 1)) else NA_real_,
    numeric(1)
  )

  # -------------------------------------------------
  # 6) Define final number (mantém comportamento original: só preenche se NA/0)
  # -------------------------------------------------
  num_out <- num_orig
  idx_replace <- needs_update & !is.na(num_extracted)
  num_out[idx_replace] <- num_extracted[idx_replace]

  # -------------------------------------------------
  # 7) Remove extracted number from address (CORRIGIDO: por linha)
  # -------------------------------------------------
  end_no_num <- end_work

  if (any(idx_replace)) {
    end_no_num[idx_replace] <- mapply(
      FUN = function(txt, n) {
        if (is.na(txt) || is.na(n)) return(txt)
        pat <- paste0("\\b", n, "\\b")
        stri_replace_all_regex(txt, pat, " ")
      },
      txt = end_no_num[idx_replace],
      n   = num_out[idx_replace],
      USE.NAMES = FALSE
    )
  }

  end_no_num <- stri_trim_both(stri_replace_all_regex(end_no_num, "\\s+", " "))

  # -------------------------------------------------
  # 8) Recompute stopword position (post-clean) - textual
  # -------------------------------------------------
  stop_pos2_txt <- stri_locate_first_regex(
    stri_trans_tolower(end_no_num),
    stop_regex
  )[, 1]

  # -------------------------------------------------
  # 8.5) Recompute stopword position (post-clean) - numérica
  # -------------------------------------------------
  stop_pos2_num <- rep(NA_integer_, length(end_no_num))

  if (use_numeric_stopwords) {
    locs2 <- stri_locate_all_regex(end_no_num, "\\d+")
    runs2 <- stri_extract_all_regex(end_no_num, "\\d+")

    stop_pos2_num <- vapply(seq_along(end_no_num), function(i) {
      cand <- runs2[[i]]
      if (!length(cand)) return(NA_integer_)

      targets <- c(cep_clean[i], mun_clean[i])
      targets <- targets[!is.na(targets)]
      if (!length(targets)) return(NA_integer_)

      ok <- vapply(seq_along(cand), function(j) {
        any(vapply(targets, function(tg) {
          utils::adist(cand[j], tg)[1, 1] <= max_digit_edits
        }, logical(1)), na.rm = TRUE)
      }, logical(1))

      if (!any(ok %in% TRUE)) return(NA_integer_)
      starts <- locs2[[i]][ok, 1]
      min(starts, na.rm = TRUE)
    }, integer(1))
  }

  # stopword final pós-clean
  stop_pos2 <- pmin(stop_pos2_txt, stop_pos2_num, na.rm = TRUE)
  stop_pos2[is.infinite(stop_pos2)] <- NA_integer_

  # -------------------------------------------------
  # 9) Apply cut modes
  # -------------------------------------------------
  end_new  <- end_no_num
  comp_out <- rep(NA_character_, length(end_no_num))

  if (endereco_update_mode != "no_cut") {

    idx_cut <- !is.na(stop_pos2)
    if (endereco_update_mode == "cut_when_missing_num") {
      idx_cut <- idx_cut & needs_update
    }

    if (any(idx_cut)) {
      pre <- substr(end_no_num[idx_cut], 1, stop_pos2[idx_cut] - 1)
      pre <- stri_trim_both(stri_replace_all_regex(pre, "\\b\\d{1,5}\\b", " "))

      end_new[idx_cut]  <- stri_trim_both(stri_replace_all_regex(pre, "\\s+", " "))
      comp_out[idx_cut] <- substr(
        end_no_num[idx_cut],
        stop_pos2[idx_cut],
        nchar(end_no_num[idx_cut])
      )
    }
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
