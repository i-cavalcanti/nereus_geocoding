default_stopwords <- unique(c(
  "sala", "setor", "bloco", "quadra", "bl", "br", "lote", "lt", "qd",
  "conj", "conjunto", "casa", "grupo", "cjto", "fundos", "fund", "cjs",
  "salas", "ap", "apt", "apto", "apartamento", "sl", "zona", "andar",
  "cj", "loja", "galpão", "galpao", "box", "boxe", "pavilhão", "pavilhao",
  "pav", "pça", "prédio", "predio", "area", "are", "entrada", "fundo",
  "subsolo", "sobreloja", "lojas", "zonas", "quadras", "lotes", "setores",
  "blocos", "andars", "apartamentos", "casas", "conjuntos",
  "ponto de referencia", "garage", "terreo", "tereo", "térreo", "andar térreo",
  "andar terreo", "parte", "slj", "quiosque", "piso"
))

change_column_to_numeric <- function(dt, column) {
  stopifnot(data.table::is.data.table(dt), column %in% names(dt))
  if (!is.numeric(dt[[column]])) dt[, (column) := as.numeric(get(column))]
  dt[]
}

logradouro_num_string_fast <- function(
  dt,
  endereco_col = "endereco",
  num_col = "numlograd",
  new_endereco_col = "endereco_limpo",
  new_num_col = "numlograd_novo",
  stopwords = default_stopwords,
  endereco_update_mode = c("always_cut_on_stopword", "cut_when_missing_num", "no_cut")
) {
  endereco_update_mode <- match.arg(endereco_update_mode)
  stopifnot(data.table::is.data.table(dt))
  require_columns(dt, c(endereco_col, num_col), "dt")

  end_orig <- as.character(dt[[endereco_col]])
  num_orig <- suppressWarnings(as.numeric(dt[[num_col]]))
  needs_update <- is.na(num_orig) | num_orig == 0

  end_work <- end_orig
  end_work <- stringi::stri_replace_all_regex(end_work, "([A-Za-z])([0-9])", "$1 $2")
  end_work <- stringi::stri_replace_all_regex(end_work, "([0-9])([A-Za-z])", "$1 $2")
  end_work <- stringi::stri_replace_all_regex(end_work, "(?i)\\bS\\s*[/\\.-]?\\s*N\\b", " ")
  end_work <- stringi::stri_replace_all_regex(end_work, "[.,]", " ")
  end_work <- stringi::stri_trim_both(stringi::stri_replace_all_regex(end_work, "\\s+", " "))

  stop_regex <- paste0("\\b(", paste(stopwords, collapse = "|"), ")\\b")
  stop_pos <- stringi::stri_locate_first_regex(stringi::stri_trans_tolower(end_work), stop_regex)[, 1]

  end_for_num <- end_work
  has_stop <- !is.na(stop_pos)
  end_for_num[has_stop] <- substr(end_for_num[has_stop], 1, stop_pos[has_stop] - 1L)
  end_for_num <- stringi::stri_trim_both(end_for_num)

  all_nums <- stringi::stri_extract_all_regex(end_for_num, "\\d{1,5}")
  num_extracted <- vapply(all_nums, function(x) {
    if (length(x) > 0L && !all(is.na(x))) as.numeric(utils::tail(x, 1L)) else NA_real_
  }, numeric(1))

  num_out <- num_orig
  idx_replace <- needs_update & !is.na(num_extracted)
  num_out[idx_replace] <- num_extracted[idx_replace]

  end_no_num <- end_work
  if (any(idx_replace)) {
    pattern <- paste0("\\b(", num_out[idx_replace], ")\\b")
    end_no_num[idx_replace] <- stringi::stri_replace_all_regex(end_no_num[idx_replace], pattern, " ")
  }
  end_no_num <- stringi::stri_trim_both(stringi::stri_replace_all_regex(end_no_num, "\\s+", " "))

  end_new <- end_no_num
  if (endereco_update_mode != "no_cut") {
    stop_pos2 <- stringi::stri_locate_first_regex(stringi::stri_trans_tolower(end_no_num), stop_regex)[, 1]
    idx_cut <- !is.na(stop_pos2)
    if (endereco_update_mode == "cut_when_missing_num") idx_cut <- idx_cut & needs_update

    if (any(idx_cut)) {
      pre <- substr(end_no_num[idx_cut], 1, stop_pos2[idx_cut] - 1L)
      pre <- stringi::stri_replace_all_regex(pre, "\\b\\d{1,5}\\b", " ")
      end_new[idx_cut] <- stringi::stri_trim_both(stringi::stri_replace_all_regex(pre, "\\s+", " "))
    }
  }

  dt[, (new_endereco_col) := stringi::stri_trim_both(end_new)]
  dt[, (new_num_col) := as.numeric(num_out)]
  dt[]
}
