ibge6_to_7 <- function(cod6) {
  cod6 <- as.character(cod6)
  invalid <- !is.na(cod6) & (nchar(cod6) != 6L | grepl("\\D", cod6))
  if (any(invalid)) stop("Municipality codes must have exactly six digits.", call. = FALSE)

  out <- rep(NA_character_, length(cod6))
  valid_idx <- which(!is.na(cod6))

  out[valid_idx] <- vapply(strsplit(cod6[valid_idx], ""), function(digs) {
    soma <- 0L
    for (i in seq_len(6L)) {
      d <- as.integer(digs[i])
      if (i %% 2L == 0L) {
        d <- d * 2L
        if (d > 9L) d <- d - 9L
      }
      soma <- soma + d
    }
    paste0(paste0(digs, collapse = ""), (10L - soma %% 10L) %% 10L)
  }, character(1))

  out
}

add_sigla_from_uf <- function(dt, old_col, new_col) {
  stopifnot(data.table::is.data.table(dt), old_col %in% names(dt))

  uf_map <- c(
    "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA",
    "16" = "AP", "17" = "TO", "21" = "MA", "22" = "PI", "23" = "CE",
    "24" = "RN", "25" = "PB", "26" = "PE", "27" = "AL", "28" = "SE",
    "29" = "BA", "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP",
    "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS", "51" = "MT",
    "52" = "GO", "53" = "DF"
  )

  x <- trimws(gsub(",", ".", as.character(dt[[old_col]])))
  suppressWarnings(num <- as.numeric(x))
  code <- ifelse(!is.na(num), sprintf("%02d", as.integer(num)), NA_character_)
  dt[, (new_col) := unname(uf_map[code])]
  dt[]
}

change_cep_99999999_to_na <- function(dt, cep_column, municipio_column) {
  stopifnot(data.table::is.data.table(dt))
  require_columns(dt, c(cep_column, municipio_column), "dt")

  idx <- which(
    !is.na(dt[[cep_column]]) &
      as.character(dt[[cep_column]]) == "99999999" &
      as.character(dt[[municipio_column]]) != "412625"
  )
  if (length(idx) > 0L) data.table::set(dt, i = idx, j = cep_column, value = NA)
  dt[]
}

add_col_if_missing <- function(dt, col, value = NA) {
  stopifnot(data.table::is.data.table(dt))
  if (!col %in% names(dt)) dt[, (col) := value]
  dt[]
}
