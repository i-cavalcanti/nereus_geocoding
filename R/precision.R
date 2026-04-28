precision_order <- c(
  "dn01", "dn02", "dn03", "dn04",
  "pn01", "pn02", "pn03", "pn04",
  "da01", "da02", "da03", "da04",
  "pa01", "pa02", "pa03", "pa04",
  "dl01", "dl02", "dl03", "dl04",
  "pl01", "pl02", "pl03", "pl04",
  "dc01", "dc02", "db01", "dm01"
)

criar_score_precisao <- function(dt, precisao_col, score_col = "score_precisao") {
  stopifnot(data.table::is.data.table(dt))
  require_columns(dt, precisao_col, "dt")

  mapa_score <- data.table::data.table(
    precisao = precision_order,
    score = seq_along(precision_order)
  )

  dt[mapa_score, (score_col) := i.score, on = stats::setNames("precisao", precisao_col)]

  unmapped <- is.na(dt[[score_col]]) & !is.na(dt[[precisao_col]])
  if (any(unmapped)) {
    warning("Precision values not mapped to scores: ", paste(unique(dt[[precisao_col]][unmapped]), collapse = ", "))
  }

  dt[]
}

classificar_precisao_best <- function(dt, precisao_best_col = "precisao_best", classe_col = "classe") {
  stopifnot(data.table::is.data.table(dt))
  require_columns(dt, precisao_best_col, "dt")

  dt[, (classe_col) := data.table::fifelse(
    is.na(get(precisao_best_col)), NA_integer_,
    data.table::fifelse(
      get(precisao_best_col) %in% c("db01", "dm01"), 2L,
      data.table::fifelse(get(precisao_best_col) %in% c("dc01", "dc02"), 1L, 0L)
    )
  )]

  dt[]
}
