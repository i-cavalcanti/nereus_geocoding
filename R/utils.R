project_log <- function(verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), paste0(...))
  }
}

require_columns <- function(dt, cols, context = "object") {
  missing <- setdiff(cols, names(dt))
  if (length(missing) > 0L) {
    stop(context, " is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

build_template <- function(template, ...) {
  values <- list(...)
  out <- template
  for (nm in names(values)) {
    out <- gsub(paste0("\\{", nm, "\\}"), as.character(values[[nm]]), out)
  }
  out
}

index_files_by_year <- function(dir_path, pattern, label = "file") {
  if (!dir.exists(dir_path)) stop("Directory does not exist: ", dir_path, call. = FALSE)

  files <- list.files(dir_path, pattern = pattern, full.names = TRUE)
  if (length(files) == 0L) {
    stop("No ", label, " found in ", dir_path, " with pattern: ", pattern, call. = FALSE)
  }

  bases <- basename(files)
  matches <- regmatches(bases, regexec(pattern, bases))
  ok <- lengths(matches) >= 2L
  if (!all(ok)) {
    stop("Could not extract year from: ", paste(bases[!ok], collapse = ", "), call. = FALSE)
  }

  years <- suppressWarnings(as.integer(vapply(matches, `[`, character(1), 2L)))
  if (anyNA(years)) {
    stop("Invalid year in: ", paste(bases[is.na(years)], collapse = ", "), call. = FALSE)
  }

  duplicated_years <- unique(years[duplicated(years)])
  if (length(duplicated_years) > 0L) {
    stop("Multiple ", label, " files for years: ", paste(sort(duplicated_years), collapse = ", "), call. = FALSE)
  }

  out <- stats::setNames(files, as.character(years))
  out[order(as.integer(names(out)))]
}

fill_from_suffix <- function(dt, suffix = ".in") {
  cols_suffix <- names(dt)[endsWith(names(dt), suffix)]

  for (col_suffix in cols_suffix) {
    col_base <- substr(col_suffix, 1L, nchar(col_suffix) - nchar(suffix))
    if (col_base %in% names(dt)) {
      idx <- which(is.na(dt[[col_base]]) & !is.na(dt[[col_suffix]]))
      if (length(idx) > 0L) {
        data.table::set(dt, i = idx, j = col_base, value = dt[[col_suffix]][idx])
      }
      dt[, (col_suffix) := NULL]
    } else {
      data.table::setnames(dt, col_suffix, col_base)
    }
  }

  dt[]
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

get_named_config_value <- function(values, name, default = NULL, label = "value") {
  if (is.null(values)) {
    if (!is.null(default)) return(default)
    stop(label, " is not configured.", call. = FALSE)
  }

  if (is.list(values)) {
    if (name %in% names(values)) return(values[[name]])
  } else {
    if (!is.null(names(values)) && name %in% names(values)) return(values[[name]])
  }

  if (!is.null(default)) return(default)

  stop(label, " is missing an entry for '", name, "'.", call. = FALSE)
}
