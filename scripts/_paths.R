resolve_project_path <- function(path, project_root = PROJECT_ROOT) {
  if (is.null(path) || !nzchar(path)) {
    stop("Path is missing or empty.", call. = FALSE)
  }

  is_absolute <- grepl("^([A-Za-z]:|/|~)", path)
  out <- if (is_absolute) path else file.path(project_root, path)
  normalizePath(out, winslash = "/", mustWork = FALSE)
}
