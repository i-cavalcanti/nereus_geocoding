if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE)
}

renv::install(c(
  "data.table",
  "sf",
  "stringi",
  "stringdist",
  "enderecobr",
  "geocodebr"
))

renv::snapshot(prompt = FALSE)
