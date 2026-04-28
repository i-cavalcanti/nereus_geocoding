library(data.table)

folder <- "D:/Bases/MTE/Estab"

files <- list.files(
  folder,
  pattern = "^Estb[0-9]{4}_limpo\\.dta$",
  full.names = TRUE
)

# Extract year from filename
years <- as.integer(sub("^Estb([0-9]{4})_limpo\\.dta$", "\\1", basename(files)))

# Read each file and compute yearly stats
out_list <- vector("list", length(files))
all_ids <- list()

for (i in seq_along(files)) {
  dt <- haven::read_dta(files[i], col_select = "identificad_m")
  setDT(dt)

  out_list[[i]] <- data.table(
    year = years[i],
    n_rows = nrow(dt),
    n_unique_identificad_m_year = uniqueN(dt$identificad_m, na.rm = TRUE)
  )

  all_ids[[i]] <- unique(na.omit(dt$identificad_m))
}

summary_dt <- rbindlist(out_list)
setorder(summary_dt, year)

overall_unique <- uniqueN(unlist(all_ids, use.names = FALSE), na.rm = TRUE)

summary_dt[, n_unique_identificad_m_overall := overall_unique]

print(summary_dt)

sum(summary_dt$n_rows, na.rm = TRUE)

fwrite(
  summary_dt,
  file.path(folder, "summary_identificad_m_by_year.csv")
)


library(data.table)
library(haven)

folder <- "D:/Bases/MTE/Vinc"

regions <- c("sp", "centrooeste", "norte", "nordeste", "sul", "mgesrj", "ni")

files <- list.files(
  folder,
  pattern = paste0("^(", paste(regions, collapse = "|"), ")[0-9]{4}_limpo\\.dta$"),
  full.names = TRUE
)

# Extract region and year from filename
file_info <- data.table(
  file = files,
  filename = basename(files)
)

file_info[, region := sub("^([a-z]+)[0-9]{4}_limpo\\.dta$", "\\1", filename)]
file_info[, year := as.integer(sub("^[a-z]+([0-9]{4})_limpo\\.dta$", "\\1", filename))]

setorder(file_info, year, region)

out_list <- vector("list", nrow(file_info))
all_ids_by_year <- list()

for (i in seq_len(nrow(file_info))) {
  dt <- read_dta(file_info$file[i], col_select = "identificad_m")
  setDT(dt)

  yr <- file_info$year[i]

  out_list[[i]] <- data.table(
    year = yr,
    region = file_info$region[i],
    n_rows = nrow(dt),
    n_unique_identificad_m_region_year = uniqueN(dt$identificad_m, na.rm = TRUE)
  )

  all_ids_by_year[[as.character(yr)]] <- c(
    all_ids_by_year[[as.character(yr)]],
    dt$identificad_m
  )
}

# Region-level intermediate table
region_year_dt <- rbindlist(out_list)

# Aggregate rows across regions by year
summary_dt <- region_year_dt[
  ,
  .(
    n_rows = sum(n_rows),
    n_unique_identificad_m_year = uniqueN(
      unlist(all_ids_by_year[[as.character(year)]], use.names = FALSE),
      na.rm = TRUE
    )
  ),
  by = year
]

setorder(summary_dt, year)

# Overall unique identificad_m across all years and regions
overall_unique <- uniqueN(
  unlist(all_ids_by_year, use.names = FALSE),
  na.rm = TRUE
)

summary_dt[, n_unique_identificad_m_overall := overall_unique]

print(summary_dt)


sum(summary_dt$n_rows, na.rm = TRUE)

fwrite(
  summary_dt,
  file.path(folder, "summary_identificad_m_by_year.csv")
)


library(haven)

file_one <- "D:/Bases/MTE/Vinc/centrooeste2002_limpo.dta"

column_names <- names(read_dta(file_one, n_max = 0))

column_names

folder <- "D:/Bases/MTE/Vinc"

regions <- c("sp", "centrooeste", "norte", "nordeste", "sul", "mgesrj", "ni")

files <- list.files(
  folder,
  pattern = paste0("^(", paste(regions, collapse = "|"), ")[0-9]{4}_limpo\\.dta$"),
  full.names = TRUE
)

all_cpfs <- list()

for (i in seq_along(files)) {
  dt <- read_dta(files[i], col_select = "cpf_m")
  setDT(dt)

  all_cpfs[[i]] <- unique(na.omit(dt$cpf_m))
}

n_unique_cpf_m_overall <- uniqueN(
  unlist(all_cpfs, use.names = FALSE),
  na.rm = TRUE
)

n_unique_cpf_m_overall
#103338581


##################

library(data.table)

folder <- "D:/Bases/MTE/Vinc"

regions <- c("sp", "centrooeste", "norte", "nordeste", "sul", "mgesrj", "ni")

files <- list.files(
  folder,
  pattern = paste0("^(", paste(regions, collapse = "|"), ")[0-9]{4}_limpo\\.dta$"),
  full.names = TRUE
)

file_sizes <- file.info(files)

total_gb <- sum(file_sizes$size, na.rm = TRUE) / 1024^3

total_gb


#####################

folder <- "D:/Bases/MTE/Estab"

files <- list.files(
  folder,
  pattern = "^Estb[0-9]{4}_limpo\\.dta$",
  full.names = TRUE
)

file_sizes <- file.info(files)

total_gb <- sum(file_sizes$size, na.rm = TRUE) / 1024^3

total_gb
