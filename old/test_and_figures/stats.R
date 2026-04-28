library(data.table)



a <- "D:\\Arq-Azzoni\\UrbanSprawl\\Bases_dados\\RAIS_estab\\temp_geocoding\\csv_por_ano\\sp_estb_2023_limpo_corr.csv"

d <- fread(a)


dir <- "D:\\Arq-Azzoni\\UrbanSprawl\\Bases_dados\\RAIS_estab\\temp_geocoding\\rds_por_ano"  # ajuste p/ sua pasta real

# pega todos os CSVs no padrão *_estb_YYYY_limpo_corr.csv
files <- list.files(
  dir,
  pattern = "_estb_\\d{4}_limpo_corr\\.rds$",
  full.names = TRUE
)

# lê, cria coluna year a partir do nome do arquivo, e empilha tudo
dt_all <- rbindlist(lapply(files, function(f) {
  x <- readRDS(f)
  setDT(x)
  yr <- as.integer(sub(".*_estb_(\\d{4})_limpo_corr\\.rds$", "\\1", basename(f)))
  x[, year := yr]
  x
}), use.names = TRUE, fill = TRUE)

# dt_all fica na memória
dt_all[]

t <- head(dt_all, 1000)

dt_all[, .(N = .N), by = imputada][
  , perc := 100 * N / sum(N)
][order(-N)]


dt_all[, .(N = .N), by = .(year, imputada)
       ][, perc := 100 * N / sum(N), by = year
       ][order(year, -N)]


dt_all[, .(N = .N), by = aceito][
  , perc := 100 * N / sum(N)
][order(-N)]


dt_all[, .(N = .N), by = aceito_updated][
  , perc := 100 * N / sum(N)
][order(-N)]

dt_all[aceito == 0 & aceito_updated == 1,
       .(N_total = .N,
         N_ok = sum(imputada == 1, na.rm = TRUE),
         N_viol = sum(imputada != 1 | is.na(imputada)))]

dt_all[imputada == 1,
       .(perc = 100 * mean(aceito == 0 & aceito_updated == 1, na.rm = TRUE),
         N = .N)]

dt_all[imputada == 1,
       .(perc = 100 * mean(aceito == 0 & aceito_updated == 1, na.rm = TRUE),
         N = .N),
       by = year][order(year)]



# Lendo os arquivos RDS gerados no primeiro estágio

dir <- "D:\\Arq-Azzoni\\UrbanSprawl\\Bases_dados\\RAIS_estab\\temp_geocoding\\geocoded_rd_test"  # ajuste p/ sua pasta real


files <- list.files(dir, pattern = "^dt_f_\\d{4}\\.rds$", full.names = TRUE)

dt_all <- rbindlist(lapply(files, function(f) {
  x <- readRDS(f)
  setDT(x)  # garante data.table
  yr <- as.integer(sub("^dt_f_(\\d{4})\\.rds$", "\\1", basename(f)))
  x[, year := yr]
  x
}), use.names = TRUE, fill = TRUE)

dt_all[]

dt_sub <- dt_all[year %in% c(2019L, 2020L, 2021L) & imputada == 0 & aceito_updated == 0]

t <- head(dt_sub, 1000)



dt_all[, .(N = .N), by = best_step][
  , perc := 100 * N / sum(N)
][order(-N)]


b <- dt_all[, .(N = .N), by = .(year, best_step)
       ][, perc := 100 * N / sum(N), by = year
       ][order(year, -N)]


################

dir <- "D:\\Arq-Azzoni\\UrbanSprawl\\Bases_dados\\RAIS_estab\\temp_geocoding\\rds_por_ano"  # ajuste p/ sua pasta real

files <- list.files(
  dir,
  pattern = "_estb_\\d{4}_.*\\.rds$",
  full.names = TRUE
)

# ler + empilhar + criar year pelo nome do arquivo
dt_all <- rbindlist(lapply(files, function(f) {
  x <- readRDS(f)
  setDT(x)
  yr <- as.integer(sub(".*_estb_(\\d{4})_.*\\.rds$", "\\1", basename(f)))
  x[, year := yr]
  x
}), use.names = TRUE, fill = TRUE)

setorder(dt_all, identificad_m)
t <- head(dt_all, 1000)

# Proporção (percentual) de cada valor de aceito_updated (geral)
prop_aceito_updated <- dt_all[, .(N = .N), by = aceito_updated][
  , prop := N / sum(N)
][order(-N)]

# (opcional) por ano
prop_aceito_updated_year <- dt_all[, .(N = .N), by = .(year, aceito_updated)][
  , prop := N / sum(N), by = year
][order(year, -N)]

prop_aceito_updated
prop_aceito_updated_year

# soma de estoque por aceito_updated + proporção da soma
estoque_por_aceito_updated <- dt_all[
  , .(estoque_sum = sum(estoque, na.rm = TRUE)),
  by = aceito_updated
][
  , prop_estoque := estoque_sum / sum(estoque_sum)
][order(-estoque_sum)]

estoque_por_aceito_updated

estoque_por_ano <- dt_all[
  , .(estoque_sum = sum(estoque, na.rm = TRUE)),
  by = .(year, aceito_updated)
][
  , prop_estoque := estoque_sum / sum(estoque_sum),
  by = year
][order(year, -estoque_sum)]

estoque_por_ano

library(ggplot2)

plot_dt <- estoque_por_ano[aceito_updated == 1]

ggplot(plot_dt, aes(x = year, y = prop_estoque)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  #coord_fixed(ratio = 1) +
  labs(x = "Ano", y = "Proporção do estoque (aceito_updated == 1)")


plot_dt <- prop_aceito_updated_year[aceito_updated == 1]

ggplot(plot_dt, aes(x = year, y = prop)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  #coord_fixed(ratio = 1) +
  labs(x = "Ano", y = "Proporção do estoque (aceito_updated == 1)")
