library(data.table)


# paths
rds_path <- "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/rds_por_ano/sp_estb_2024_limpo_corr.rds"
csv_path <- "D:/Arq-Azzoni/RAIS/rais-geocoding/data/raw/rais_temp/sp_estb_2024_limpo.csv"

# load files
dt_rds <- as.data.table(readRDS(rds_path))
dt_csv <- fread(csv_path)

# merge by id
dt_merged <- merge(
    dt_rds, 
    dt_csv,
  by = "id",
  all = FALSE   # only matched rows; use all.x=TRUE if you want keep all from RDS
)

# list of clascnae20 values to keep
keep_list <-  c(56112, 56201, 56121, 47211, 47296, 10911, 47121, 47237, 47229)

# filter where clascnae20 is in the list
dt_filtered <- dt_merged[clascnae20 %in% keep_list]

# view result
dt_filtered


dt_csv_nonzero <- dt_csv[estoque != 0]


dt_merged_

t <- head(dt_rds, 1000)




###############################################

library(data.table)

rds_path <- "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/rds_por_ano/sp_estb_2024_limpo_corr.rds"

dt_rds <- as.data.table(readRDS(rds_path))

filter_municipio_7 = c("3525904","3543402","3529005","3534708")

filter_municipio_7 = c("3525904") #Jundiai

dt_mun_2024 <- dt_rds[municipio_7 %in% filter_municipio_7]


dt_mun <- dt_rds[municipio_7 %in% filter_municipio_7]

dt_mun_2024[, sobreposto := as.integer(duplicated(geometry_updated) | duplicated(geometry_updated, fromLast = TRUE))]


dt_mun_2024[, sobreposto := as.integer(duplicated(geometry_updated) | duplicated(geometry_updated, fromLast = TRUE))]
dt_mun_2024[, sobre_grupos := NA_integer_]
dt_mun_2024[sobreposto == 1, sobre_grupos := match(geometry_updated, unique(geometry_updated[sobreposto == 1]))]






soma_grupos <- dt_mun_2024[sobreposto == 1,
  .(
    estoque_total = sum(estoque, na.rm = TRUE),
    n_empresas = uniqueN(identificad_m),
    geometria = geometry_updated[1]
  ),
  by = sobre_grupos
][order(sobre_grupos)]


soma_grupos_1 <- dt_mun_2024[sobreposto == 1  & aceito_updated == 1,
  .(
    estoque_total = sum(estoque, na.rm = TRUE),
    n_empresas = uniqueN(identificad_m),
    geometria = geometry_updated[1]
  ),
  by = sobre_grupos
][order(sobre_grupos)]

soma_grupos_0 <- dt_mun_2024[sobreposto == 1  & aceito_updated == 0,
  .(
    estoque_total = sum(estoque, na.rm = TRUE),
    n_empresas = uniqueN(identificad_m),
    geometria = geometry_updated[1]
  ),
  by = sobre_grupos
][order(sobre_grupos)]


dt_mun_2024[, `:=`(
  estoque_total = NA_real_,
  n_empresas = NA_integer_
)]

dt_mun_2024[soma_grupos, on = "sobre_grupos", `:=`(
  estoque_total = i.estoque_total,
  n_empresas = i.n_empresas
)]

dt_mun_2024[sobreposto != 1, `:=`(
  estoque_total = NA_real_,
  n_empresas = NA_integer_
)]

dt_sobreposto <- dt_mun_2024[sobreposto == 1]

dt_sobreposto_1 <- dt_sobreposto[aceito_updated == 1]

dt_sobreposto_0 <- dt_sobreposto[aceito_updated == 0]

dt_aceito_0 <- dt_mun_2024[aceito_updated == 0]


setorder(dt_mun_2024, is.na(n_empresas), -n_empresas)



dt_aceito_0 <- dt_mun[aceito_updated == 0]
dt_filtrado <- dt_aceito_0[estoque >= 20]

library(writexl)

write_xlsx(dt_filtrado, "dt_filtrado.xlsx")


fwrite(dt_filtrado, "dt_filtrado_2.csv", sep = ";", bom = TRUE)

a <- dt_mun[, .N, by = aceito_updated][
  , proporcao := 100 * N / sum(N)
]


dt_mun_err <- dt_mun[aceito_updated==0]

dt_mun_err <- dt_mun_err[order(-estoque)]



t<- head()

names(dt_mun_2023)


result <- merge(
  dt_mun_2023,
  dt_mun_2024[, .(identificad_m, endereco_best, estoque, municipio, year)],
  by = "identificad_m",
  all.x = TRUE
)

bad_rows <- result[is.na(estoque.y)]


only_in_23 <- dt_mun_2023[!dt_mun_2024, on = "identificad_m"]

only_in_24 <- dt_mun_2024[!dt_mun_2023, on = "identificad_m"]



# pasta onde estão os .rds
pasta <- "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/rds_por_ano"

# municípios de interesse
filter_municipio_7 <- c("3525904", "3543402", "3529005", "3534708")

# listar arquivos no padrão desejado
arquivos <- list.files(
  path = pasta,
  pattern = "^sp_estb_[0-9]{4}_limpo_corr\\.rds$",
  full.names = TRUE
)

# função para processar 1 arquivo
processa_arquivo <- function(arq) {
  ano <- sub("^sp_estb_([0-9]{4})_limpo_corr\\.rds$", "\\1", basename(arq))
  
  dt_rds <- readRDS(arq)
  setDT(dt_rds)
  
  # garantir tipo compatível
  dt_rds[, municipio_7 := as.character(municipio_7)]
  
  # filtrar e agregar
  dt_mun <- dt_rds[
    municipio_7 %in% filter_municipio_7,
    .(estoque = sum(estoque, na.rm = TRUE)),
    by = municipio_7
  ]
  
  dt_mun[, ano := as.integer(ano)]
  setcolorder(dt_mun, c("ano", "municipio_7", "estoque"))
  
  dt_mun
}

# aplicar em todos os arquivos e juntar resultados
resultado_final <- rbindlist(lapply(arquivos, processa_arquivo), use.names = TRUE, fill = TRUE)

# ordenar
setorder(resultado_final, ano, municipio_7)

library(ggplot2)

resultado_final[, municipio_nome := fifelse(municipio_7 == "3525904", "Jundiaí",
                                 fifelse(municipio_7 == "3543402", "Ribeirão Preto",
                                 fifelse(municipio_7 == "3529005", "Marília",
                                 fifelse(municipio_7 == "3534708", "Ourinhos", municipio_7))))]

ggplot(resultado_final, aes(x = ano, y = estoque, color = municipio_nome, group = municipio_nome)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Soma do estoque por município ao longo dos anos",
    x = "Ano",
    y = "Estoque",
    color = "Município"
  ) +
  theme_minimal()
