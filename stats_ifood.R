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

dt_mun <- dt_rds[municipio_7 %in% filter_municipio_7]

a <- dt_mun[, .N, by = aceito_updated][
  , proporcao := 100 * N / sum(N)
]


dt_mun_err <- dt_mun[aceito_updated==0]

dt_mun_err <- dt_mun_err[order(-estoque)]



t<- head()