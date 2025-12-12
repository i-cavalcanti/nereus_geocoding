library("data.table")
library("geocodebr")
library("sf")



dt <- as.data.table(readRDS("D:\\Arq-Azzoni\\UrbanSprawl\\Bases_dados\\RAIS_estab\\Niteroi\\rais_niteroi_2009_2023.rds"))

dt_f <- dt3[dt3$precisao %in% c("cep"), ]
dt_f <- data.table(dt_f)
13624/379858
t <- head(dt_f, 1000)
