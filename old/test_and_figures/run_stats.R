library(data.table)

dt <- readRDS("D:/Arq-Azzoni/UrbanSprawl/Ifood_2026/dados/localizado_ifood_longo.rds")

t <- head(dt, 1000)

dt <- dt[is_mei_ifood == FALSE]


dt <- readRDS("D:/Arq-Azzoni/UrbanSprawl/Ifood_2026/dados/restaurantes_longo.rds")

dt_ok <- dt[
  ativo_rfb == 1 &
  ativo_rais == 1 &
  ativo_ifood == 1 &
  nao_mei_rfb == 1 &
  nao_mei_rais == 1 &
  nao_mei_ifood == 1
]

dt_ok <- dt[
  ativo_rais == 1 &
  ativo_ifood == 1 

]

unique_id <- unique(dt$id_m)


uni_id <- unique(dt_filtrado$identificad_m)

names(dt_filtrado)

t <- head(dt_filtrado, 1000)

unique_id <- unique(dt$id_m)

unique_cnae <- unique(dt$cnae_fipe)

x <- trimws(unlist(unique_cnae))   # list -> character vector, remove spaces
first5 <- substr(x, 1, 5)  

bad <- unique(first5[!grepl("^\\d{5}$", first5)])
bad

first5_ok <- first5[grepl("^\\d{5}$", first5)]
obj <- unique(as.integer(first5_ok))


obj <- unique(as.integer(substr(unlist(unique_cnae), 1, 5)))
obj_num <- as.numeric(obj)




dt <- readRDS("D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/rds_por_ano/centrooeste_estb_2008_limpo_corr.rds")


pasta_rds <- "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/rds_por_ano"

pasta_t <- "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/filtro_2"
# vetor de strings para filtrar em identificad_m
ids_alvo <- unique_id


# pasta temporária para shards filtrados
pasta_tmp <- file.path(pasta_t, "_tmp_filtrados_identificad_m_")
dir.create(pasta_tmp, recursive = TRUE, showWarnings = FALSE)

# lista arquivos de entrada
arquivos <- list.files(pasta_rds, pattern = "\\.rds$", full.names = TRUE)

# opcional: limpar shards antigos
unlink(list.files(pasta_tmp, pattern = "\\.rds$", full.names = TRUE))

k <- 0L

for (f in arquivos) {
  dt <- as.data.table(readRDS(f))
  
  if (!("identificad_m" %in% names(dt))) next
  
  dt[, identificad_m := as.character(identificad_m)]
  
  out <- dt[identificad_m %in% ids_alvo]
  
  if (nrow(out) > 0L) {
    out[, arquivo_origem := basename(f)]
    
    k <- k + 1L
    arq_tmp <- file.path(pasta_tmp, sprintf("chunk_%04d.rds", k))
    saveRDS(out, arq_tmp)
  }
  
  rm(dt, out)
  gc()
}

# consolidar em um único data.table
arquivos_tmp <- list.files(pasta_tmp, pattern = "^chunk_\\d+\\.rds$", full.names = TRUE)

dt_filtrado <- if (length(arquivos_tmp) > 0L) {
  rbindlist(lapply(arquivos_tmp, readRDS), use.names = TRUE, fill = TRUE)
} else {
  data.table()
}

# salvar consolidado final (opcional)
saveRDS(dt_filtrado, file.path(pasta_t, "dt_filtrado_identificad_m_3.rds"))

dt_filtrado

t <- head(dt_filtrado, 1000)

dt_filtrado[, .N, by = aceito_updated][order(aceito_updated)]

tab_aceito <- dt_filtrado[
  !is.na(aceito_updated),
  .N,
  by = aceito_updated
][order(aceito_updated)]

tab_aceito[, pct := 100 * N / sum(N)]

tab_estoque <- dt_filtrado[
  !is.na(aceito_updated) & aceito_updated %in% c(0, 1) & !is.na(estoque),
  .(estoque_total = sum(estoque, na.rm = TRUE)),
  by = aceito_updated
][order(aceito_updated)]

tab_estoque[, pct_estoque := 100 * estoque_total / sum(estoque_total)]

tab_estoque


tab_estoque <- dt_filtrado[
  year >= 2012 &
    !is.na(aceito_updated) & aceito_updated %in% c(0, 1),
  .(
    n = .N,
    estoque_total = sum(estoque, na.rm = TRUE)
  ),
  by = aceito_updated
][order(aceito_updated)]

tab_estoque[, `:=`(
  pct_linhas = 100 * n / sum(n),
  pct_estoque = 100 * estoque_total / sum(estoque_total)
)]

tab_estoque


tabulate_aceito_updated_in_folder <- function(folder_path, pattern = "\\.rds$") {
  if (!dir.exists(folder_path)) stop("Folder does not exist: ", folder_path)
  
  files <- list.files(folder_path, pattern = pattern, full.names = TRUE)
  if (length(files) == 0L) stop("No .rds files found in: ", folder_path)
  
  # Per-file tabulation
  per_file <- rbindlist(lapply(files, function(f) {
    obj <- readRDS(f)
    dt  <- as.data.table(obj)
    
    if (!("aceito_updated" %in% names(dt))) {
      return(data.table(
        file = basename(f),
        aceito_updated = NA,
        N = 0L,
        note = "column aceito_updated not found"
      ))
    }
    
    tab <- dt[, .N, by = aceito_updated][order(aceito_updated)]
    tab[, file := basename(f)]
    tab[, note := NA_character_]
    setcolorder(tab, c("file", "aceito_updated", "N", "note"))
    tab
  }), fill = TRUE, use.names = TRUE)
  
  # Combined tabulation across all files
  combined <- per_file[
    is.na(note),
    .(N = sum(N, na.rm = TRUE)),
    by = aceito_updated
  ][order(aceito_updated)]
  
  if (nrow(combined) > 0) {
    combined[, pct := 100 * N / sum(N)]
  }
  
  list(
    per_file = per_file,
    combined = combined
  )
}


# Example
res <- tabulate_aceito_updated_in_folder(
  "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/rds_por_ano"
)
res$per_file     # table by file
res$combined     # total across all files
