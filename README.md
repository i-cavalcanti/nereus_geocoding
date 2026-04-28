# RAIS Establishment Geocoding Pipeline

This repository contains an R pipeline for geocoding Brazilian RAIS establishment records and improving geocoding consistency across years. It is designed for large administrative datasets where establishment addresses may be incomplete, noisy, or recorded inconsistently over time.

The project has two main goals:

1. Produce the best available geocoded location for each establishment-year observation.
2. Use the longitudinal structure of RAIS to correct weaker geocoding results when the same establishment has a better and consistent location in another year.

Reusable functions are separated from execution scripts, machine-specific paths are centralized in one configuration file, and package dependencies can be restored with `renv`.

## Geocoding engine

This project is built around the R package [`geocodebr`](https://github.com/ipeaGIT/geocodebr). The package geocodes Brazilian addresses using open spatial reference data, especially the Cadastro Nacional de Endereços para Fins Estatísticos (CNEFE), published by IBGE.

The pipeline uses two core `geocodebr` operations:

- `geocodebr::definir_campos()`: maps columns in a table to address components such as street, number, CEP, municipality, and state.
- `geocodebr::geocode()`: geocodes the input addresses and returns precision indicators and, when requested, spatial geometries.

This repository does not replace `geocodebr`. It adds a RAIS-specific workflow around it: address standardization, multiple geocoding attempts, precision scoring, CEP-level quality checks, and multi-year consistency correction.

## Repository structure

```text
.
├── R/
│   ├── 00_load.R
│   ├── address_cleaning.R
│   ├── data_standardization.R
│   ├── first_stage.R
│   ├── geocode_pipeline.R
│   ├── logging.R
│   ├── precision.R
│   ├── quality_filters.R
│   ├── second_stage.R
│   └── utils.R
├── config/
│   └── config_example.R
├── docs/
│   └── run_log_dictionary.md
├── scripts/
│   ├── 00_setup_renv.R
│   ├── 01_run_first_stage.R
│   ├── 02_run_second_stage.R
│   ├── _paths.R
│   └── run_all.R
├── README.md
└── .gitignore
```

The `R/` folder contains reusable functions only. It should not contain local paths, package installation calls, or concrete region/year execution loops.

The `scripts/` folder contains runnable workflows.

The `config/` folder contains machine-specific paths, file-name templates, regions, CRS choices, thresholds, and filters.


## File-name templates

Input files no longer need to be named exactly as `<region>_estb_<year>_limpo.csv`. File names are controlled by templates in `config/config_example.R`.

The available placeholders are:

- `{prefix}`: region code, such as `sp`, `nordeste`, or `sul`.
- `{year}`: year, such as `2010` or `2024`.

Default templates:

```r
input_csv_template <- "{prefix}_estb_{year}_limpo.csv"
prev_csv_template <- input_csv_template
first_stage_rds_template <- "dt_f_{prefix}_{year}.rds"
first_stage_rds_pattern <- "^dt_f_{prefix}_(\\d{4})\\.rds$"
final_rds_template <- "{prefix}_estb_{year}_limpo_corr.rds"
```

Example: if your raw files are named `rais_sp_2010.csv`, set:

```r
input_csv_template <- "rais_{prefix}_{year}.csv"
prev_csv_template <- input_csv_template
```

If you change `first_stage_rds_template`, also update `first_stage_rds_pattern` so the second stage can find the first-stage RDS files and extract the year.

## Expected input columns

Each input CSV must contain at least:

```text
id
municipio
cduf
cep
endereco
estoque
identificad_m
matrizfilial
```

The following columns are optional; if absent, they are created as missing values:

```text
bairro
numlograd
```

If `filter_cnae` is used, the input must also contain:

```text
clascnae20
```

## Distance CRS by region

Distance calculations must be done in a projected CRS with units in meters. The project uses a configurable CRS dictionary by region:

```r
region_distance_crs <- c(
  sp = 31983,
  centrooeste = 5880,
  norte = 5880,
  nordeste = 5880,
  sul = 31982,
  mgesrj = 31983,
  ni = 5880
)

default_distance_crs <- 5880
```

These values are used in first-stage CEP dispersion checks through `cep_distance_crs`.

`EPSG:5880` is SIRGAS 2000 / Brazil Polyconic. It uses meter units and covers Brazil, so it is a useful fallback for broad regions or regions spanning multiple UTM zones.

Some smaller regions can use regional SIRGAS 2000 / UTM CRS values. For example:

- `EPSG:31982`: SIRGAS 2000 / UTM zone 22S.
- `EPSG:31983`: SIRGAS 2000 / UTM zone 23S.
- `EPSG:31984`: SIRGAS 2000 / UTM zone 24S.

The current dictionary is a practical default, not a universal rule. If a region is split across multiple UTM zones, `5880` is safer as a broad projected CRS. If analysis is restricted to a smaller area, a local UTM CRS may be preferable.

## Pipeline stages

### First stage

For each region and year, the first stage:

1. Reads the cleaned RAIS establishment CSV using `input_csv_template`.
2. Creates standardized address fields.
3. Runs several geocoding variants: raw, standardized, and cleaned logradouro/number versions.
4. Scores the precision of each result.
5. Keeps the best result per observation.
6. Applies CEP-level consistency checks.
7. Saves one RDS per region-year using `first_stage_rds_template`.

Default output example:

```text
dt_f_sp_2010.rds
```

### Second stage

For each region, the second stage:

1. Reads all first-stage RDS files matching `first_stage_rds_pattern`.
2. Re-reads the original cleaned CSVs using `prev_csv_template`.
3. Stacks all available years.
4. Identifies the best geocoded observation for each establishment ID.
5. Imputes better address and geometry information to weaker observations when they are consistent by municipality, matriz/filial status, string similarity, or geographic distance.
6. Saves corrected yearly RDS files using `final_rds_template`.

Default output example:

```text
sp_estb_2010_limpo_corr.rds
```

## CEP consistency rule

Geocoding precision is converted into broad classes:

```text
classe == 0: high precision; accepted directly
classe == 1: CEP-level precision; accepted only if the CEP passes the consistency rule
classe == 2: low precision; rejected
```

For CEP-level results, the pipeline geocodes the CEP and computes dispersion among returned points. A CEP is accepted when its dispersion is below `sd_threshold_km`.

Single-point CEPs are accepted by default:

```r
accept_single_point_cep = TRUE
```

This is a pragmatic choice. A single returned point does not provide evidence of internal spatial disagreement, but it also does not allow dispersion to be estimated. For stricter sensitivity checks, set:

```r
accept_single_point_cep = FALSE
```

## Run log

The pipeline writes a lightweight append-only CSV log. The default path is configured in `config/config_example.R`:

```r
log_dir <- "D:/Arq-Azzoni/UrbanSprawl/Bases_dados/RAIS_estab/temp_geocoding/logs"
log_file <- file.path(log_dir, "run_log.csv")
```

The log is designed for reproducibility and troubleshooting, not for final analysis. Each row corresponds to one stage-region-year attempt.

A column-by-column description is available in `docs/run_log_dictionary.md`.

The log records:

```text
run_id
stage
region
year
status
message
input_file
output_file
started_at
ended_at
runtime_seconds
input_rows
rows_after_filters
output_rows
accepted_rows
rejected_rows
class0_rows
class1_rows
class2_rows
cep_rows
cep_accepted
cep_rejected
cep_single_point_accepted
cep_single_point_rejected
imputed_rows
geometry_nonmissing
params
timestamp_written
```

Typical `status` values are:

```text
success
missing_input_file
output_exists
error
```

The same `run_id` is used across first and second stages when running `scripts/run_all.R`. To set a fixed ID manually, define it in the config:

```r
run_id <- "baseline_2026_04_28"
```

If `run_id = NULL`, the scripts create a timestamped ID automatically.

The log file is append-only. If you want a clean log for a new experiment, rename or delete the previous `run_log.csv`, or change `log_file` in the config.

## Key parameters

| Parameter | Meaning |
|---|---|
| `regions` | Region prefixes to process. |
| `years` | Years to process. |
| `input_csv_template` | Template used to find raw input CSVs. |
| `prev_csv_template` | Template used to re-read source CSVs in the second stage. |
| `first_stage_rds_template` | Template for first-stage RDS outputs. |
| `first_stage_rds_pattern` | Regex used by the second stage to find first-stage RDS files and extract years. |
| `final_rds_template` | Template for final corrected RDS outputs. |
| `log_file` | Path to the append-only CSV run log. |
| `run_id` | Optional fixed run identifier; if `NULL`, scripts create one automatically. |
| `region_distance_crs` | Named CRS dictionary used for distance calculations. |
| `sd_threshold_km` | Maximum CEP dispersion allowed for CEP-level geocodes. |
| `accept_single_point_cep` | Whether to accept CEPs with only one returned point. |
| `diff_threshold` | Maximum normalized string distance for second-stage imputation. |
| `geo_threshold_m` | Maximum geographic distance in meters for second-stage imputation. |
| `filter_municipio_7` | Optional filter for 7-digit municipality codes. |
| `filter_cnae` | Optional CNAE filter. |
| `overwrite` | Whether to overwrite existing outputs. |

## Reproducing the run

### 1. Restore dependencies

On a new machine:

```r
install.packages("renv")
renv::restore()
```

To initialize the project environment for the first time:

```r
source("scripts/00_setup_renv.R")
```

### 2. Edit the config

Edit:

```text
config/config_example.R
```

At minimum, check:

- `raw_csv_dir`
- `first_stage_out`
- `final_out`
- `log_file`
- file-name templates
- `regions`
- `years`
- `region_distance_crs`

### 3. Run the full pipeline

From the project root:

```r
source("scripts/run_all.R")
```

Or run each stage separately:

```r
source("scripts/01_run_first_stage.R")
source("scripts/02_run_second_stage.R")
```

## Outputs

First-stage intermediate files are saved to `first_stage_out`.

Final corrected files are saved to `final_out`.

The audit log is saved to `log_file`.

Large data files and generated outputs should not be committed to the repository. Keep only code, configuration examples, documentation, and dependency files under version control.
