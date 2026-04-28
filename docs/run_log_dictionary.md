# Run log dictionary

The pipeline writes an append-only CSV log to the path configured as `log_file` in `config/config_example.R`. Each row represents one attempt to process a stage-region-year combination.

| Column | Meaning |
|---|---|
| `run_id` | Identifier shared by all rows from the same pipeline run. |
| `stage` | Pipeline stage: `first_stage` or `second_stage`. |
| `region` | Region prefix processed. |
| `year` | Year processed. |
| `status` | Outcome: `success`, `missing_input_file`, `output_exists`, `error`, or `no_output_rows`. |
| `message` | Error or status message, when relevant. |
| `input_file` | Input file path. In the second stage, the first-stage RDS and source CSV are separated by `;`. |
| `output_file` | Intended or written output file path. |
| `started_at` | Start time for the stage-region-year attempt. |
| `ended_at` | End time for the stage-region-year attempt. |
| `runtime_seconds` | Elapsed time in seconds. |
| `input_rows` | Number of rows read from the main input file. |
| `rows_after_filters` | Number of rows after filtering or merging. In the second stage, this is the merged row count before longitudinal correction. |
| `output_rows` | Number of rows written to the output file. |
| `accepted_rows` | Number of accepted rows. In the second stage, this uses `aceito_updated`. |
| `rejected_rows` | Number of rejected rows. In the second stage, this uses `aceito_updated`. |
| `class0_rows` | First-stage rows with high precision geocoding class. |
| `class1_rows` | First-stage rows with CEP-level geocoding class. |
| `class2_rows` | First-stage rows with low or missing precision class. |
| `cep_rows` | Number of CEPs evaluated by the first-stage CEP consistency rule. |
| `cep_accepted` | Number of CEPs accepted by the first-stage CEP consistency rule. |
| `cep_rejected` | Number of CEPs rejected by the first-stage CEP consistency rule. |
| `cep_single_point_accepted` | Number of single-point CEPs accepted. |
| `cep_single_point_rejected` | Number of single-point CEPs rejected. |
| `imputed_rows` | Number of rows imputed in the second stage. |
| `geometry_nonmissing` | Number of rows with non-missing geometry or updated geometry. |
| `params` | Compact string with key parameter values used for the row. |
| `timestamp_written` | Time at which the log row was appended to disk. |
