args <- commandArgs(trailingOnly = TRUE)

usage <- paste(
  "Usage:",
  "Rscript scripts/run_analysis.R INPUT CONFIG_JSON OUTPUT",
  "[--export-predictions]"
)
if (length(args) < 3L) {
  stop(usage, call. = FALSE)
}
if (!requireNamespace("clinriskr", quietly = TRUE)) {
  stop(
    paste(
      "clinriskr is not installed.",
      "From the project root, run: R CMD INSTALL ."
    ),
    call. = FALSE
  )
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The jsonlite package is required.", call. = FALSE)
}

input_path <- args[[1L]]
config_path <- args[[2L]]
output_dir <- args[[3L]]
extra_args <- if (length(args) > 3L) args[-seq_len(3L)] else character()
export_predictions <- "--export-predictions" %in% extra_args

if (!file.exists(config_path)) {
  stop("Configuration file does not exist: ", config_path, call. = FALSE)
}
config <- jsonlite::read_json(
  config_path,
  simplifyVector = TRUE
)
cohort <- clinriskr::read_clinical_data(input_path)
result <- clinriskr::run_clinrisk_analysis(
  data = cohort,
  config = config,
  output_dir = output_dir,
  export_predictions = export_predictions
)

cat("Analysis completed.\n")
cat("Output: ", result$output_dir, "\n", sep = "")
print(result$performance)
