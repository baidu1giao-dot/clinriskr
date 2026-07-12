args <- commandArgs(trailingOnly = TRUE)

if (!requireNamespace("clinriskr", quietly = TRUE)) {
  stop(
    paste(
      "clinriskr is not installed.",
      "From the project root, run: R CMD INSTALL ."
    ),
    call. = FALSE
  )
}

timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
output_dir <- if (length(args) >= 1L) {
  args[[1L]]
} else {
  file.path("examples", "results", paste0("synthetic-hdp-", timestamp))
}

cohort <- clinriskr::simulate_hdp_data(
  n = 420,
  outcome_rate = 0.38,
  missing_rate = 0.02,
  seed = 20260502
)
result <- clinriskr::run_clinrisk_analysis(
  data = cohort,
  config = clinriskr::default_hdp_config(),
  output_dir = output_dir
)

cat("Synthetic example completed.\n")
cat("Output: ", result$output_dir, "\n", sep = "")
print(result$performance)
