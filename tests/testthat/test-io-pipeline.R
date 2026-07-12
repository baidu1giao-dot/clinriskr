test_that("CSV input round-trips through the local reader", {
  cohort <- simulate_hdp_data(n = 50, missing_rate = 0, seed = 51)
  path <- tempfile(fileext = ".csv")
  utils::write.csv(cohort, path, row.names = FALSE)

  restored <- read_clinical_data(path)
  expect_equal(nrow(restored), nrow(cohort))
  expect_equal(names(restored), names(cohort))
})

test_that("pipeline is aggregate-first and predictions require opt-in", {
  cohort <- simulate_hdp_data(n = 90, missing_rate = 0.01, seed = 52)
  aggregate_dir <- tempfile("clinriskr-aggregate-")
  aggregate_result <- run_clinrisk_analysis(
    cohort,
    default_hdp_config(),
    aggregate_dir
  )

  expect_true(file.exists(file.path(aggregate_dir, "manifest.csv")))
  expect_true(file.exists(file.path(
    aggregate_dir,
    "tables",
    "model_coefficients.csv"
  )))
  expect_false(file.exists(file.path(
    aggregate_dir,
    "tables",
    "predictions.csv"
  )))
  expect_false(any(aggregate_result$manifest$contains_row_level_data))

  row_level_dir <- tempfile("clinriskr-row-level-")
  row_level_result <- run_clinrisk_analysis(
    cohort,
    default_hdp_config(),
    row_level_dir,
    export_predictions = TRUE
  )
  expect_true(file.exists(file.path(
    row_level_dir,
    "tables",
    "predictions.csv"
  )))
  expect_true(any(row_level_result$manifest$contains_row_level_data))
})
