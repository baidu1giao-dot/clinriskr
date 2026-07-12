test_that("valid cohorts return useful counts", {
  cohort <- simulate_hdp_data(n = 100, missing_rate = 0, seed = 22)
  result <- validate_clinical_data(cohort, default_hdp_config())

  expect_equal(result$rows, 100)
  expect_equal(result$events + result$non_events, 100)
  expect_equal(result$complete_model_rows, 100)
})

test_that("validation catches structural errors", {
  cohort <- simulate_hdp_data(n = 80, missing_rate = 0, seed = 23)
  config <- default_hdp_config()

  missing_column <- cohort
  missing_column$ua <- NULL
  expect_error(
    validate_clinical_data(missing_column, config),
    "Missing required columns"
  )

  duplicate_id <- cohort
  duplicate_id$id[[2L]] <- duplicate_id$id[[1L]]
  expect_error(
    validate_clinical_data(duplicate_id, config),
    "complete and unique"
  )

  invalid_outcome <- cohort
  invalid_outcome$outcome[[1L]] <- 2
  expect_error(
    validate_clinical_data(invalid_outcome, config),
    "only 0 and 1"
  )

  invalid_type <- cohort
  invalid_type$age <- as.character(invalid_type$age)
  expect_error(
    validate_clinical_data(invalid_type, config),
    "must be numeric"
  )
})
