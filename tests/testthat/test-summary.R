test_that("baseline summary includes declared variables and missingness", {
  cohort <- simulate_hdp_data(n = 100, missing_rate = 0.05, seed = 31)
  result <- summarize_baseline(cohort, default_hdp_config())

  expect_s3_class(result, "data.frame")
  expect_true(all(default_hdp_config()$continuous %in% result$variable))
  expect_true(all(default_hdp_config()$categorical %in% result$variable))
  expect_true(all(c(
    "group_0",
    "group_1",
    "missing_n",
    "test",
    "p_value"
  ) %in% names(result)))
  expect_gt(result$missing_n[result$variable == "ua"][[1L]], 0)
})
