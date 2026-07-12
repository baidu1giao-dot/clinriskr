test_that("synthetic cohort is reproducible and structurally valid", {
  first <- simulate_hdp_data(n = 80, missing_rate = 0, seed = 12)
  second <- simulate_hdp_data(n = 80, missing_rate = 0, seed = 12)

  expect_identical(first, second)
  expect_equal(nrow(first), 80)
  expect_true(all(first$outcome %in% c(0, 1)))
  expect_true(is.factor(first$hdp_type))
  expect_true(all(c("ua", "plt", "cr") %in% names(first)))
  expect_gt(length(unique(first$outcome)), 1)
})

test_that("synthetic cohort arguments are constrained", {
  expect_error(simulate_hdp_data(n = 39), "at least 40")
  expect_error(simulate_hdp_data(outcome_rate = 1), "between")
  expect_error(simulate_hdp_data(missing_rate = 0.25), "between")
})
