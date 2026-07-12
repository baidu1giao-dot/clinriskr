test_that("both configured models return probabilities and coefficients", {
  cohort <- simulate_hdp_data(n = 120, missing_rate = 0, seed = 44)
  result <- fit_clinical_models(cohort, default_hdp_config())

  expect_true(all(c("glm", "firth") %in% result$coefficients$model))
  expect_equal(nrow(result$predictions), 120)
  expect_true(all(result$predictions$glm_probability >= 0))
  expect_true(all(result$predictions$glm_probability <= 1))
  expect_true(all(result$predictions$firth_probability >= 0))
  expect_true(all(result$predictions$firth_probability <= 1))
})

test_that("binary performance metrics detect ordered predictions", {
  truth <- rep(c(0, 1), each = 20)
  probability <- c(
    seq(0.05, 0.60, length.out = 20),
    seq(0.40, 0.95, length.out = 20)
  )
  result <- evaluate_binary_predictions(truth, probability)

  expect_gt(result$metrics$auc, 0.8)
  expect_lt(result$metrics$brier_score, 0.2)
  expect_true(all(c("fpr", "tpr") %in% names(result$curve)))
})
