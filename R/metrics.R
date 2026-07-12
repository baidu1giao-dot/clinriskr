safe_calibration_stat <- function(truth, probability, type) {
  clipped_probability <- pmin(pmax(probability, 1e-6), 1 - 1e-6)
  linear_predictor <- stats::qlogis(clipped_probability)
  fit <- if (identical(type, "intercept")) {
    capture_fit(
      stats::glm(
        truth ~ 1,
        offset = linear_predictor,
        family = stats::binomial()
      )
    )
  } else {
    capture_fit(
      stats::glm(
        truth ~ linear_predictor,
        family = stats::binomial()
      )
    )
  }
  if (inherits(fit$value, "error")) {
    return(NA_real_)
  }
  if (identical(type, "intercept")) {
    return(unname(stats::coef(fit$value)[[1L]]))
  }
  coefficients <- stats::coef(fit$value)
  if (length(coefficients) < 2L) {
    return(NA_real_)
  }
  unname(coefficients[[2L]])
}

#' Evaluate binary outcome probabilities
#'
#' Computes apparent AUC with a 95 percent confidence interval, Brier score,
#' Youden threshold, sensitivity, specificity, and simple calibration
#' intercept and slope estimates.
#'
#' @param truth Binary outcome coded 0 and 1.
#' @param probability Predicted event probabilities.
#'
#' @return A list with one-row metrics and ROC curve points.
#' @export
evaluate_binary_predictions <- function(truth, probability) {
  truth <- as_binary_outcome(truth)
  if (!is.numeric(probability) || length(probability) != length(truth)) {
    stop(
      "probability must be numeric and have the same length as truth.",
      call. = FALSE
    )
  }
  keep <- stats::complete.cases(truth, probability)
  truth <- truth[keep]
  probability <- probability[keep]
  if (
    length(truth) < 10L ||
      length(unique(truth)) != 2L ||
      any(!is.finite(probability)) ||
      any(probability < 0 | probability > 1)
  ) {
    stop(
      "At least 10 complete binary outcomes and finite probabilities in [0, 1] are required.",
      call. = FALSE
    )
  }

  roc_object <- pROC::roc(
    response = truth,
    predictor = probability,
    levels = c(0, 1),
    direction = "<",
    quiet = TRUE
  )
  auc_value <- as.numeric(pROC::auc(roc_object))
  auc_ci <- tryCatch(
    as.numeric(pROC::ci.auc(roc_object)),
    error = function(e) rep(NA_real_, 3L)
  )
  best <- as.data.frame(
    pROC::coords(
      roc_object,
      x = "best",
      best.method = "youden",
      ret = c("threshold", "sensitivity", "specificity"),
      transpose = FALSE
    )
  )
  best <- best[1L, , drop = FALSE]

  metrics <- data.frame(
    n = length(truth),
    events = sum(truth == 1L),
    auc = auc_value,
    auc_ci_low = auc_ci[[1L]],
    auc_ci_high = auc_ci[[3L]],
    brier_score = mean((truth - probability)^2),
    youden_threshold = as.numeric(best$threshold),
    sensitivity = as.numeric(best$sensitivity),
    specificity = as.numeric(best$specificity),
    calibration_intercept = safe_calibration_stat(
      truth,
      probability,
      "intercept"
    ),
    calibration_slope = safe_calibration_stat(
      truth,
      probability,
      "slope"
    ),
    evaluation = "apparent_in_sample",
    stringsAsFactors = FALSE
  )
  curve <- data.frame(
    threshold = roc_object$thresholds,
    specificity = roc_object$specificities,
    sensitivity = roc_object$sensitivities,
    fpr = 1 - roc_object$specificities,
    tpr = roc_object$sensitivities,
    stringsAsFactors = FALSE
  )
  list(metrics = metrics, curve = curve)
}
