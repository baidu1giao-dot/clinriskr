empty_coefficient_table <- function(model_name, note) {
  data.frame(
    model = model_name,
    term = NA_character_,
    estimate = NA_real_,
    odds_ratio = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    p_value = NA_real_,
    p_value_formatted = NA_character_,
    note = note,
    stringsAsFactors = FALSE
  )
}

tidy_glm_fit <- function(fit_result) {
  fit <- fit_result$value
  if (inherits(fit, "error")) {
    return(empty_coefficient_table("glm", conditionMessage(fit)))
  }
  coefficient_matrix <- summary(fit)$coefficients
  estimates <- coefficient_matrix[, "Estimate"]
  standard_errors <- coefficient_matrix[, "Std. Error"]
  p_values <- coefficient_matrix[, ncol(coefficient_matrix)]
  conf_low <- estimates - stats::qnorm(0.975) * standard_errors
  conf_high <- estimates + stats::qnorm(0.975) * standard_errors
  warning_note <- if (length(fit_result$warnings) > 0L) {
    paste(fit_result$warnings, collapse = " | ")
  } else {
    NA_character_
  }

  data.frame(
    model = "glm",
    term = names(estimates),
    estimate = unname(estimates),
    odds_ratio = exp(unname(estimates)),
    conf_low = exp(unname(conf_low)),
    conf_high = exp(unname(conf_high)),
    p_value = unname(p_values),
    p_value_formatted = format_p_value(unname(p_values)),
    note = warning_note,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

tidy_firth_fit <- function(fit_result) {
  fit <- fit_result$value
  if (inherits(fit, "error")) {
    return(empty_coefficient_table("firth", conditionMessage(fit)))
  }
  if (
    !inherits(fit, "logistf") ||
      is.null(fit$coefficients) ||
      is.null(fit$ci.lower) ||
      is.null(fit$ci.upper)
  ) {
    return(empty_coefficient_table("firth", "Unexpected logistf result."))
  }
  warning_note <- if (length(fit_result$warnings) > 0L) {
    paste(fit_result$warnings, collapse = " | ")
  } else {
    NA_character_
  }

  data.frame(
    model = "firth",
    term = names(fit$coefficients),
    estimate = unname(fit$coefficients),
    odds_ratio = exp(unname(fit$coefficients)),
    conf_low = exp(unname(fit$ci.lower)),
    conf_high = exp(unname(fit$ci.upper)),
    p_value = unname(fit$prob),
    p_value_formatted = format_p_value(unname(fit$prob)),
    note = warning_note,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

predict_model <- function(fit_result, model_data) {
  if (inherits(fit_result$value, "error")) {
    return(rep(NA_real_, nrow(model_data)))
  }
  tryCatch(
    as.numeric(stats::predict(fit_result$value, newdata = model_data, type = "response")),
    error = function(e) rep(NA_real_, nrow(model_data))
  )
}

#' Fit conventional and Firth logistic regression models
#'
#' The configured model is fit on complete cases for the outcome and selected
#' predictors. The returned performance data are in-sample and should not be
#' interpreted as external validation.
#'
#' @param data A data frame.
#' @param config Analysis configuration.
#'
#' @return A list containing model objects, coefficient tables, predictions,
#'   model diagnostics, and captured warnings.
#' @export
fit_clinical_models <- function(data, config) {
  validation <- validate_clinical_data(data, config)
  working <- data
  working[[config$outcome]] <- as_binary_outcome(
    working[[config$outcome]]
  )

  categorical_predictors <- intersect(
    config$categorical,
    config$predictors
  )
  for (variable in categorical_predictors) {
    working[[variable]] <- base::droplevels(factor(working[[variable]]))
  }

  model_columns <- unique(c(
    config$id,
    config$outcome,
    config$predictors
  ))
  complete_rows <- stats::complete.cases(working[model_columns])
  model_data <- working[complete_rows, model_columns, drop = FALSE]
  row_indices <- which(complete_rows)

  if (nrow(model_data) < 10L) {
    stop("At least 10 complete rows are required for model fitting.", call. = FALSE)
  }
  model_outcome <- model_data[[config$outcome]]
  if (length(unique(model_outcome)) != 2L) {
    stop(
      "Complete-case model data must contain both outcome classes.",
      call. = FALSE
    )
  }

  model_formula <- stats::reformulate(
    termlabels = config$predictors,
    response = config$outcome
  )
  glm_result <- capture_fit(
    stats::glm(
      model_formula,
      data = model_data,
      family = stats::binomial()
    )
  )
  firth_result <- capture_fit(
    logistf::logistf(
      model_formula,
      data = model_data,
      pl = TRUE
    )
  )

  coefficient_table <- rbind(
    tidy_glm_fit(glm_result),
    tidy_firth_fit(firth_result)
  )
  model_matrix <- stats::model.matrix(model_formula, data = model_data)
  parameter_count <- max(1L, ncol(model_matrix) - 1L)
  event_count <- sum(model_outcome == 1L)
  non_event_count <- sum(model_outcome == 0L)

  model_info <- data.frame(
    metric = c(
      "input_rows",
      "complete_case_rows",
      "excluded_incomplete_rows",
      "events",
      "non_events",
      "estimated_parameters",
      "minority_outcomes_per_parameter"
    ),
    value = c(
      validation$rows,
      nrow(model_data),
      validation$rows - nrow(model_data),
      event_count,
      non_event_count,
      parameter_count,
      min(event_count, non_event_count) / parameter_count
    ),
    stringsAsFactors = FALSE
  )

  predictions <- data.frame(
    row_index = row_indices,
    id = model_data[[config$id]],
    outcome = model_outcome,
    glm_probability = predict_model(glm_result, model_data),
    firth_probability = predict_model(firth_result, model_data),
    stringsAsFactors = FALSE
  )
  warning_rows <- function(model_name, messages) {
    if (length(messages) == 0L) {
      return(data.frame(
        model = character(),
        message = character(),
        stringsAsFactors = FALSE
      ))
    }
    data.frame(
      model = rep(model_name, length(messages)),
      message = messages,
      stringsAsFactors = FALSE
    )
  }
  warning_table <- rbind(
    warning_rows("glm", glm_result$warnings),
    warning_rows("firth", firth_result$warnings)
  )

  list(
    formula = model_formula,
    glm = glm_result$value,
    firth = firth_result$value,
    coefficients = coefficient_table,
    predictions = predictions,
    model_info = model_info,
    warnings = warning_table
  )
}
