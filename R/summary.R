continuous_group_stat <- function(values) {
  values <- stats::na.omit(values)
  if (length(values) == 0L) {
    return(NA_character_)
  }
  normal <- FALSE
  if (
    length(values) >= 3L &&
      length(values) <= 5000L &&
      length(unique(values)) >= 3L
  ) {
    normal <- tryCatch(
      stats::shapiro.test(values)$p.value > 0.05,
      error = function(e) FALSE
    )
  }
  if (normal) {
    paste0(
      format_number(mean(values)),
      " (SD ",
      format_number(stats::sd(values)),
      ")"
    )
  } else {
    quantiles <- stats::quantile(
      values,
      probs = c(0.25, 0.5, 0.75),
      na.rm = TRUE,
      names = FALSE
    )
    paste0(
      format_number(quantiles[[2L]]),
      " (IQR ",
      format_number(quantiles[[1L]]),
      "-",
      format_number(quantiles[[3L]]),
      ")"
    )
  }
}

continuous_test <- function(values, outcome) {
  group_0 <- stats::na.omit(values[outcome == 0L])
  group_1 <- stats::na.omit(values[outcome == 1L])
  if (length(group_0) < 2L || length(group_1) < 2L) {
    return(list(test = "Not estimable", p_value = NA_real_))
  }

  is_normal <- function(x) {
    if (length(x) < 3L || length(x) > 5000L || length(unique(x)) < 3L) {
      return(FALSE)
    }
    tryCatch(
      stats::shapiro.test(x)$p.value > 0.05,
      error = function(e) FALSE
    )
  }
  if (is_normal(group_0) && is_normal(group_1)) {
    p_value <- tryCatch(
      stats::t.test(group_0, group_1)$p.value,
      error = function(e) NA_real_
    )
    return(list(test = "Welch t-test", p_value = p_value))
  }

  p_value <- tryCatch(
    suppressWarnings(
      stats::wilcox.test(group_0, group_1, exact = FALSE)$p.value
    ),
    error = function(e) NA_real_
  )
  list(test = "Mann-Whitney U", p_value = p_value)
}

summarize_continuous_variable <- function(data, variable, outcome_name) {
  values <- data[[variable]]
  outcome <- data[[outcome_name]]
  test_result <- continuous_test(values, outcome)
  data.frame(
    variable = variable,
    variable_type = "continuous",
    level = NA_character_,
    group_0 = continuous_group_stat(values[outcome == 0L]),
    group_1 = continuous_group_stat(values[outcome == 1L]),
    missing_n = sum(is.na(values)),
    missing_pct = mean(is.na(values)) * 100,
    test = test_result$test,
    p_value = test_result$p_value,
    p_value_formatted = format_p_value(test_result$p_value),
    stringsAsFactors = FALSE
  )
}

categorical_test <- function(values, outcome) {
  keep <- !is.na(values)
  test_table <- table(
    factor(values[keep]),
    factor(outcome[keep], levels = 0:1)
  )
  if (nrow(test_table) < 2L || ncol(test_table) < 2L) {
    return(list(test = "Not estimable", p_value = NA_real_))
  }

  expected <- suppressWarnings(stats::chisq.test(test_table)$expected)
  if (identical(dim(test_table), c(2L, 2L)) && any(expected < 5)) {
    p_value <- tryCatch(
      stats::fisher.test(test_table)$p.value,
      error = function(e) NA_real_
    )
    return(list(test = "Fisher exact", p_value = p_value))
  }

  label <- if (any(expected < 5)) {
    "Chi-square (small expected counts)"
  } else {
    "Chi-square"
  }
  p_value <- tryCatch(
    suppressWarnings(stats::chisq.test(test_table, correct = FALSE)$p.value),
    error = function(e) NA_real_
  )
  list(test = label, p_value = p_value)
}

summarize_categorical_variable <- function(data, variable, outcome_name) {
  raw_values <- data[[variable]]
  outcome <- data[[outcome_name]]
  test_result <- categorical_test(raw_values, outcome)

  if (is.factor(raw_values)) {
    observed_levels <- levels(raw_values)
  } else {
    observed_levels <- sort(unique(as.character(stats::na.omit(raw_values))))
  }
  if (anyNA(raw_values)) {
    observed_levels <- c(observed_levels, "(Missing)")
  }

  display_values <- as.character(raw_values)
  display_values[is.na(display_values)] <- "(Missing)"
  denominators <- table(factor(outcome, levels = 0:1))

  rows <- lapply(observed_levels, function(level_name) {
    counts <- table(
      factor(outcome[display_values == level_name], levels = 0:1)
    )
    data.frame(
      variable = variable,
      variable_type = "categorical",
      level = level_name,
      group_0 = paste0(
        counts[[1L]],
        " (",
        format_number(100 * counts[[1L]] / denominators[[1L]], 1L),
        "%)"
      ),
      group_1 = paste0(
        counts[[2L]],
        " (",
        format_number(100 * counts[[2L]] / denominators[[2L]], 1L),
        "%)"
      ),
      missing_n = sum(is.na(raw_values)),
      missing_pct = mean(is.na(raw_values)) * 100,
      test = test_result$test,
      p_value = test_result$p_value,
      p_value_formatted = format_p_value(test_result$p_value),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Create a baseline summary by binary outcome
#'
#' Continuous variables are summarized as mean (SD) when both outcome groups
#' pass a Shapiro-Wilk check and otherwise as median (IQR). Categorical
#' percentages use all rows in each outcome group as the denominator, including
#' an explicit missing category when needed.
#'
#' @param data A data frame.
#' @param config Analysis configuration.
#'
#' @return A data frame with descriptive statistics and group-comparison tests.
#' @export
summarize_baseline <- function(data, config) {
  validate_clinical_data(data, config)
  working <- data
  working[[config$outcome]] <- as_binary_outcome(
    working[[config$outcome]]
  )

  continuous_rows <- lapply(
    config$continuous,
    function(variable) {
      summarize_continuous_variable(working, variable, config$outcome)
    }
  )
  categorical_rows <- lapply(
    config$categorical,
    function(variable) {
      summarize_categorical_variable(working, variable, config$outcome)
    }
  )
  do.call(rbind, c(continuous_rows, categorical_rows))
}

summarize_missingness <- function(data) {
  data.frame(
    variable = names(data),
    missing_n = vapply(data, function(x) sum(is.na(x)), numeric(1)),
    missing_pct = vapply(data, function(x) mean(is.na(x)) * 100, numeric(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
