#' Validate a clinical cohort against an analysis configuration
#'
#' Stops on structural errors that would make an analysis invalid and returns
#' a compact summary plus non-fatal warnings.
#'
#' @param data A data frame.
#' @param config Analysis configuration, such as [default_hdp_config()].
#'
#' @return A list containing row counts, event counts, and warnings.
#' @export
validate_clinical_data <- function(data, config) {
  validate_config(config)
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("data must be a non-empty data frame.", call. = FALSE)
  }
  if (anyDuplicated(names(data))) {
    stop("Input column names must be unique.", call. = FALSE)
  }

  required <- unique(c(
    config$id,
    config$outcome,
    config$continuous,
    config$categorical,
    config$predictors
  ))
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyNA(data[[config$id]]) || anyDuplicated(data[[config$id]])) {
    stop("The ID column must be complete and unique.", call. = FALSE)
  }

  outcome <- as_binary_outcome(data[[config$outcome]])
  if (anyNA(outcome)) {
    stop("The outcome column cannot contain missing values.", call. = FALSE)
  }
  counts <- table(factor(outcome, levels = 0:1))
  if (any(counts == 0L)) {
    stop(
      "The outcome must contain at least one event and one non-event.",
      call. = FALSE
    )
  }

  non_numeric <- config$continuous[
    !vapply(data[config$continuous], is.numeric, logical(1))
  ]
  if (length(non_numeric) > 0L) {
    stop(
      "Continuous variables must be numeric: ",
      paste(non_numeric, collapse = ", "),
      call. = FALSE
    )
  }

  all_missing <- required[
    vapply(data[required], function(x) all(is.na(x)), logical(1))
  ]
  if (length(all_missing) > 0L) {
    stop(
      "Required columns cannot be entirely missing: ",
      paste(all_missing, collapse = ", "),
      call. = FALSE
    )
  }
  constant_predictors <- config$predictors[
    vapply(
      data[config$predictors],
      function(x) length(unique(stats::na.omit(x))) < 2L,
      logical(1)
    )
  ]
  if (length(constant_predictors) > 0L) {
    stop(
      "Predictors must have at least two observed values: ",
      paste(constant_predictors, collapse = ", "),
      call. = FALSE
    )
  }

  warnings <- character()
  missing_rate <- vapply(
    data[required],
    function(x) mean(is.na(x)),
    numeric(1)
  )
  high_missing <- names(missing_rate)[missing_rate > 0.20]
  if (length(high_missing) > 0L) {
    warnings <- c(
      warnings,
      paste0("More than 20% missing: ", paste(high_missing, collapse = ", "))
    )
  }
  if (min(counts) < 20L) {
    warnings <- c(
      warnings,
      "Fewer than 20 observations in one outcome group; estimates may be unstable."
    )
  }

  model_complete <- stats::complete.cases(
    data[c(config$outcome, config$predictors)]
  )
  if (sum(model_complete) < 30L) {
    warnings <- c(
      warnings,
      "Fewer than 30 complete rows are available for the configured model."
    )
  }

  list(
    rows = nrow(data),
    events = unname(counts[[2L]]),
    non_events = unname(counts[[1L]]),
    event_rate = mean(outcome),
    complete_model_rows = sum(model_complete),
    warnings = unique(warnings)
  )
}
