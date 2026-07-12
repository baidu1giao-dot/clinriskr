#' Default configuration for the synthetic HDP example
#'
#' @return A named list accepted by [run_clinrisk_analysis()].
#' @export
#' @examples
#' config <- default_hdp_config()
#' config$predictors
default_hdp_config <- function() {
  list(
    dataset_name = "Synthetic hypertensive disorders of pregnancy cohort",
    id = "id",
    outcome = "outcome",
    continuous = c(
      "age", "bmi", "gravida", "para", "sbp", "dbp", "ua", "plt",
      "alt", "ast", "cr"
    ),
    categorical = c("hdp_type", "proteinuria", "antihyp", "mgso4"),
    predictors = c("ua", "sbp", "hdp_type", "plt", "age", "cr")
  )
}

required_config_fields <- function() {
  c("dataset_name", "id", "outcome", "continuous", "categorical", "predictors")
}

validate_config <- function(config) {
  if (!is.list(config)) {
    stop("config must be a named list.", call. = FALSE)
  }
  missing_fields <- setdiff(required_config_fields(), names(config))
  if (length(missing_fields) > 0L) {
    stop("Missing config fields: ", paste(missing_fields, collapse = ", "), call. = FALSE)
  }
  scalar_fields <- c("dataset_name", "id", "outcome")
  scalar_ok <- vapply(
    config[scalar_fields],
    function(x) is.character(x) && length(x) == 1L && nzchar(x),
    logical(1)
  )
  if (!all(scalar_ok)) {
    stop("dataset_name, id, and outcome must be non-empty strings.", call. = FALSE)
  }
  vector_fields <- c("continuous", "categorical", "predictors")
  vector_ok <- vapply(config[vector_fields], is.character, logical(1))
  if (!all(vector_ok) || length(config$predictors) == 0L) {
    stop(
      "continuous, categorical, and predictors must be character vectors; predictors cannot be empty.",
      call. = FALSE
    )
  }
  duplicate_roles <- intersect(config$continuous, config$categorical)
  if (length(duplicate_roles) > 0L) {
    stop(
      "Variables cannot be both continuous and categorical: ",
      paste(duplicate_roles, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
