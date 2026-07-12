format_p_value <- function(x) {
  ifelse(is.na(x), NA_character_, ifelse(x < 0.001, "<0.001", sprintf("%.3f", x)))
}

format_number <- function(x, digits = 2L) {
  ifelse(is.na(x), NA_character_, sprintf(paste0("%.", digits, "f"), x))
}

clip_number <- function(x, lower, upper) {
  pmin(pmax(x, lower), upper)
}

as_binary_outcome <- function(x) {
  if (is.logical(x)) {
    return(as.integer(x))
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.character(x)) {
    if (!all(stats::na.omit(x) %in% c("0", "1"))) {
      stop("The outcome must contain only 0 and 1.", call. = FALSE)
    }
    return(as.integer(x))
  }
  if (!is.numeric(x) || !all(stats::na.omit(x) %in% c(0, 1))) {
    stop("The outcome must contain only 0 and 1.", call. = FALSE)
  }
  as.integer(x)
}

capture_fit <- function(expr) {
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  list(value = value, warnings = unique(warnings))
}

ensure_output_directory <- function(path, overwrite = FALSE) {
  if (dir.exists(path)) {
    existing <- list.files(path, all.files = TRUE, no.. = TRUE)
    if (length(existing) > 0L && !isTRUE(overwrite)) {
      stop("Output directory is not empty. Set overwrite = TRUE to reuse it.", call. = FALSE)
    }
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, mustWork = TRUE)
}

safe_package_version <- function() {
  tryCatch(as.character(utils::packageVersion("clinriskr")), error = function(e) "development")
}
