write_csv_table <- function(data, path) {
  utils::write.csv(
    data,
    file = path,
    row.names = FALSE,
    na = ""
  )
}

save_plot_pair <- function(plot, directory, stem, width, height) {
  ggplot2::ggsave(
    filename = file.path(directory, paste0(stem, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
  ggplot2::ggsave(
    filename = file.path(directory, paste0(stem, ".pdf")),
    plot = plot,
    width = width,
    height = height
  )
}

create_roc_plot <- function(curves) {
  fpr <- tpr <- model <- NULL
  ggplot2::ggplot(
    curves,
    ggplot2::aes(x = fpr, y = tpr, color = model)
  ) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "#8B8B82"
    ) +
    ggplot2::coord_equal() +
    ggplot2::scale_color_manual(values = c(glm = "#1F6F8B", firth = "#C4513B")) +
    ggplot2::labs(
      title = "Apparent model discrimination",
      subtitle = "Performance is evaluated on the model-fitting cohort",
      x = "1 - specificity",
      y = "Sensitivity",
      color = "Model"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

create_forest_plot <- function(coefficients) {
  term <- odds_ratio <- conf_low <- conf_high <- NULL
  plot_data <- coefficients[
    coefficients$model == "firth" &
      coefficients$term != "(Intercept)" &
      is.finite(coefficients$odds_ratio) &
      is.finite(coefficients$conf_low) &
      is.finite(coefficients$conf_high),
    ,
    drop = FALSE
  ]
  if (nrow(plot_data) == 0L) {
    return(NULL)
  }
  plot_data$term <- factor(
    plot_data$term,
    levels = rev(plot_data$term)
  )
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = odds_ratio, y = term)
  ) +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      color = "#8B8B82"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = conf_low, xmax = conf_high),
      width = 0.18,
      orientation = "y",
      color = "#1F6F8B"
    ) +
    ggplot2::geom_point(size = 2.7, color = "#C4513B") +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      title = "Firth logistic regression",
      subtitle = "Profile-likelihood 95% confidence intervals",
      x = "Odds ratio (log scale)",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

evaluate_model_set <- function(predictions) {
  specifications <- c(
    glm = "glm_probability",
    firth = "firth_probability"
  )
  metric_rows <- list()
  curve_rows <- list()
  for (model_name in names(specifications)) {
    probability <- predictions[[specifications[[model_name]]]]
    evaluation <- tryCatch(
      evaluate_binary_predictions(predictions$outcome, probability),
      error = function(e) e
    )
    if (inherits(evaluation, "error")) {
      metric_rows[[model_name]] <- data.frame(
        model = model_name,
        n = sum(stats::complete.cases(predictions$outcome, probability)),
        events = NA_real_,
        auc = NA_real_,
        auc_ci_low = NA_real_,
        auc_ci_high = NA_real_,
        brier_score = NA_real_,
        youden_threshold = NA_real_,
        sensitivity = NA_real_,
        specificity = NA_real_,
        calibration_intercept = NA_real_,
        calibration_slope = NA_real_,
        evaluation = "failed",
        note = conditionMessage(evaluation),
        stringsAsFactors = FALSE
      )
      next
    }
    metric_rows[[model_name]] <- cbind(
      model = model_name,
      evaluation$metrics,
      note = NA_character_,
      stringsAsFactors = FALSE
    )
    curve_rows[[model_name]] <- cbind(
      model = model_name,
      evaluation$curve,
      stringsAsFactors = FALSE
    )
  }
  curves <- if (length(curve_rows) > 0L) {
    do.call(rbind, curve_rows)
  } else {
    data.frame(
      model = character(),
      threshold = numeric(),
      specificity = numeric(),
      sensitivity = numeric(),
      fpr = numeric(),
      tpr = numeric(),
      stringsAsFactors = FALSE
    )
  }
  list(
    metrics = do.call(rbind, metric_rows),
    curves = curves
  )
}

create_manifest <- function(output_root) {
  files <- list.files(
    output_root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE
  )
  files <- files[file.info(files)$isdir %in% FALSE]
  relative_paths <- substring(files, nchar(output_root) + 2L)
  data.frame(
    path = relative_paths,
    bytes = unname(file.info(files)$size),
    md5 = unname(tools::md5sum(files)),
    contains_row_level_data = grepl(
      "predictions\\.csv$",
      relative_paths
    ),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Run a privacy-conscious clinical risk analysis
#'
#' Writes aggregate tables, model coefficients, figures, configuration, and
#' session metadata. The input data are not copied. Row-level predictions are
#' excluded by default and require explicit opt-in.
#'
#' @param data A data frame.
#' @param config Analysis configuration.
#' @param output_dir Directory for generated outputs.
#' @param export_predictions Whether to write row-level IDs, outcomes, and
#'   model predictions.
#' @param overwrite Whether an existing non-empty output directory may be used.
#'
#' @return A list containing key in-memory results and the output path.
#' @export
run_clinrisk_analysis <- function(
  data,
  config,
  output_dir,
  export_predictions = FALSE,
  overwrite = FALSE
) {
  if (
    !is.logical(export_predictions) ||
      length(export_predictions) != 1L ||
      is.na(export_predictions)
  ) {
    stop("export_predictions must be TRUE or FALSE.", call. = FALSE)
  }
  output_root <- ensure_output_directory(output_dir, overwrite = overwrite)
  table_dir <- file.path(output_root, "tables")
  figure_dir <- file.path(output_root, "figures")
  metadata_dir <- file.path(output_root, "metadata")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)

  validation <- validate_clinical_data(data, config)
  baseline <- summarize_baseline(data, config)
  missingness <- summarize_missingness(data)
  models <- fit_clinical_models(data, config)
  performance <- evaluate_model_set(models$predictions)

  validation_table <- data.frame(
    metric = c(
      "rows",
      "events",
      "non_events",
      "event_rate",
      "complete_model_rows"
    ),
    value = c(
      validation$rows,
      validation$events,
      validation$non_events,
      validation$event_rate,
      validation$complete_model_rows
    ),
    stringsAsFactors = FALSE
  )
  validation_warnings <- data.frame(
    warning = validation$warnings,
    stringsAsFactors = FALSE
  )

  write_csv_table(validation_table, file.path(table_dir, "validation_summary.csv"))
  write_csv_table(validation_warnings, file.path(table_dir, "validation_warnings.csv"))
  write_csv_table(missingness, file.path(table_dir, "missingness.csv"))
  write_csv_table(baseline, file.path(table_dir, "baseline_by_outcome.csv"))
  write_csv_table(models$model_info, file.path(table_dir, "model_info.csv"))
  write_csv_table(models$warnings, file.path(table_dir, "model_warnings.csv"))
  write_csv_table(models$coefficients, file.path(table_dir, "model_coefficients.csv"))
  write_csv_table(performance$metrics, file.path(table_dir, "apparent_performance.csv"))
  write_csv_table(performance$curves, file.path(table_dir, "roc_curve_points.csv"))

  if (isTRUE(export_predictions)) {
    write_csv_table(
      models$predictions,
      file.path(table_dir, "predictions.csv")
    )
  }

  if (nrow(performance$curves) > 0L) {
    roc_plot <- create_roc_plot(performance$curves)
    save_plot_pair(
      roc_plot,
      figure_dir,
      "roc_curves",
      width = 7.2,
      height = 6
    )
  }
  forest_plot <- create_forest_plot(models$coefficients)
  if (!is.null(forest_plot)) {
    save_plot_pair(
      forest_plot,
      figure_dir,
      "firth_forest",
      width = 8,
      height = 6
    )
  }

  writexl::write_xlsx(
    list(
      validation = validation_table,
      missingness = missingness,
      baseline = baseline,
      model_info = models$model_info,
      coefficients = models$coefficients,
      performance = performance$metrics
    ),
    path = file.path(table_dir, "analysis_summary.xlsx")
  )

  jsonlite::write_json(
    config,
    path = file.path(metadata_dir, "analysis_config.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  metadata <- list(
    generated_at_utc = format(
      Sys.time(),
      tz = "UTC",
      format = "%Y-%m-%dT%H:%M:%SZ"
    ),
    clinriskr_version = safe_package_version(),
    dataset_name = config$dataset_name,
    model_formula = paste(deparse(models$formula), collapse = " "),
    input_data_copied = FALSE,
    row_level_predictions_exported = isTRUE(export_predictions),
    performance_scope = "Apparent in-sample performance; external validation is required.",
    intended_use = paste(
      "Research workflow support only.",
      "Not for diagnosis, treatment, or individual clinical decisions."
    )
  )
  jsonlite::write_json(
    metadata,
    path = file.path(metadata_dir, "run_metadata.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  writeLines(
    utils::capture.output(utils::sessionInfo()),
    con = file.path(metadata_dir, "session_info.txt")
  )

  manifest <- create_manifest(output_root)
  write_csv_table(manifest, file.path(output_root, "manifest.csv"))

  list(
    output_dir = output_root,
    validation = validation,
    baseline = baseline,
    coefficients = models$coefficients,
    performance = performance$metrics,
    manifest = manifest
  )
}
