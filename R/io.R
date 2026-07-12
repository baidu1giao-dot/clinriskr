#' Read clinical data from a local file
#'
#' Supported inputs are CSV, TSV, XLS, and XLSX. Input data are read locally
#' and are never uploaded by this package.
#'
#' @param path Path to a local data file.
#' @param sheet Excel worksheet name or position.
#'
#' @return A data frame.
#' @export
read_clinical_data <- function(path, sheet = 1L) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    stop("path must identify an existing local file.", call. = FALSE)
  }
  extension <- tolower(tools::file_ext(path))
  result <- switch(
    extension,
    csv = utils::read.csv(
      path,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    tsv = utils::read.delim(
      path,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    xls = as.data.frame(
      readxl::read_excel(path, sheet = sheet, .name_repair = "minimal")
    ),
    xlsx = as.data.frame(
      readxl::read_excel(path, sheet = sheet, .name_repair = "minimal")
    ),
    stop("Unsupported file type. Use CSV, TSV, XLS, or XLSX.", call. = FALSE)
  )
  if (nrow(result) == 0L || ncol(result) == 0L) {
    stop("The input file contains no tabular data.", call. = FALSE)
  }
  result
}
