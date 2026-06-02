#' Import and standardize Alzheimer's study data
#'
#' Reads data from common Alzheimer's study formats (ADNI, NACC, or custom CSV)
#' and maps variable names to a consistent internal format.
#'
#' @param path Path to the data file.
#' @param source Source format. One of \code{"adni"}, \code{"nacc"}, or \code{"custom"}.
#' @param ... Additional arguments passed to the underlying read function.
#'
#' @return A \link[data.frame]{data.frame} with standardized column names.
#'
#' @export
read_ad_data <- function(path, source = c("adni", "nacc", "custom"), ...) {
  stop("Not implemented yet.")
}

#' List available built-in reference datasets
#'
#' Returns a data frame describing the reference datasets bundled with the package.
#'
#' @return A data frame with columns \code{dataset}, \code{description},
#'   \code{n_subjects}, and \code{variables}.
#'
#' @export
list_reference_data <- function() {
  stop("Not implemented yet.")
}
