#' Import and standardize Alzheimer's study data
#'
#' Reads data from common Alzheimer's study formats and maps variable names
#' to a consistent internal format. Currently supports ADNI and NACC CSV
#' exports, as well as generic custom CSV files.
#'
#' Standardised column names (where mapped):
#' \code{id}, \code{visit}, \code{date}, \code{dx}, \code{age}, \code{sex},
#' \code{education}, \code{apoe}, \code{mmse}, \code{moca}, \code{cdr_sb},
#' \code{cdr_global}, \code{adas_cog13}, and common biomarker names.
#'
#' @param path Path to the data file (CSV).
#' @param source Source format. One of \code{"adni"}, \code{"nacc"}, or
#'   \code{"custom"}. \code{"custom"} reads the file as-is.
#' @param ... Additional arguments passed to \code{\link[utils]{read.csv}}.
#'
#' @return A data frame with (where possible) standardised column names.
#'
#' @examples
#' \dontrun{
#' # ADNI CSV with default mapping
#' dat <- read_ad_data("ADNI_data.csv", source = "adni")
#'
#' # Custom CSV, pass-through
#' dat <- read_ad_data("my_study.csv", source = "custom")
#' }
#'
#' @export
read_ad_data <- function(path, source = c("adni", "nacc", "custom"), ...) {
  source <- match.arg(source)
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, ...)

  if (source == "custom") {
    return(raw)
  }

  # Column name mapping dictionaries
  adni_map <- c(
    PTID       = "id",
    RID        = "id",
    VISCODE    = "visit",
    EXAMDATE   = "date",
    DX         = "dx",
    DXCHANGE   = "dx_change",
    AGE        = "age",
    PTGENDER   = "sex",
    PTEDUCAT   = "education",
    APOE4      = "apoe",
    MMSE       = "mmse",
    CDRSB      = "cdr_sb",
    CDRGLOBAL  = "cdr_global",
    ADAS13     = "adas_cog13",
    MOCA       = "moca",
    ABETA42    = "abeta42",
    TAU        = "ttau",
    PTAU       = "ptau",
    PTAU181    = "ptau",
    PTAU217    = "ptau217",
    NFL        = "nfl",
    GFAP       = "gfap"
  )

  nacc_map <- c(
    ID         = "id",
    VISIT      = "visit",
    VISITDATE  = "date",
    NACCDX     = "dx",
    AGE        = "age",
    SEX        = "sex",
    EDUC       = "education",
    APOE       = "apoe",
    NACCMMSE   = "mmse",
    CDRSUM     = "cdr_sb",
    CDRGLOB    = "cdr_global"
  )

  map <- if (source == "adni") adni_map else nacc_map
  cols <- names(raw)
  mapped <- cols %in% names(map)

  if (!any(mapped)) {
    message("No recognised ", toupper(source), " column names found. ",
            "Returning data as-is.")
    return(raw)
  }

  names(raw)[mapped] <- unname(map[cols[mapped]])
  raw
}

#' List available built-in reference datasets
#'
#' Returns a data frame describing the reference datasets bundled with the
#' package. Currently no datasets are bundled; this function serves as a
#' placeholder for future releases.
#'
#' @return A data frame with columns \code{dataset}, \code{description},
#'   \code{n_subjects}, and \code{variables}.
#'
#' @export
list_reference_data <- function() {
  data.frame(
    dataset = character(),
    description = character(),
    n_subjects = integer(),
    variables = character(),
    stringsAsFactors = FALSE
  )
}
