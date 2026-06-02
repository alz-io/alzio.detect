#' Compute demographically-adjusted Z-scores for cognitive tests
#'
#' Converts raw cognitive test scores to Z-scores adjusted for age, education,
#' and sex using regression-based normative methods.
#'
#' @param raw_scores A numeric vector of raw test scores.
#' @param test Name of the cognitive test. One of \code{"mmse"}, \code{"moca"},
#'   \code{"adas_cog13"}, or \code{"cdr_sb"}.
#' @param age Numeric vector of ages in years.
#' @param education Numeric vector of education in years.
#' @param sex Factor or character vector with levels \code{"male"} and
#'   \code{"female"} (or \code{"M"} / \code{"F"}).
#' @param reference Which reference norms to use. Currently only
#'   \code{"internal"} is supported.
#'
#' @return A numeric vector of Z-scores.
#'
#' @export
compute_zscore <- function(raw_scores, test, age, education, sex,
                           reference = "internal") {
  stop("Not implemented yet.")
}

#' Compute Reliable Change Index (RCI)
#'
#' Calculates the RCI for longitudinal cognitive assessment, indicating
#' whether a change in score is statistically reliable beyond measurement error.
#'
#' @param score_t1 Score at time 1 (baseline).
#' @param score_t2 Score at time 2 (follow-up).
#' @param test_retest_r Test-retest reliability coefficient for the test.
#' @param sd_baseline Standard deviation of the test at baseline in the
#'   reference population.
#'
#' @return A numeric RCI value. Values outside \eqn{\pm 1.96} suggest
#'   reliable change (\code{abs(RCI) >= 1.96}).
#'
#' @export
compute_rci <- function(score_t1, score_t2, test_retest_r, sd_baseline) {
  stop("Not implemented yet.")
}
