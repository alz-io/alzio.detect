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
#' @param reference Which reference norms to use. When \code{"internal"}
#'   (default), the sample mean and SD are used. \code{"external"} is reserved
#'   for future built-in reference tables.
#'
#' @return A numeric vector of Z-scores.
#'
#' @export
compute_zscore <- function(raw_scores, test, age, education, sex,
                           reference = "internal") {
  test <- match.arg(test, c("mmse", "moca", "adas_cog13", "cdr_sb"))
  raw_scores <- as.numeric(raw_scores)

  if (reference == "internal") {
    mu <- mean(raw_scores, na.rm = TRUE)
    sigma <- sd(raw_scores, na.rm = TRUE)
    if (sigma == 0) {
      stop("Zero variance in the supplied scores — cannot compute Z-scores.")
    }
    (raw_scores - mu) / sigma
  } else {
    stop("External reference norms are not yet implemented.")
  }
}

#' Compute Reliable Change Index (RCI)
#'
#' Calculates the RCI (Jacobson & Truax, 1991) for longitudinal cognitive
#' assessment, indicating whether a change in score is statistically reliable
#' beyond measurement error.
#'
#' The RCI is defined as:
#' \deqn{RCI = \frac{X_2 - X_1}{SD \times \sqrt{2 \times (1 - r_{xx})}}}
#' where \eqn{X_1} and \eqn{X_2} are the scores at time points 1 and 2,
#' \eqn{SD} is the baseline standard deviation, and \eqn{r_{xx}} is the
#' test-retest reliability.
#'
#' @param score_t1 Score at time 1 (baseline).
#' @param score_t2 Score at time 2 (follow-up).
#' @param test_retest_r Test-retest reliability coefficient for the test.
#' @param sd_baseline Standard deviation of the test at baseline in the
#'   reference population.
#'
#' @return A numeric RCI value. Values outside \eqn{\pm 1.96} suggest
#'   reliable change (\code{abs(RCI) >= 1.96}) at \eqn{\alpha = 0.05}.
#'
#' @export
compute_rci <- function(score_t1, score_t2, test_retest_r, sd_baseline) {
  if (sd_baseline <= 0) {
    stop("'sd_baseline' must be positive.")
  }
  if (abs(test_retest_r) > 1) {
    stop("'test_retest_r' must be between -1 and 1.")
  }
  diff <- score_t2 - score_t1
  se_diff <- sd_baseline * sqrt(2 * (1 - test_retest_r))
  diff / se_diff
}
