#' Plot a cognitive profile (radar/spider plot)
#'
#' Creates a radar plot of Z-scores across cognitive domains for a single
#' subject or group.
#'
#' @param z_scores A named numeric vector of Z-scores. Names should be
#'   cognitive domain labels (e.g., "Memory", "Executive", "Language").
#' @param title Optional plot title.
#'
#' @return A \link[ggplot2]{ggplot} object. Requires \pkg{ggplot2} and
#'   \pkg{ggiraph} or equivalent.
#'
#' @export
plot_cognitive_profile <- function(z_scores, title = NULL) {
  stop("Not implemented yet.")
}

#' Plot longitudinal trajectories
#'
#' Plots individual and/or group-level trajectories for a cognitive or
#' biomarker outcome over time.
#'
#' @param data A data frame with columns \code{id}, \code{time}, and
#'   \code{value}, and optionally \code{group}.
#' @param outcome Name of the outcome variable (for axis label).
#' @param group_var Optional grouping variable name for colour.
#'
#' @return A \link[ggplot2]{ggplot} object. Requires \pkg{ggplot2}.
#'
#' @export
plot_longitudinal <- function(data, outcome, group_var = NULL) {
  stop("Not implemented yet.")
}

#' Generate a diagnostic summary table
#'
#' Produces a publication-ready summary table of diagnostic performance
#' metrics.
#'
#' @param accuracy_result Output from \code{\link{compute_diagnostic_accuracy}}.
#'
#' @return A data frame formatted for printing, with metric names and values.
#'
#' @export
summary_diagnostic_table <- function(accuracy_result) {
  stop("Not implemented yet.")
}
