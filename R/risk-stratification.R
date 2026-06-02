#' Compute MCI-to-dementia progression risk score
#'
#' Estimates the probability of progression from Mild Cognitive Impairment (MCI)
#' to Alzheimer's dementia within a given time horizon using selected cognitive,
#' biomarker, and demographic predictors.
#'
#' @param age Age in years.
#' @param mmse MMSE total score (0--30).
#' @param cdr_sb Clinical Dementia Rating sum of boxes.
#' @param apoe4 Logical, whether at least one APOE ε4 allele is present.
#' @param abeta_status CSF or amyloid PET status: \code{"positive"} or
#'   \code{"negative"}.
#' @param time_horizon Months for the risk projection (default 36).
#'
#' @return A list with elements \code{risk_score} (0--1 probability) and
#'   \code{risk_category} (\code{"low"}, \code{"moderate"}, \code{"high"}).
#'
#' @export
compute_progression_risk <- function(age, mmse, cdr_sb, apoe4,
                                      abeta_status,
                                      time_horizon = 36) {
  stop("Not implemented yet.")
}

#' Classify cognitive staging
#'
#' Assigns a cognitive stage based on clinical and biomarker data using
#' common research frameworks (e.g., NIA-AA 2018 criteria).
#'
#' @param mmse MMSE total score (0--30).
#' @param cdr_global Global CDR score (0--3).
#' @param abeta_status Amyloid status: \code{"positive"} or \code{"negative"}.
#' @param tau_status Tau status: \code{"positive"} or \code{"negative"}.
#'
#' @return A factor with levels \code{"cognitively_normal"},
#'   \code{"preclinical_ad"}, \code{"mci"}, \code{"dementia"}.
#'
#' @export
classify_cognitive_stage <- function(mmse, cdr_global, abeta_status, tau_status) {
  stop("Not implemented yet.")
}
