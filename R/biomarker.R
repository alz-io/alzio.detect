#' Classify CSF biomarker profiles
#'
#' Applies cutoff values to CSF biomarkers (Aβ42, t-tau, p-tau) and
#' returns an AT(N) classification.
#'
#' @param abeta42 Cerebrospinal fluid Aβ42 concentration (pg/mL).
#' @param ttau Cerebrospinal fluid total tau concentration (pg/mL).
#' @param ptau Cerebrospinal fluid phosphorylated tau (p-tau181) concentration (pg/mL).
#' @param cutoff_abeta42 Cutoff for Aβ42 abnormality (default: 980 pg/mL).
#' @param cutoff_ptau_abeta42 Cutoff for p-tau/Aβ42 ratio abnormality (default: 0.08).
#'
#' @return A list with elements \code{ATN_class} (character, e.g., "A+T+N+"),
#'   \code{abeta_status} ("positive" / "negative"),
#'   \code{tau_status} ("positive" / "negative"),
#'   \code{neurodeg_status} ("positive" / "negative").
#'
#' @export
classify_csf <- function(abeta42, ttau, ptau,
                         cutoff_abeta42 = 980,
                         cutoff_ptau_abeta42 = 0.08) {
  stop("Not implemented yet.")
}

#' Classify APOE genotype risk
#'
#' Maps APOE genotypes to established risk categories for late-onset
#' Alzheimer's disease.
#'
#' @param genotype A character vector of APOE genotypes (e.g., "3/3", "3/4", "4/4").
#'
#' @return A factor with levels \code{"protective"}, \code{"neutral"},
#'   \code{"moderate_risk"}, \code{"high_risk"}.
#'
#' @export
classify_apoe_risk <- function(genotype) {
  stop("Not implemented yet.")
}

#' Interpret blood-based biomarker levels
#'
#' Provides a clinical interpretation for blood-based AD biomarkers
#' (p-tau217, NfL, GFAP) given age-adjusted cutoffs.
#'
#' @param ptau217 Plasma p-tau217 concentration (pg/mL).
#' @param nfl Plasma neurofilament light chain concentration (pg/mL).
#' @param gfap Plasma glial fibrillary acidic protein concentration (pg/mL).
#' @param age Age in years (for age-adjusted NfL interpretation).
#'
#' @return A data frame with one row per biomarker, containing the raw value,
#'   an interpretation (\code{"normal"}, \code{"borderline"}, \code{"abnormal"}),
#'   and the cutoff used.
#'
#' @export
interpret_blood_biomarkers <- function(ptau217 = NULL, nfl = NULL,
                                        gfap = NULL, age = NULL) {
  stop("Not implemented yet.")
}
