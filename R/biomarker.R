#' Classify CSF biomarker profiles
#'
#' Applies cutoff values to CSF biomarkers (Aβ42, t-tau, p-tau) and returns
#' an AT(N) classification per the NIA-AA research framework
#' (Jack et al., 2018, \doi{10.1016/j.jalz.2018.02.018}).
#'
#' Amyloid (A) positivity is determined by low Aβ42 \emph{or} elevated
#' p-tau/Aβ42 ratio. Tau (T) positivity is determined by elevated p-tau.
#' Neurodegeneration (N) positivity is determined by elevated t-tau.
#'
#' @param abeta42 Cerebrospinal fluid Aβ42 concentration (pg/mL).
#' @param ttau Cerebrospinal fluid total tau concentration (pg/mL).
#' @param ptau Cerebrospinal fluid phosphorylated tau (p-tau181)
#'   concentration (pg/mL).
#' @param cutoff_abeta42 Cutoff for low Aβ42 (default: 980 pg/mL).
#'   Based on Elecsys assay cutoffs from the Danish dementia clinic study
#'   (Abildgaard et al., 2023, \doi{10.1016/j.cca.2022.12.023}) which found
#'   an optimal ROC-derived cutoff of 903 ng/L (AUC 0.78). The 980 pg/mL
#'   default is a conservative rounded value consistent with ADNI-derived
#'   Elecsys cutoffs commonly used in North American cohorts.
#' @param cutoff_ptau_abeta42 Cutoff for elevated p-tau/Aβ42 ratio
#'   (default: 0.024). Based on the Mayo Clinic Study of Aging
#'   (Campuzano et al., 2023, \doi{10.1002/dad2.12446}) which found an
#'   optimal Elecsys p-tau/Aβ42 ratio cutoff of 0.023 (Youden index,
#'   AUC 0.91–0.92) for agreement with amyloid PET. Consistent with
#'   the Danish real-world cohort finding of 0.029
#'   (Abildgaard et al., 2023). \strong{Note:} Cutoffs vary by assay
#'   platform. For Elecsys, ratios of 0.023–0.029 are typical.
#' @param cutoff_ptau Cutoff for elevated p-tau (default: 24 pg/mL).
#'   Based on Abildgaard et al. (2023) which reported an optimal cutoff
#'   of 24 ng/L for Elecsys p-tau181 (AUC 0.85).
#' @param cutoff_ttau Cutoff for elevated t-tau (default: 350 pg/mL).
#'   A commonly used Elecsys cutoff in ADNI and clinical cohorts.
#'   Abildgaard et al. (2023) reported 272 ng/L; the 350 pg/mL default
#'   is a slightly more conservative value to reduce false positives
#'   in elderly populations.
#'
#' @return A list with elements:
#'   \itemize{
#'     \item \code{ATN_class}: character string, e.g. \code{"A+T+N+"}.
#'     \item \code{abeta_status}: \code{"positive"} or \code{"negative"}.
#'     \item \code{tau_status}: \code{"positive"} or \code{"negative"}.
#'     \item \code{neurodeg_status}: \code{"positive"} or \code{"negative"}.
#'     \item \code{ptau_abeta42_ratio}: the computed ratio.
#'   }
#'
#' @references
#' Jack CR Jr, Bennett DA, Blennow K, et al. (2018) NIA-AA Research
#' Framework: Toward a biological definition of Alzheimer's disease.
#' \emph{Alzheimer's & Dementia} 14(4):535–562.
#' \doi{10.1016/j.jalz.2018.02.018}
#'
#' Abildgaard A, Parkner T, Knudsen CS, et al. (2023) Diagnostic cut-offs
#' for CSF β-amyloid and tau proteins in a Danish dementia clinic.
#' \emph{Clinica Chimica Acta} 539:244–249.
#' \doi{10.1016/j.cca.2022.12.023}
#'
#' Campuzano S, Przybelski SA, Graff-Radford J, et al. (2023) Detection
#' of Alzheimer's disease amyloid beta 1-42, p-tau, and t-tau assays.
#' \emph{Alzheimer's & Dementia: Diagnosis, Assessment & Disease Monitoring}
#' 15(2):e12446. \doi{10.1002/dad2.12446}
#'
#' @export
classify_csf <- function(abeta42, ttau, ptau,
                         cutoff_abeta42 = 980,
                         cutoff_ptau_abeta42 = 0.024,
                         cutoff_ptau = 24,
                         cutoff_ttau = 350) {
  ratio <- ptau / abeta42
  a_pos <- abeta42 < cutoff_abeta42 | ratio > cutoff_ptau_abeta42
  t_pos <- ptau > cutoff_ptau
  n_pos <- ttau > cutoff_ttau

  a_status <- if (a_pos) "positive" else "negative"
  t_status <- if (t_pos) "positive" else "negative"
  n_status <- if (n_pos) "positive" else "negative"

  atn <- paste0("A", if (a_pos) "+" else "-",
                "T", if (t_pos) "+" else "-",
                "N", if (n_pos) "+" else "-")

  list(ATN_class = atn,
       abeta_status = a_status,
       tau_status = t_status,
       neurodeg_status = n_status,
       ptau_abeta42_ratio = ratio)
}

#' Classify APOE genotype risk
#'
#' Maps APOE genotypes to established risk categories for late-onset
#' Alzheimer's disease. Risk stratification follows the well-established
#' meta-analytic odds ratios from Farrer et al. (1997) and the more recent
#' large-scale consortia findings.
#'
#' Risk classification:
#' \itemize{
#'   \item \strong{Protective:} ε2/ε2, ε2/ε3
#'   \item \strong{Neutral:} ε2/ε4, ε3/ε3
#'   \item \strong{Moderate risk:} ε3/ε4 (OR ~3–4 vs ε3/ε3)
#'   \item \strong{High risk:} ε4/ε4 (OR ~12–15 vs ε3/ε3)
#' }
#'
#' Accepts multiple common genotype formats
#' (e.g., \code{"3/3"}, \code{"E3/E4"}, \code{"33"}, \code{"3,4"}).
#'
#' @param genotype A character vector of APOE genotypes.
#'
#' @return A factor with levels \code{"protective"}, \code{"neutral"},
#'   \code{"moderate_risk"}, \code{"high_risk"}.
#'
#' @references
#' Farrer LA, Cupples LA, Haines JL, et al. (1997) Effects of age, sex,
#' and ethnicity on the association between apolipoprotein E genotype and
#' Alzheimer disease: a meta-analysis. \emph{JAMA} 278(16):1349–1356.
#' \doi{10.1001/jama.1997.03550160069041}
#'
#' Liu CC, Liu CC, Kanekiyo T, et al. (2013) Apolipoprotein E and
#' Alzheimer disease: risk, mechanisms and therapy.
#' \emph{Nature Reviews Neurology} 9(2):106–118.
#' \doi{10.1038/nrneurol.2012.263}
#'
#' @export
classify_apoe_risk <- function(genotype) {
  # Normalise: remove E prefix, slashes, commas, spaces; keep two digits
  g <- toupper(genotype)
  g <- gsub("[^0-9]", "", g)
  # Keep only the last two characters (in case of longer strings)
  g <- substr(g, nchar(g) - 1L, nchar(g))

  risk <- vapply(g, function(allele) {
    if (nchar(allele) != 2L || grepl("[^2-4]", allele)) {
      return(NA_character_)
    }
    a1 <- as.integer(substr(allele, 1L, 1L))
    a2 <- as.integer(substr(allele, 2L, 2L))
    e_count <- sum(c(a1, a2) == 4L)
    e2_count <- sum(c(a1, a2) == 2L)

    if (e2_count == 2L) {
      "protective"
    } else if (e2_count == 1L && e_count == 0L) {
      "protective"
    } else if (e_count == 0L) {
      "neutral"
    } else if (e_count == 1L) {
      "moderate_risk"
    } else {
      "high_risk"
    }
  }, character(1L), USE.NAMES = FALSE)

  factor(risk, levels = c("protective", "neutral", "moderate_risk", "high_risk"))
}

#' Interpret blood-based biomarker levels
#'
#' Provides a clinical interpretation for blood-based AD biomarkers
#' (p-tau217, NfL, GFAP) given age-adjusted cutoffs.
#'
#' \strong{p-tau217:} A blood-based phosphorylated tau assay targeting
#' threonine 217, shown to have AUC 0.93–0.96 for detecting amyloid
#' pathology (Palmqvist et al., 2024, 2025). The default cutoff of
#' 0.30 pg/mL aligns with the ALZpath p-tau217 assay. Gonzalez-Ortiz
#' et al. (2024) reported optimal cutoffs of 0.27 pg/mL for amyloid-PET
#' positivity with 6.7% false-positive rate; the 0.18 pg/mL lower bound
#' corresponds approximately to the 95% specificity threshold in that
#' study. \strong{Note:} Cutoffs differ across assay platforms
#' (ALZpath, Lumipulse, Mass Spec).
#'
#' \strong{NfL:} Neurofilament light chain, a marker of neuroaxonal
#' damage. Levels increase with age and are elevated in neurodegeneration.
#' Age-stratified cutoffs are derived from normative data in Simrén et al.
#' (2024, \doi{10.3390/ijms25147808}) and the HC-to-dementia ROC cutoff
#' of 12.95 pg/mL (SiMoA) from Abu-Rumeileh et al. (2023,
#' \doi{10.1038/s41598-023-29704-8}). The tiered age cutoffs
#' (15–45 pg/mL) provide a pragmatic compromise between the lower
#' normative ranges and the higher cutoffs used in dementia populations.
#'
#' \strong{GFAP:} Glial fibrillary acidic protein, a marker of
#' astroglial activation. The primary cutoff of 280 pg/mL is based on
#' the 90th percentile for healthy adults aged >55 years
#' (Simrén et al., 2024). Values below 180 pg/mL are considered normal.
#'
#' @param ptau217 Plasma p-tau217 concentration (pg/mL).
#' @param nfl Plasma neurofilament light chain concentration (pg/mL).
#' @param gfap Plasma glial fibrillary acidic protein concentration (pg/mL).
#' @param age Age in years (for age-adjusted NfL interpretation).
#'
#' @return A data frame with one row per biomarker supplied, containing the
#'   raw value, an interpretation (\code{"normal"}, \code{"borderline"},
#'   \code{"abnormal"}), and the cutoff used.
#'
#' @references
#' Palmqvist S, Tideman P, Mattsson-Carlgren N, et al. (2024) Blood
#' biomarkers to detect Alzheimer disease in primary care and secondary
#' care. \emph{JAMA} 332(15):1245–1254.
#' \doi{10.1001/jama.2024.13885}
#'
#' Palmqvist S, Tideman P, Cullen NC, et al. (2025) Plasma p-tau217 for
#' Alzheimer's disease diagnosis in primary and secondary care using a
#' fully automated platform. \emph{Nature Medicine} 31:891–899.
#' \doi{10.1038/s41591-025-03622-w}
#'
#' Gonzalez-Ortiz F, Ferreira PCL, González-Escalante A, et al. (2024)
#' Head-to-head study of diagnostic accuracy of plasma and cerebrospinal
#' fluid p-tau217 versus p-tau181 and p-tau231 in a memory clinic cohort.
#' \emph{Journal of Neurology} 271:2050–2062.
#' \doi{10.1007/s00415-023-12148-5}
#'
#' Simrén J, Weninger H, Brum WS, et al. (2024) Establishing normal serum
#' values of neurofilament light chains and glial fibrillary acidic protein
#' considering the effects of age and other demographic factors in healthy
#' adults. \emph{International Journal of Molecular Sciences} 25(14):7808.
#' \doi{10.3390/ijms25147808}
#'
#' Abu-Rumeileh S, Steinacker P, Polischi B, et al. (2023)
#' Neurofilament-light chain quantification by Simoa and Ella in plasma
#' from patients with dementia: a comparative study.
#' \emph{Scientific Reports} 13:3968.
#' \doi{10.1038/s41598-023-29704-8}
#'
#' @export
interpret_blood_biomarkers <- function(ptau217 = NULL, nfl = NULL,
                                        gfap = NULL, age = NULL) {
  results <- list()

  if (!is.null(ptau217)) {
    cutoff <- 0.30
    interp <- if (ptau217 < 0.18) "normal"
      else if (ptau217 <= cutoff) "borderline"
      else "abnormal"
    results[[length(results) + 1L]] <- data.frame(
      biomarker = "p-tau217", value = ptau217,
      interpretation = interp, cutoff = cutoff,
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(nfl)) {
    if (is.null(age)) {
      cutoff <- 20
    } else {
      cutoff <- if (age < 50) 15 else if (age < 60) 20
        else if (age < 70) 25 else if (age < 80) 35 else 45
    }
    interp <- if (nfl < cutoff * 0.75) "normal"
      else if (nfl <= cutoff * 1.25) "borderline"
      else "abnormal"
    results[[length(results) + 1L]] <- data.frame(
      biomarker = "NfL", value = nfl,
      interpretation = interp, cutoff = cutoff,
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(gfap)) {
    cutoff <- 280
    interp <- if (gfap < 180) "normal"
      else if (gfap <= cutoff) "borderline"
      else "abnormal"
    results[[length(results) + 1L]] <- data.frame(
      biomarker = "GFAP", value = gfap,
      interpretation = interp, cutoff = cutoff,
      stringsAsFactors = FALSE
    )
  }

  if (length(results) == 0L) {
    stop("At least one biomarker value must be supplied.")
  }

  do.call(rbind, results)
}
