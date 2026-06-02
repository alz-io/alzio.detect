#' Score the Mini-Mental State Examination (MMSE)
#'
#' Calculates the total MMSE score from item-level responses.
#'
#' @param items A numeric vector or matrix of 30 item responses (0/1).
#'   Missing values are treated as incorrect.
#'
#' @return The total MMSE score (integer between 0 and 30).
#'
#' @export
score_mmse <- function(items) {
  stop("Not implemented yet.")
}

#' Score the Montreal Cognitive Assessment (MoCA)
#'
#' Calculates the total MoCA score from domain-level or item-level responses.
#'
#' @param items A numeric vector or matrix of item or domain scores.
#' @param education_years Years of education for the education adjustment.
#'   If \code{NULL}, no adjustment is applied.
#'
#' @return The total MoCA score (integer 0--30), education-adjusted if applicable.
#'
#' @export
score_moca <- function(items, education_years = NULL) {
  stop("Not implemented yet.")
}

#' Score the Clinical Dementia Rating (CDR)
#'
#' Computes the global CDR score and sum of boxes (CDR-SB) from domain ratings.
#'
#' @param memory Memory domain rating (0--3).
#' @param orientation Orientation domain rating (0--3).
#' @param judgment Judgment and problem solving rating (0--3).
#' @param community Community affairs rating (0--3).
#' @param home_hobbies Home and hobbies rating (0--3).
#' @param personal_care Personal care rating (0--3).
#'
#' @return A list with elements \code{global} (the global CDR score) and
#'   \code{sb} (the sum of boxes).
#'
#' @export
score_cdr <- function(memory, orientation, judgment,
                      community, home_hobbies, personal_care) {
  stop("Not implemented yet.")
}

#' Score the ADAS-Cog13
#'
#' Calculates the total ADAS-Cog13 score from item-level data.
#'
#' @param items A data frame or matrix with columns matching ADAS-Cog13 items.
#'
#' @return The total ADAS-Cog13 score (range 0--85).
#'
#' @export
score_adas_cog13 <- function(items) {
  stop("Not implemented yet.")
}
