#' Score the Mini-Mental State Examination (MMSE)
#'
#' Calculates the total MMSE score from item-level responses or returns
#' the total score directly if a single value is provided.
#'
#' @param items A numeric vector of 30 item responses (0/1) or a single
#'   integer total (0--30). Missing values are treated as incorrect.
#'
#' @return The total MMSE score (integer between 0 and 30).
#'
#' @export
score_mmse <- function(items) {
  items <- as.numeric(items)
  if (length(items) == 1L) {
    if (items < 0 || items > 30) {
      stop("Single-item input must be between 0 and 30.")
    }
    return(round(items))
  }
  if (length(items) != 30L) {
    stop("'items' must be a vector of length 30 (one per item) or a single total.")
  }
  if (any(!items %in% c(0, 1, NA))) {
    stop("Item responses must be 0 or 1.")
  }
  items[is.na(items)] <- 0L
  sum(items)
}

#' Score the Montreal Cognitive Assessment (MoCA)
#'
#' Calculates the total MoCA score from domain-level or item-level responses,
#' applying the standard education adjustment (+1 if education \eqn{\le} 12 years).
#'
#' @param items A numeric vector of item/domain scores, or a single total (0--30).
#' @param education_years Years of education for the education adjustment.
#'   If \code{NULL}, no adjustment is applied.
#'
#' @return The total MoCA score (integer 0--30), education-adjusted if applicable
#'   and capped at 30.
#'
#' @export
score_moca <- function(items, education_years = NULL) {
  items <- as.numeric(items)
  if (length(items) == 1L) {
    if (items < 0 || items > 30) {
      stop("Single-item input must be between 0 and 30.")
    }
    total <- items
  } else {
    total <- sum(items, na.rm = TRUE)
  }
  total <- round(total)
  if (!is.null(education_years)) {
    if (education_years <= 12) total <- total + 1L
  }
  min(total, 30L)
}

#' Score the Clinical Dementia Rating (CDR)
#'
#' Computes the global CDR score using the Washington University algorithm
#' (Morris, 1993) and the sum of boxes (CDR-SB) from domain ratings.
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
  nm <- c(orientation, judgment, community, home_hobbies, personal_care)
  if (any(c(memory, nm) > 3 | c(memory, nm) < 0)) {
    stop("All domain ratings must be between 0 and 3.")
  }

  sb <- memory + orientation + judgment + community + home_hobbies + personal_care

  if (sb == 0) {
    global <- 0
  } else if (memory == 0) {
    global <- if (any(nm > 0)) 0.5 else 0
  } else if (memory == 0.5) {
    global <- if (sum(nm >= 1) >= 3) 1 else 0.5
  } else if (memory == 1) {
    global <- 1
    if (sum(nm >= 2) >= 2) global <- 2
  } else if (memory == 2) {
    global <- 2
    if (sum(nm >= 3) >= 2) global <- 3
  } else {
    global <- 3
  }

  list(global = global, sb = sb)
}

#' Score the ADAS-Cog13
#'
#' Calculates the total ADAS-Cog13 score from item-level data. Accepts
#' a named data frame/matrix (column names matched) or a numeric vector
#' of exactly 13 items in standard order.
#'
#' Standard ADAS-Cog13 item order:
#' \enumerate{
#'   \item Word recall (0--10)
#'   \item Naming (0--5)
#'   \item Commands (0--5)
#'   \item Constructional praxis (0--5)
#'   \item Ideational praxis (0--5)
#'   \item Orientation (0--8)
#'   \item Word recognition (0--12)
#'   \item Remembering instructions (0--5)
#'   \item Spoken language (0--5)
#'   \item Word finding (0--5)
#'   \item Comprehension (0--5)
#'   \item Delayed word recall (0--10)
#'   \item Maze errors (0--5)
#' }
#'
#' @param items A data frame, matrix, or numeric vector. If named, the
#'   function attempts to match columns to item names. If unnamed with
#'   length 13, items are taken in the order above.
#'
#' @return The total ADAS-Cog13 score (range 0--85).
#'
#' @export
score_adas_cog13 <- function(items) {
  known_cols <- c(
    "wordrecall", "naming", "commands", "constructional",
    "ideational", "orientation", "wordrecog",
    "instruction", "spokenlang", "wordfind",
    "comprehension", "delayedrecall", "maze"
  )

  if (is.data.frame(items) || is.matrix(items)) {
    items <- as.data.frame(items)
    matched <- tolower(names(items)) %in% known_cols
    if (any(matched)) {
      scores <- as.numeric(items[, matched, drop = TRUE])
    } else {
      stop("No recognised ADAS-Cog13 column names found.")
    }
  } else {
    scores <- as.numeric(items)
  }

  n <- length(scores)
  if (n == 1L) {
    if (scores < 0 || scores > 85) {
      stop("Single-item input must be between 0 and 85.")
    }
    return(scores)
  }
  if (n != 13L) {
    stop("Need exactly 13 item scores (or a single total).")
  }
  if (any(scores < 0, na.rm = TRUE)) {
    stop("Item scores cannot be negative.")
  }

  sum(scores, na.rm = TRUE)
}
