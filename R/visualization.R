#' Plot a cognitive profile (radar/spider plot)
#'
#' Creates a radar (spider) plot of Z-scores across cognitive domains for a
#' single subject or group. Uses base R graphics so no additional packages
#' are required.
#'
#' @param z_scores A named numeric vector of Z-scores. Names should be
#'   cognitive domain labels (e.g., \code{"Memory"}, \code{"Executive"},
#'   \code{"Language"}). At least 3 domains are needed for a meaningful plot.
#' @param title Optional plot title.
#'
#' @return Invisibly returns the angle-labelled data used to draw the plot.
#'
#' @references
#' The radar plot is a standard visualisation in neuropsychological
#' assessment (Lezak et al., 2012, \emph{Neuropsychological Assessment},
#' 5th ed., Oxford University Press).
#'
#' @export
plot_cognitive_profile <- function(z_scores, title = NULL) {
  if (length(z_scores) < 3L) {
    stop("Provide at least 3 cognitive domains for a radar plot.")
  }
  if (is.null(names(z_scores))) {
    stop("'z_scores' must be a named vector with domain names.")
  }

  n <- length(z_scores)
  angles <- seq(0, 2 * pi, length.out = n + 1L)[-1L]
  x <- z_scores * cos(angles)
  y <- z_scores * sin(angles)
  x <- c(x, x[1L])
  y <- c(y, y[1L])

  # Determine plot range
  max_abs <- max(abs(z_scores), 2)  # at least ±2
  rng <- c(-max_abs, max_abs)

  graphics::par(mar = c(2, 2, 3, 2))
  graphics::plot(x, y, type = "n", xlim = rng, ylim = rng,
       xlab = "", ylab = "", axes = FALSE, asp = 1,
       main = if (is.null(title)) "Cognitive Profile" else title)

  # Draw reference circles
  for (r in seq(-2, 2, by = 1)) {
    if (r == 0) next
    graphics::lines(r * cos(seq(0, 2 * pi, length.out = 100)),
          r * sin(seq(0, 2 * pi, length.out = 100)),
          col = "grey80", lty = 2)
  }
  graphics::abline(h = 0, v = 0, col = "grey90")

  # Draw domain axes and labels
  label_offset <- max_abs * 1.15
  for (i in seq_len(n)) {
    graphics::lines(c(0, label_offset * cos(angles[i])),
          c(0, label_offset * sin(angles[i])),
          col = "grey80")
    graphics::text(label_offset * cos(angles[i]),
         label_offset * sin(angles[i]),
         names(z_scores)[i], cex = 0.8)
  }

  # Fill polygon
  graphics::polygon(x, y, col = rgb(0.2, 0.4, 0.8, 0.3), border = "steelblue", lwd = 2)

  # Legend
  graphics::legend("topright", legend = "Z-score", fill = rgb(0.2, 0.4, 0.8, 0.3),
         bty = "n", cex = 0.8)

  invisible(data.frame(domain = names(z_scores), z_score = z_scores,
                        angle = angles))
}

#' Plot longitudinal trajectories
#'
#' Plots individual trajectories for a cognitive or biomarker outcome
#' over time, with an optional grouping variable and mean trajectory
#' overlay. Uses base R graphics.
#'
#' @param data A data frame with columns \code{id}, \code{time}, and
#'   \code{value}, and optionally \code{group}.
#' @param outcome Name of the outcome variable for axis label (e.g.,
#'   \code{"MMSE"}, \code{"CDR-SB"}, \code{"p-tau217"}).
#' @param group_var Optional column name in \code{data} for grouping
#'   (e.g., \code{"dx"}). Individual lines are coloured by group.
#'
#' @return Invisibly returns the input data.
#'
#' @references
#' Longitudinal trajectory plotting follows conventions in AD clinical
#' research (Aisen et al., 2020, \emph{Alzheimer's & Dementia}).
#'
#' @export
plot_longitudinal <- function(data, outcome, group_var = NULL) {
  required <- c("id", "time", "value")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("'data' must contain columns: ", paste(missing, collapse = ", "))
  }

  time_range <- range(data$time, na.rm = TRUE)
  val_range <- range(data$value, na.rm = TRUE)
  val_range <- val_range + c(-diff(val_range) * 0.05, diff(val_range) * 0.05)

  if (!is.null(group_var)) {
    if (!group_var %in% names(data)) {
      stop("'group_var' column '", group_var, "' not found in data.")
    }
    groups <- unique(data[[group_var]])
    n_groups <- length(groups)
    # Use a simple colour palette
    palette <- c("steelblue", "firebrick", "forestgreen",
                 "orange", "purple", "darkgoldenrod")
    col_map <- setNames(palette[seq_len(n_groups)], groups)
  }

  graphics::par(mar = c(4, 4, 2, 1))
  graphics::plot(NA, xlim = time_range, ylim = val_range,
       xlab = "Time (months)", ylab = outcome,
       main = paste("Longitudinal Trajectory —", outcome))

  ids <- unique(data$id)

  for (sid in ids) {
    sub <- data[data$id == sid, , drop = FALSE]
    sub <- sub[order(sub$time), ]

    if (!is.null(group_var)) {
      g <- sub[[group_var]][1L]
      col <- col_map[[g]]
    } else {
      col <- "grey60"
    }

    graphics::lines(sub$time, sub$value, col = col, lwd = 0.5)
    graphics::points(sub$time, sub$value, col = col, cex = 0.4, pch = 16)
  }

  # Group means
  if (!is.null(group_var)) {
    for (g in groups) {
      sub <- data[data[[group_var]] == g, , drop = FALSE]
      means <- stats::aggregate(value ~ time, data = sub, FUN = mean,
                                na.rm = TRUE)
      graphics::lines(means$time, means$value, col = col_map[[g]],
            lwd = 3)
    }
    graphics::legend("topright", legend = names(col_map), col = col_map,
           lwd = 3, bty = "n", cex = 0.8)
  }

  invisible(data)
}

#' Generate a diagnostic summary table
#'
#' Produces a formatted summary table of diagnostic performance metrics
#' from the output of \code{\link{compute_diagnostic_accuracy}}.
#'
#' @param accuracy_result Output from \code{\link{compute_diagnostic_accuracy}}.
#'
#' @return A data frame with columns \code{Metric} and \code{Value},
#'   suitable for printing or exporting.
#'
#' @export
summary_diagnostic_table <- function(accuracy_result) {
  required <- c("auc", "cutoff", "sensitivity", "specificity",
                "ppv", "npv", "lr_plus", "lr_minus", "accuracy")
  missing <- setdiff(required, names(accuracy_result))
  if (length(missing)) {
    stop("'accuracy_result' missing elements: ", paste(missing, collapse = ", "))
  }

  fmt_val <- function(x, d = 3) format(round(x, d), nsmall = d)

  data.frame(
    Metric = c("AUC",
               "Optimal cutoff",
               "Sensitivity",
               "Specificity",
               "Positive predictive value (PPV)",
               "Negative predictive value (NPV)",
               "Positive likelihood ratio (LR+)",
               "Negative likelihood ratio (LR-)",
               "Accuracy"),
    Value = c(fmt_val(accuracy_result$auc),
              fmt_val(accuracy_result$cutoff),
              fmt_val(accuracy_result$sensitivity),
              fmt_val(accuracy_result$specificity),
              fmt_val(accuracy_result$ppv),
              fmt_val(accuracy_result$npv),
              fmt_val(accuracy_result$lr_plus),
              fmt_val(accuracy_result$lr_minus),
              fmt_val(accuracy_result$accuracy)),
    stringsAsFactors = FALSE
  )
}
