#' Compute ROC curve and diagnostic utility metrics
#'
#' Calculates the ROC curve, area under the curve (AUC), optimal cutoff
#' via Youden's index (Youden, 1950), and associated diagnostic accuracy
#' metrics. Uses the standard non-parametric trapezoidal AUC
#' (Fawcett, 2006).
#'
#' @param truth A logical or binary (0/1) vector of true labels.
#'   The positive class is \code{TRUE} or \code{1}.
#' @param predicted A numeric vector of predicted probabilities or scores.
#'   Higher values should indicate higher probability of the positive class.
#'
#' @return A list with elements:
#'   \itemize{
#'     \item \code{auc}: Area under the ROC curve (numeric).
#'     \item \code{cutoff}: Optimal cutoff via Youden's index.
#'     \item \code{sensitivity}: Sensitivity at optimal cutoff.
#'     \item \code{specificity}: Specificity at optimal cutoff.
#'     \item \code{ppv}: Positive predictive value at optimal cutoff.
#'     \item \code{npv}: Negative predictive value at optimal cutoff.
#'     \item \code{lr_plus}: Positive likelihood ratio (sens / (1 - spec)).
#'     \item \code{lr_minus}: Negative likelihood ratio ((1 - sens) / spec).
#'     \item \code{accuracy}: Overall classification accuracy at optimal cutoff.
#'     \item \code{roc_curve}: A data frame with columns \code{specificity}
#'       and \code{sensitivity} for plotting.
#'   }
#'
#' @references
#' Fawcett T (2006) An introduction to ROC analysis.
#' \emph{Pattern Recognition Letters} 27(8):861–874.
#' \doi{10.1016/j.patrec.2005.10.010}
#'
#' Youden WJ (1950) Index for rating diagnostic tests.
#' \emph{Cancer} 3(1):32–35.
#' \doi{10.1002/1097-0142(1950)3:1<32::AID-CNCR2820030106>3.0.CO;2-3}
#'
#' Simundić AM (2009) Measures of diagnostic accuracy—basic definitions.
#' \emph{Biochemia Medica} 19(2):120–130.
#' \doi{10.11613/BM.2009.011}
#'
#' @export
compute_diagnostic_accuracy <- function(truth, predicted) {
  truth <- as.logical(truth)
  if (anyNA(truth)) {
    stop("'truth' must not contain missing values.")
  }
  predicted <- as.numeric(predicted)
  if (length(truth) != length(predicted)) {
    stop("'truth' and 'predicted' must have the same length.")
  }

  # Sort by descending predicted score
  ord <- order(predicted, decreasing = TRUE)
  truth <- truth[ord]
  predicted <- predicted[ord]
  n_pos <- sum(truth)
  n_neg <- sum(!truth)

  # Sweep over unique predicted values to build ROC curve
  thresholds <- unique(predicted)
  thresholds <- sort(thresholds, decreasing = TRUE)
  n_thresh <- length(thresholds)
  tpr <- numeric(n_thresh + 1L)
  fpr <- numeric(n_thresh + 1L)
  tpr[1L] <- 0
  fpr[1L] <- 0

  for (i in seq_len(n_thresh)) {
    pred_pos <- predicted >= thresholds[i]
    tp <- sum(pred_pos & truth)
    fp <- sum(pred_pos & !truth)
    tpr[i + 1L] <- tp / n_pos
    fpr[i + 1L] <- fp / n_neg
  }

  # AUC via trapezoidal rule
  auc <- 0
  for (i in seq_len(n_thresh)) {
    auc <- auc + (fpr[i + 1L] - fpr[i]) * (tpr[i + 1L] + tpr[i]) / 2
  }

  # Youden's index: J = sensitivity + specificity - 1
  spec <- 1 - fpr
  youden <- tpr + spec - 1
  best_idx <- which.max(youden)
  best_cutoff <- if (best_idx == 1L) -Inf else thresholds[best_idx - 1L]
  best_sens <- tpr[best_idx]
  best_spec <- spec[best_idx]

  # Confusion matrix at optimal cutoff
  pred_class <- predicted >= best_cutoff
  tp <- sum(pred_class & truth)
  fp <- sum(pred_class & !truth)
  fn <- sum(!pred_class & truth)
  tn <- sum(!pred_class & !truth)

  ppv <- tp / (tp + fp)
  npv <- tn / (tn + fn)
  lr_plus <- best_sens / (1 - best_spec)
  lr_minus <- (1 - best_sens) / best_spec
  acc <- (tp + tn) / length(truth)

  list(
    auc = auc,
    cutoff = best_cutoff,
    sensitivity = best_sens,
    specificity = best_spec,
    ppv = ppv,
    npv = npv,
    lr_plus = lr_plus,
    lr_minus = lr_minus,
    accuracy = acc,
    roc_curve = data.frame(
      specificity = 1 - fpr,
      sensitivity = tpr,
      stringsAsFactors = FALSE
    )
  )
}

#' Train a classifier for AD diagnosis
#'
#' Fits a binary classifier to predict diagnostic status from cognitive,
#' biomarker, and demographic features.
#'
#' Two methods are supported:
#' \itemize{
#'   \item \code{"glm"}: Logistic regression via \code{\link[stats]{glm}}
#'     with \code{family = binomial}. Suitable for interpretable models
#'     with few predictors (Hosmer et al., 2013).
#'   \item \code{"rf"}: Random forest via the \pkg{ranger} package
#'     (Wright & Ziegler, 2017). Handles non-linear relationships and
#'     high-dimensional data. \emph{Requires ranger to be installed.}
#' }
#'
#' @param formula A model formula (e.g., \code{dx ~ age + mmse + ptau217}).
#' @param data A data frame containing the variables.
#' @param method Method to use: \code{"glm"} (logistic regression, default)
#'   or \code{"rf"} (random forest via ranger).
#' @param ... Additional arguments passed to the underlying fit function
#'   (\code{\link[stats]{glm}} or \code{\link[ranger]{ranger}}).
#'
#' @return A fitted model object of class \code{"glm"} or \code{"ranger"}.
#'
#' @references
#' Hosmer DW, Lemeshow S, Sturdivant RX (2013) \emph{Applied Logistic
#' Regression}, 3rd ed. Wiley. \doi{10.1002/9781118548387}
#'
#' Wright MN, Ziegler A (2017) ranger: A fast implementation of random
#' forests for high dimensional data in C++ and R.
#' \emph{Journal of Statistical Software} 77(1):1–17.
#' \doi{10.18637/jss.v077.i01}
#'
#' @export
train_classifier <- function(formula, data, method = c("glm", "rf"), ...) {
  method <- match.arg(method)

  if (method == "glm") {
    stats::glm(formula, data = data, family = stats::binomial, ...)
  } else if (method == "rf") {
    if (!requireNamespace("ranger", quietly = TRUE)) {
      stop("Package 'ranger' is required for method = 'rf'. ",
           "Install it with install.packages('ranger').")
    }
    ranger::ranger(formula, data = data,
                   probability = TRUE, ...)
  }
}

#' Predict with uncertainty intervals
#'
#' Generates class predictions with confidence intervals. For logistic
#' regression (\code{glm}) models, uses the delta method via the linear
#' predictor's standard errors (Hosmer et al., 2013, Section 3.3).
#' For random forest (\code{ranger}) models, extracts the predicted
#' probabilities directly (bootstrap-based CIs are not computed by ranger).
#'
#' @param model A fitted model from \code{\link{train_classifier}}.
#' @param newdata A data frame of new observations.
#' @param level Confidence level for intervals (default 0.95).
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{predicted_class}: Binary prediction (0/1) at 0.5 cutoff.
#'     \item \code{probability}: Predicted probability of the positive class.
#'     \item \code{lower}: Lower confidence bound (GLM only).
#'     \item \code{upper}: Upper confidence bound (GLM only).
#'   }
#'
#' @references
#' Hosmer DW, Lemeshow S, Sturdivant RX (2013) \emph{Applied Logistic
#' Regression}, 3rd ed. Wiley. \doi{10.1002/9781118548387}
#'
#' @export
predict_classifier <- function(model, newdata, level = 0.95) {
  alpha <- 1 - level
  z <- stats::qnorm(1 - alpha / 2)

  if (inherits(model, "glm")) {
    pred <- stats::predict.glm(model, newdata = newdata,
                                se.fit = TRUE, type = "link")
    logit <- pred$fit
    se <- pred$se.fit
    lower_logit <- logit - z * se
    upper_logit <- logit + z * se
    prob <- stats::plogis(logit)
    lower <- stats::plogis(lower_logit)
    upper <- stats::plogis(upper_logit)
  } else if (inherits(model, "ranger")) {
    pred <- stats::predict(model, data = newdata)$predictions
    if (is.matrix(pred)) {
      prob <- pred[, 2L, drop = TRUE]
    } else {
      prob <- pred
    }
    lower <- NA_real_
    upper <- NA_real_
  } else {
    stop("Unsupported model class. Use 'glm' or 'ranger'.")
  }

  data.frame(
    predicted_class = as.integer(prob >= 0.5),
    probability = prob,
    lower = lower,
    upper = upper,
    stringsAsFactors = FALSE
  )
}
