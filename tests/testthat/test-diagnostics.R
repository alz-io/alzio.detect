test_that("compute_diagnostic_accuracy returns correct structure", {
  truth <- c(1, 1, 1, 0, 0, 1, 0, 0, 1, 0)
  pred  <- c(0.9, 0.8, 0.7, 0.3, 0.2, 0.85, 0.1, 0.4, 0.75, 0.25)
  r <- compute_diagnostic_accuracy(truth, pred)
  expect_type(r, "list")
  expect_true(r$auc > 0.8)
  expect_true(r$auc <= 1)
  expect_true(r$sensitivity > 0)
  expect_true(r$specificity > 0)
  expect_s3_class(r$roc_curve, "data.frame")
  expect_true(all(c("specificity", "sensitivity") %in% names(r$roc_curve)))
})

test_that("train_classifier glm works", {
  set.seed(42)
  n <- 50
  dat <- data.frame(
    dx = factor(sample(c("AD", "CN"), n, replace = TRUE)),
    age = rnorm(n, 70, 8),
    mmse = sample(20:30, n, replace = TRUE)
  )
  m <- train_classifier(dx ~ age + mmse, data = dat, method = "glm")
  expect_s3_class(m, "glm")
})

test_that("predict_classifier works for glm", {
  set.seed(42)
  n <- 50
  dat <- data.frame(
    dx = factor(sample(c("AD", "CN"), n, replace = TRUE)),
    age = rnorm(n, 70, 8),
    mmse = sample(20:30, n, replace = TRUE)
  )
  m <- train_classifier(dx ~ age + mmse, data = dat, method = "glm")
  nd <- data.frame(age = 72, mmse = 24)
  p <- predict_classifier(m, nd)
  expect_s3_class(p, "data.frame")
  expect_true(all(c("predicted_class", "probability", "lower", "upper") %in% names(p)))
  expect_true(p$lower <= p$probability)
  expect_true(p$upper >= p$probability)
})
