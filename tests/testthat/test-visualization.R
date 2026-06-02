test_that("plot_cognitive_profile errors on insufficient domains", {
  expect_error(plot_cognitive_profile(c(Memory = -1)))
  expect_error(plot_cognitive_profile(c(1, 2)))
})

test_that("plot_cognitive_profile errors on unnamed vector", {
  expect_error(plot_cognitive_profile(c(-1, -0.5, 0.5)))
})

test_that("summary_diagnostic_table returns correct structure", {
  acc <- list(
    auc = 0.91, cutoff = 0.65, sensitivity = 0.85,
    specificity = 0.88, ppv = 0.82, npv = 0.90,
    lr_plus = 7.08, lr_minus = 0.17, accuracy = 0.87
  )
  tbl <- summary_diagnostic_table(acc)
  expect_s3_class(tbl, "data.frame")
  expect_equal(nrow(tbl), 9)
  expect_true(all(c("Metric", "Value") %in% names(tbl)))
})

test_that("plot_longitudinal errors on missing columns", {
  expect_error(plot_longitudinal(data.frame(a = 1, b = 2), "test"))
})
