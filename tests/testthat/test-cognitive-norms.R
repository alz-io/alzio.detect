test_that("compute_zscore internal works", {
  z <- compute_zscore(c(25, 26, 27, 28, 29, 30), test = "mmse")
  expect_length(z, 6)
  expect_equal(mean(z), 0, tolerance = 1e-12)
  expect_equal(sd(z), 1, tolerance = 1e-12)
})

test_that("compute_zscore errors on zero variance", {
  expect_error(compute_zscore(c(25, 25, 25), test = "mmse"))
})

test_that("compute_rci returns correct values", {
  rci <- compute_rci(25, 20, test_retest_r = 0.8, sd_baseline = 5)
  se_diff <- 5 * sqrt(2 * (1 - 0.8))
  expected <- -5 / se_diff
  expect_equal(rci, expected)
})

test_that("compute_rci errors on invalid input", {
  expect_error(compute_rci(25, 20, test_retest_r = 0.8, sd_baseline = 0))
  expect_error(compute_rci(25, 20, test_retest_r = 1.5, sd_baseline = 5))
})
