test_that("classify_csf returns correct AT(N)", {
  # A+T+N+
  r <- classify_csf(850, 400, 35)
  expect_equal(r$ATN_class, "A+T+N+")
  expect_equal(r$abeta_status, "positive")
  expect_equal(r$tau_status, "positive")
  expect_equal(r$neurodeg_status, "positive")

  # A-T-N- (normal)
  r <- classify_csf(1100, 250, 18)
  expect_equal(r$ATN_class, "A-T-N-")

  # A+ via ratio
  r <- classify_csf(1000, 300, 28)
  ratio <- 28 / 1000
  expect_true(ratio > 0.024)
  expect_equal(r$abeta_status, "positive")
})

test_that("classify_apoe_risk handles formats", {
  expect_equal(as.character(classify_apoe_risk("3/3")), "neutral")
  expect_equal(as.character(classify_apoe_risk("3/4")), "moderate_risk")
  expect_equal(as.character(classify_apoe_risk("4/4")), "high_risk")
  expect_equal(as.character(classify_apoe_risk("2/3")), "protective")
  expect_equal(as.character(classify_apoe_risk("E3/E4")), "moderate_risk")
  expect_equal(as.character(classify_apoe_risk("34")), "moderate_risk")
})

test_that("interpret_blood_biomarkers returns data frame", {
  r <- interpret_blood_biomarkers(ptau217 = 0.45, nfl = 32, gfap = 220, age = 68)
  expect_s3_class(r, "data.frame")
  expect_equal(nrow(r), 3)
  expect_true(all(c("biomarker", "interpretation") %in% names(r)))
})

test_that("interpret_blood_biomarkers errors without data", {
  expect_error(interpret_blood_biomarkers())
})
