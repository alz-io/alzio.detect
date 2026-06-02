test_that("compute_progression_risk returns plausible values", {
  # Low risk profile
  r <- compute_progression_risk(age = 62, mmse = 28, cdr_sb = 1,
                                 apoe4 = FALSE, abeta_status = "negative")
  expect_true(r$risk_score < 0.2)
  expect_equal(r$risk_category, "low")

  # High risk profile
  r <- compute_progression_risk(age = 78, mmse = 22, cdr_sb = 4,
                                 apoe4 = TRUE, abeta_status = "positive")
  expect_true(r$risk_score > 0.3)
  expect_false(r$risk_category == "low")
})

test_that("compute_progression_risk errors on invalid input", {
  expect_error(compute_progression_risk(age = 72, mmse = -1, cdr_sb = 3,
                                        apoe4 = TRUE, abeta_status = "positive"))
  expect_error(compute_progression_risk(age = 72, mmse = 24, cdr_sb = -1,
                                        apoe4 = TRUE, abeta_status = "positive"))
})

test_that("classify_cognitive_stage returns correct stages", {
  s <- classify_cognitive_stage(mmse = 28, cdr_global = 0,
                                 abeta_status = "negative", tau_status = "negative")
  expect_equal(as.character(s), "cognitively_normal")

  s <- classify_cognitive_stage(mmse = 28, cdr_global = 0,
                                 abeta_status = "positive", tau_status = "positive")
  expect_equal(as.character(s), "preclinical_ad")

  s <- classify_cognitive_stage(mmse = 24, cdr_global = 0.5,
                                 abeta_status = "positive", tau_status = "positive")
  expect_equal(as.character(s), "mci")

  s <- classify_cognitive_stage(mmse = 20, cdr_global = 1,
                                 abeta_status = "positive", tau_status = "positive")
  expect_equal(as.character(s), "dementia")
})
