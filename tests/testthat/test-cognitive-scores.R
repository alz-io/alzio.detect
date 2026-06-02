test_that("score_mmse handles single total", {
  expect_equal(score_mmse(27), 27)
  expect_equal(score_mmse(30), 30)
  expect_equal(score_mmse(0), 0)
})

test_that("score_mmse handles item vector", {
  items <- rep(1, 30)
  expect_equal(score_mmse(items), 30)
  items[30] <- 0
  expect_equal(score_mmse(items), 29)
})

test_that("score_mmse errors on invalid input", {
  expect_error(score_mmse(31))
  expect_error(score_mmse(-1))
  expect_error(score_mmse(rep(1, 29)))
})

test_that("score_mmse treats NA as 0", {
  items <- c(rep(1, 28), NA, NA)
  expect_equal(score_mmse(items), 28)
})

test_that("score_moca handles single total", {
  expect_equal(score_moca(25), 25)
  expect_equal(score_moca(30), 30)
})

test_that("score_moca applies education adjustment", {
  expect_equal(score_moca(25, education_years = 11), 26)
  expect_equal(score_moca(30, education_years = 11), 30)  # capped
  expect_equal(score_moca(25, education_years = 16), 25)
})

test_that("score_cdr returns correct global and sb", {
  r <- score_cdr(0, 0, 0, 0, 0, 0)
  expect_equal(r$global, 0)
  expect_equal(r$sb, 0)

  r <- score_cdr(0.5, 0.5, 0, 0, 0, 0)
  expect_equal(r$global, 0.5)
  expect_equal(r$sb, 1)
})

test_that("score_adas_cog13 handles single total", {
  expect_equal(score_adas_cog13(42), 42)
  expect_error(score_adas_cog13(100))
})

test_that("score_adas_cog13 handles named columns", {
  df <- data.frame(wordrecall = 5, naming = 2, commands = 1,
                   constructional = 1, ideational = 2, orientation = 3,
                   wordrecog = 4, instruction = 1, spokenlang = 1,
                   wordfind = 2, comprehension = 1, delayedrecall = 4,
                   maze = 1)
  expect_equal(score_adas_cog13(df), 28)
})
