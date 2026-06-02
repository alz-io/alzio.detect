test_that("read_ad_data with custom source returns as-is", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(x = 1:3, y = letters[1:3]), tmp, row.names = FALSE)
  dat <- read_ad_data(tmp, source = "custom")
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 3)
  unlink(tmp)
})

test_that("list_reference_data returns empty data frame", {
  ref <- list_reference_data()
  expect_s3_class(ref, "data.frame")
  expect_equal(nrow(ref), 0)
})
