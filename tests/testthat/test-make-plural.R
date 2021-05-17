context("make plural")

test_that("pluralize stops when it should", {

  expect_error(pluralize_gift(1))
  expect_error(pluralize_gift(TRUE))
  expect_error(pluralize_gift(matrix(3)))

})

test_that("pluralize does its job correctly", {

  in1 <- "car"
  in2 <- "book"
  in3 <- "baby"
  in4 <- "candy"
  out1 <- "cars"
  out2 <- "books"
  out3 <- "babies"
  out4 <- "candies"

  expect_equal(pluralize_gift(in1), out1)
  expect_equal(pluralize_gift(in2), out2)
  expect_equal(pluralize_gift(in3), out3)
  expect_equal(pluralize_gift(in4), out4)

})
