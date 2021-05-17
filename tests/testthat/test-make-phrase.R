library(twelvedays)
context("make phrase")

test_that("make_phrase stops when it should", {
  expect_error(make_phrase(1))
  expect_error(make_phrase(TRUE))
  expect_error(make_phrase(matrix(3)))
})

test_that("make_phrase does create phrases correctly", {
  out1 <- "Ten lords a-leaping"
  out2 <- "and a pan in a kitchen"

  expect_equal(make_phrase(num = 10, num_word = "ten", item = "lord",
                           verb = "a-leaping", adjective = "", location = ""),
               out1)
  expect_equal(make_phrase(num = 1, num_word = "", item = "pan",
                           verb = "", adjective = "", location = "in a kitchen"),
               out2)
})
