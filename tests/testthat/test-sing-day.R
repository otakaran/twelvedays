context("sing day")

test_that("sing day stops when it should", {
  expect_error(sing_day(1))
  expect_error(sing_day(TRUE))
  expect_error(sing_day(matrix(3)))
})

test_that("sing day does song verses correctly", {
  xmas <- read.csv("https://www.dropbox.com/s/e584pryn8evm1gz/xmas.csv?dl=1")
  xmas <- xmas %>% mutate(Full.Phrase =
                            pmap(list(num=Day,
                                 num_word=Day.in.Words,
                                 item=Gift.Item,
                                 verb=Verb,
                                 adjective=Adjective,
                                 location=Location),
                            make_phrase))

  out1 <- "On the first day of Christmas, my true love sent to me,\na partridge in a pear tree."
  expect_equal(sing_day(xmas, 1, Full.Phrase), out1)
})
