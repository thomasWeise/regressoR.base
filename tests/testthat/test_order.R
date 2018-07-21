library("regressoR.base")
context("xy.order")

test_that("Test xy.order",{
  x <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4);
  y <- c(1, 3, 2, 1, 4, 1, 2, 1, 2, 5, 2, 3);
  expect_identical(xy.order(x, y),
                   c(2L, 3L, 1L, 5L, 4L, 6L, 7L, 9L, 8L, 10L, 12L, 11L));
})
