library("regressoR.base")
context("model.protect")

test_that("Test model.protect 1",{
  f <- function(x) {
    if(x < -5) { NaN }
    else {
      if(x > 5) { +Inf; }
      else {
        if((x > 2) && (x < 3)) { -Inf; }
        else { (x - 3) * 7 }
      }
    }
  };

  x <- -5:5;
  y <- vapply(X=x, FUN=f, FUN.VALUE = -Inf);

  f.p <- model.protect(f, x, y);
  expect_identical(y, vapply(X=x, FUN=f.p, FUN.VALUE = -Inf))

  expect_identical(f.p(-5), f(-5));
  expect_identical(f.p(-6), f(-5));

  expect_identical(f.p(5), f(5));
  expect_identical(f.p(6), f(5));

  expect_equal(f.p(2.5), 0.5*(f(2) + f(3)));
  expect_equal(f.p(2.25), 0.75*f(2) + 0.25*f(3));
})

test_that("Test model.protect 2",{
  f <- function(x) {
    if(x < -5) { NaN }
    else {
      if(x > 5) { +Inf; }
      else {
        if((x > 2) && (x < 3)) { -Inf; }
        else { (x - 3) * 7 }
      }
    }
  };

  x <- -5:5;
  y <- vapply(X=x, FUN=f, FUN.VALUE = -Inf);

  f.p <- model.protect(f, x, y);
  expect_identical(y, vapply(X=x, FUN=f.p, FUN.VALUE = -Inf))

  x.2 <- c(-5, -6, 5, 6, 2.5, 2.25);
  y.2 <- c(f(-5), f(-5), f(5), f(5), 0.5*(f(2) + f(3)), 0.75*f(2) + 0.25*f(3));

  expect_identical(f.p(x.2), y.2);
})

