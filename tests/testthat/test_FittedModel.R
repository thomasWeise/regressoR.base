library("regressoR.base")
context("FittedModel")

test_that("Test FittedModel constructor", {
  f <- function(x) x
  quality <- 12;
  size <- 7L;
  instance <- new("FittedModel", f=f, quality=quality, size=size);
  methods::validObject(instance);
  expect_identical(instance@quality, quality);
  expect_identical(instance@f, f);
  expect_identical(instance@size, size);

  str <- as.character(instance);
  expect_gt(nchar(str), 0);
  expect_identical(str, FittedModel.as.character(instance));
})

test_that("Test FittedModel constructor error", {
  f <- function(y) y
  quality <- 12;
  size <- 7L;
  expect_error(new("FittedModel", f=f, quality=quality, size=size));

  f <- function(x) x
  quality <- -12;
  size <- 7L;
  expect_error(new("FittedModel", f=f, quality=quality, size=size));

  f <- function(x) x
  quality <- 12;
  size <- -7L;
  expect_error(new("FittedModel", f=f, quality=quality, size=size));

  f <- function(x) x
  quality <- 12;
  size <- 7L;
  expect_error(new("FittedModel", f=f, size=size));
  expect_error(new("FittedModel", quality=quality, size=size));
  expect_error(new("FittedModel", f=f));
  expect_error(new("FittedModel", quality=quality));
  expect_error(new("FittedModel", size=size));
  instance <- new("FittedModel");
  expect_error(validObject(instance));
})


test_that("Test FittedModel.new", {
  f <- function(x) x
  quality <- 12;
  size <- 7L;
  instance <- FittedModel.new(f=f, quality=quality, size=size);
  methods::validObject(instance);
  expect_identical(instance@quality, quality);
  expect_identical(instance@f, f);
  expect_identical(instance@size, size);

  str <- as.character(instance);
  expect_gt(nchar(str), 0);
  expect_identical(str, FittedModel.as.character(instance));
})

test_that("Test FittedModel.new error", {
  f <- function(y) y
  quality <- 12;
  size <- 7L;
  expect_error(FittedModel.new(f=f, quality=quality, size=size));

  f <- function(x) x
  quality <- -12;
  size <- 7L;
  expect_error(FittedModel.new(f=f, quality=quality, size=size));

  f <- function(x) x
  quality <- 12;
  size <- -7L;
  expect_error(FittedModel.new(f=f, quality=quality, size=size));

  f <- function(x) x
  quality <- 12;
  size <- 7L;
  expect_error(FittedModel.new(f=f, size=size));
  expect_error(FittedModel.new(quality=quality, size=size));
  expect_error(FittedModel.new(f=f));
  expect_error(FittedModel.new(quality=quality));
  expect_error(FittedModel.new(f=f, quality=quality));
  expect_error(FittedModel.new(size=size));
  expect_error(FittedModel.new());
})
