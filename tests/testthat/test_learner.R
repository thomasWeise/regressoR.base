library("regressoR.base")
context("regressoR.applyLearners")

test_that("Test regressoR.applyLearners plain call",{
  expect_identical(
    regressoR.applyLearners(x=c(1,2), y=c(1,2),
                            learners=list(function(metric, transformation.x, transformation.y, metric.transformed, q) NULL)),
    NULL);
})

test_that("Test regressoR.applyLearners errors",{
  expect_error(regressoR.applyLearners());
  expect_error(regressoR.applyLearners(x=c(1,2), y=c(1,2)));
  expect_error(regressoR.applyLearners(x=c(1,2), y=c(1,2),
                                        learners=list(function(metric, transformation.x, transformation.y, q) NULL)));
  expect_error(
    regressoR.applyLearners(x=c(1,2), y=c(1),
                             learners=list(function(metric, transformation.x, transformation.y, metric.transformed, q) NULL)));
  expect_error(
    regressoR.applyLearners(x=c(1,2), y=c(1,2),
                             learners=list(function(metric, transformation.x, transformation.y, metric.transformed, q) NULL),
                             metricGenerator = NULL));
})


test_that("Test regressoR.applyLearners with result",{
  f <- function(x) x
  quality <- 12;
  size <- 7L;
  instance <- FittedModel.new(f=f, quality=quality, size=size);

  expect_identical(
    regressoR.applyLearners(x=c(1), y=c(1),
                             learners=list(function(metric, transformation.x, transformation.y, metric.transformed, q) instance)),
    instance);

  expect_identical(
    regressoR.applyLearners(x=c(1,2), y=c(1,2),
                             learners=list(function(metric, transformation.x, transformation.y, metric.transformed, q) instance)),
    instance);

  expect_identical(
    regressoR.applyLearners(x=c(1, 2, 3), y=c(1, 2, 3),
                             learners=list(function(metric, transformation.x, transformation.y, metric.transformed, q) instance)),
    instance);

  expect_identical(
    regressoR.applyLearners(x=c(1, 2, 3, 4), y=c(1, 2, 3, 4),
                             learners=list(function(metric, transformation.x, transformation.y, metric.transformed, q) instance)),
    instance);
})



test_that("Test regressoR.applyLearners with multiple result",{
  f.1       <- function(x) x
  quality.1 <- 12;
  size.1   <- 7L;
  instance.1 <- FittedModel.new(f=f.1, quality=quality.1, size=size.1);

  f.2       <- function(x) x + 1
  quality.2 <- 2;
  size.2   <- 9L;
  instance.2 <- FittedModel.new(f=f.2, quality=quality.2, size=size.2);

  expect_identical(
    regressoR.applyLearners(x=c(1,2,3,4,5), y=c(1,2,3,4,5), # <- the data fits to the first learner
                             learners=list(
                             function(metric, transformation.x, transformation.y, metric.transformed, q) instance.1,
                             function(metric, transformation.x, transformation.y, metric.transformed, q) instance.2
                             )),
    instance.1);


  expect_identical(
    regressoR.applyLearners(x=c(1,2,3,4,5), y=c(2,3,4,5,6), # <- the data fits better to the second learner
                             learners=list(
                               function(metric, transformation.x, transformation.y, metric.transformed, q) instance.1,
                               function(metric, transformation.x, transformation.y, metric.transformed, q) instance.2
                             )),
    instance.2);
})



test_that("Test regressoR.applyLearners with multiple result",{
  f.1       <- function(x) x
  quality.1 <- 12;
  size.1   <- 7L;
  instance.1 <- FittedModel.new(f=f.1, quality=quality.1, size=size.1);

  f.2       <- function(x) x + 1
  quality.2 <- 2;
  size.2   <- 9L;
  instance.2 <- FittedModel.new(f=f.2, quality=quality.2, size=size.2);

  f.3       <- function(x) x*x - 1
  quality.3 <- 5;
  size.3   <- 6L;
  instance.3 <- FittedModel.new(f=f.3, quality=quality.3, size=size.3);

  f.4       <- function(x) x + 1
  quality.4 <- 2;
  size.4   <- 8L; # same as f.2, but smaller
  instance.4 <- FittedModel.new(f=f.4, quality=quality.4, size=size.4);

  f.5       <- function(x) x + 1
  quality.5 <- 2;
  size.5   <- 18L; # same as f.2, but bigger
  instance.5 <- FittedModel.new(f=f.5, quality=quality.5, size=size.5);


  ls <- list(
    function(metric, transformation.x, transformation.y, metric.transformed, q) instance.1,
    function(metric, transformation.x, transformation.y, metric.transformed, q) instance.2,
    function(metric, transformation.x, transformation.y, metric.transformed, q) instance.3,
    function(metric, transformation.x, transformation.y, metric.transformed, q) instance.4,
    function(metric, transformation.x, transformation.y, metric.transformed, q) instance.5
  );

  expect_identical(
    regressoR.applyLearners(x=c(1,2,3,4,5), y=c(1,2,3,4,5), # <- the data fits to the first learner
                             learners=ls),
    instance.1);


  xt <- -(1:7);
  expect_identical(
    regressoR.applyLearners(x=xt, y=xt+1, # <- the data fits better to the second learner
                             learners=ls),
    instance.4);

  xt <- 1:20;
  expect_identical(
    regressoR.applyLearners(x=xt, y=xt*xt-1, # <- the data fits better to the second learner
                             learners=ls),
    instance.3);
})
