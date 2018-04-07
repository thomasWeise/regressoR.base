#' @include FittedModel.R

# The internal method to check a learner and to wrap it so that it can deal with
# the internal data structures.
.prepare.learner <- function(learner) {
  # check the learner
  learner <- force(learner);
  if(is.null(learner) || (!(is.function(learner)))) {
    stop("A learner must be a valid function.");
  }
  # make sure the learner is a proper function
  if(is.primitive(learner)) {
    learner.args <- formals(args(learner));
  } else {
    learner.args <- formals(learner);
  }
  if(!(identical(names(learner.args), c("metric",
                                        "transformation.x",
                                        "transformation.y",
                                        "metric.transformed",
                                        "q")))) {
    stop("Learner function must have exactly five arguments named 'metric', 'transformation.x', 'transformation.y', 'metric.transformed', and 'q'.");
  }

  learner.new <- function(data, q)
                       learner(metric = data$metric,
                               transformation.x = data$transformation.x,
                               transformation.y = data$transformation.y,
                               metric.transformed = data$metric.transformed,
                               q = q);
  learner.new <- force(learner.new);
  return(learner.new);
}

# create a selection tuple with the following elements:
# m the original, raw metric
# tx the x transformation
# ty the y transformation
# tm the transformed metric
#' @importFrom dataTransformeR TransformedData.select2D
.make.selection <- function(data, selection, metricGenerator, metric=NULL) {
  if(is.null(selection)) {
    sel <- data;
  } else {
    sel <- TransformedData.select2D(data=data, selection=selection);
  }
  sel <- force(sel);
  metric.transformed <- metricGenerator(sel@x@data, sel@y@data);
  metric.transformed <- force(metric.transformed);
  transformation.x <- sel@x@transformation;
  transformation.x <- force(transformation.x);
  transformation.y <- sel@y@transformation;
  transformation.y <- force(transformation.y);
  metric <- force(metric);
  if(is.null(metric)) {
    metric <- metric.transformed;
  }
  metric <- force(metric);
  tuple <- list(metric=metric,
                transformation.x=transformation.x,
                transformation.y=transformation.y,
                metric.transformed=metric.transformed);
  tuple <- force(tuple);
  return(tuple);
}

# Compute the test quality
#' @importClassesFrom regressoR.quality RegressionQualityMetric
.test.quality <- function(data, result) {
  if(methods::is(result, "FittedModel")) {
    return(data@quality(result@f));
  }
  return(+Inf);
}

#' @title Apply a Set of Regression Learners
#' @description Use the \code{learnerSelectoR} package to apply a set of
#' learners to a set of data representations and pick the approach which
#' generalizes best.
#'
#' The data is represented by two vectors, \code{x} and \code{y}.
#'
#' Each learner must be function with exactly four arguments named
#' \code{metric}, \code{transformation.x}, \code{transformation.y}, and
#' \code{metric.transformed} Its parameter \code{metric} will be an instance of
#' \code{\link{RegressionQualityMetric}} which guides the search on the actual,
#' raw data. However, since we internally use the
#' \code{\link{Transformation.applyDefault2D}} method from the
#' \code{dataTransformeR} package by default to generate different
#' representations of the raw data, each model fitting procedure may take place
#' in two steps, first on a transformed representation of the data
#' (\code{metric.transformed} based on \code{transformation.x} and
#' \code{transformation.y}) and then the actual finalization fitting the actual
#' \code{metric}. A learner returns an instance of \code{\link{FittedModel}}
#' which represents, well, the model it has fitted to its input data. Each
#' learner thus represents the process of adapting a specific model to some
#' data.
#'
#' The \code{metricGenerator} is a function which accepts two vectors \code{x}
#' and \code{y} and returns an instance of
#' \code{\link{RegressionQualityMetric}}. It will be used to generate the
#' quality metrics for guiding the model fitters. Since we internally use the
#' \code{\link{learning.learn}} method from the \code{learnerSelectoR} package,
#' the model may be chosen based on cross-validation and the metric generator is
#' then also used to generate quality metrics for the training and test datasets
#' used internally. If nothing else is specified, we use
#' \code{\link{RegressionQualityMetric.default}} to generate the quality
#' metrics.
#'
#' \code{representations} is a list of \code{\link{TransformedData2D}} instances
#' providing alernative views on the data, or \code{NULL} if only the raw data
#' should be concerned. By default, we use
#' \code{\link{Transformation.applyDefault2D}} to get a set of representations
#' if nothing else is specified.
#'
#' The return value of this method will be an instance of
#' \code{\link{FittedModel}} or \code{NULL} if no learner could produce any
#' result.
#'
#' @param x the vector of \code{x} coordinates
#' @param y the vector of \code{y} coordinates
#' @param learners the list of regression-based learner functions
#' @param representations the list of data representations, or \code{NULL} if
#'   fitting should take place only on the raw data
#' @param metricGenerator the metric generator function
#' @param q the effort to be spent on learning: 0 is minimal (potentially
#'   fast/poor quality), 1 is maximal (potentially slow/high quality)
#' @importFrom dataTransformeR Transformation.applyDefault2D Transformation.identity2D
#' @importFrom regressoR.quality RegressionQualityMetric.default
#' @importFrom learnerSelectoR learning.learn
#' @export regressoR.applyLearners
regressoR.applyLearners <- function(x, y,
                                    learners,
                                    representations=Transformation.applyDefault2D(x=x, y=y, addIdentity=TRUE),
                                    metricGenerator=RegressionQualityMetric.default,
                                    q=0.75) {

  # check the input data
  if(is.null(x) || is.null(y) ||
     (!(is.vector(x) && is.vector(y)))) {
    stop("x and y must be vectors.");
  }
  .data.size <- length(x);
  if((.data.size <= 0L) || (.data.size != length(y))) {
    stop("x and y must be vectors of the same, positive, non-zero length.");
  }

  # check the metric generator
  if(is.null(metricGenerator) || (!(is.function(metricGenerator)))) {
    stop("metricGenerator must be a function.");
  }
  # make sure the learner is a proper function
  if(is.primitive(metricGenerator)) {
    metricGenerator.args <- formals(args(metricGenerator));
  } else {
    metricGenerator.args <- formals(metricGenerator);
  }
  if(!(identical(names(metricGenerator.args), c("x", "y")))) {
    stop("metricGenerator must be a function with exactly two arguments, 'x' and 'y'.")
  }

  # Check all learners.
  learners <- force(learners);
  learners <- lapply(X = learners, FUN = .prepare.learner);
  learners <- force(learners);

  # Now prepare the .data.
  .data <- NULL;

  # check the representations
  if(!(is.null(representations))) {
    if(is.list(representations)) {
      # OK, it did return a list
      if(length(representations) <= 0L) {
        # but the list is empty
        representations <- NULL;
      } else {
        # the list is not empty, so we can
        for(representation in representations) {
          if(methods::is(representation, "TransformedData2D")) {
            if( (representation@x@transformation@complexity <= 0L) &&
                (representation@y@transformation@complexity <= 0L)) {
              # Only identity transformations have complexity 0L, so this is the raw .data record.
              .data <- representation;
              break;
            }
          } else {
            stop("All representations must be instances of TransformedData2D.");
          }
        }
      }
    } else {
      stop("'representations' must be a list.")
    }
  }

  if(is.null(.data)) {
    # We did not have a raw .data record.
    .data <- Transformation.identity2D(x, y);
  }

  if(is.null(representations)) {
    # No representations yet? OK, let's create one.
    representations <- list(.data);
  }

  .env <- new.env();
  assign(x="i", value=NULL, pos=.env);
  assign(x="r", value=NULL, pos=.env);
  assign(x="j", value=NULL, pos=.env);
  assign(x="s", value=NULL, pos=.env);

  # Now we have data and representations, we are ready to do the learning.
  .selector <- function(data, selection, index) {

    # if we just start the iteration, first compute the true metric
    if(!(identical(get(x="i", pos=.env, inherits=FALSE), index))) {
      # At the beginning of each iteration, first generate the 'raw' metric
      assign(x="i", value=index, pos=.env);
      .r <- .make.selection(.data, selection, metricGenerator=metricGenerator, metric=NULL);
      .r <- force(.r);
      assign(x="r", value=.r, pos=.env);
    }

    # Now extract the original metric
    m <- get(x="r", pos=.env, inherits=FALSE);
    m <- force(m);
    if(identical(data, .data)) {
      # if the data is the same as the original data, use the raw metric
      return(m);
    }
    # otherwise, create the selection
    .r <- .make.selection(data, selection, metricGenerator=metricGenerator, metric=m$metric);
    force(.r);
    return(.r);
  }

  # Now we have data and representations, we are ready to do the learning.
  .test.selector <- function(data, selection, index) {
    if(!(identical(get(x="j", pos=.env, inherits=FALSE), index))) {
      # At the beginning of each iteration, first generate the 'raw' metric
      assign(x="j", value=index, pos=.env);
      if(is.null(selection)) {
        .s <- .data;
      } else {
        .s <- TransformedData.select2D(data=.data, selection=selection);
      }
      .s <- force(.s);
      .s <- metricGenerator(.s@x@data, .s@y@data);
      .s <- force(.s);
      assign(x="s", value=.s, pos=.env);
    } else {
      .s <- get(x="s", pos=.env, inherits=FALSE);
    }
    .s <- force(.s);
    return(.s);
  };

  return(learnerSelectoR::learning.learn(data = .data,
                                         data.size = .data.size,
                                         learners = learners,
                                         test.quality = .test.quality,
                                         selector = .selector,
                                         representations = representations,
                                         test.selector = .test.selector,
                                         q = q));
}
