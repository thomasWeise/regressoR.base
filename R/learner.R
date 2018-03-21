#' @include FittedModel.R

# The internal method to check a learner and to wrap it so that it can deal with
# the internal data structures.
.prepare.learner <- function(learner) {
  # check the learner
  learner <- base::force(learner);
  if(base::is.null(learner) ||
     (!(base::is.function(learner)))) {
    base::stop("A learner must be a valid function.");
  }
  # make sure the learner is a proper function
  if(base::is.primitive(learner)) {
    learner.args <- base::formals(base::args(learner));
  } else {
    learner.args <- base::formals(learner);
  }
  if((base::length(learner.args) != 4L) ||
     (!(base::identical(base::names(learner.args), base::c("metric",
                                                           "transformation.x",
                                                           "transformation.y",
                                                           "metric.transformed"))))) {
    base::stop("Learner function must have at exactly four arguments named 'metric', 'transformation.x', 'transformation.y', and 'metric.transformed'.");
  }

  learner.new <- function(data) learner(metric = data$metric,
                                        transformation.x = data$transformation.x,
                                        transformation.y = data$transformation.y,
                                        metric.transformed = data$metric.transformed);
  learner.new <- base::force(learner.new);
  return(learner.new);
}

# create a selection tuple with the following elements:
# m the original, raw metric
# tx the x transformation
# ty the y transformation
# tm the transformed metric
.make.selection <- function(data, selection, metricGenerator, metric=NULL) {
  if(base::is.null(selection)) {
    sel <- data;
  } else {
    sel <- dataTransformeR::TransformedData.select2D(data=data, selection=selection);
  }
  sel <- base::force(sel);
  metric.transformed <- metricGenerator(sel@x@data, sel@y@data);
  metric.transformed <- base::force(metric.transformed);
  transformation.x <- sel@x@transformation;
  transformation.x <- base::force(transformation.x);
  transformation.y <- sel@y@transformation;
  transformation.y <- base::force(transformation.y);
  metric <- base::force(metric);
  if(base::is.null(metric)) {
    metric <- metric.transformed;
  }
  metric <- base::force(metric);
  tuple <- base::list(metric=metric,
                      transformation.x=transformation.x,
                      transformation.y=transformation.y,
                      metric.transformed=metric.transformed);
  tuple <- base::force(tuple);
  return(tuple);
}

# Compute the test quality
#' @importClassesFrom regressoR.quality RegressionQualityMetric
.test.quality <- function(data, result) {
  if(methods::is(result, "FittedModel") &&
     methods::is(data, "RegressionQualityMetric")) {
    return(data@quality(result@f));
  }
  return(+Inf);
}

#' @title Apply a Set if Learners
#' @description Use the learnerSelectoR package to apply a set of learners to a
#' set of data representations and pick the approach which generalizes best.
#' @param x the vector of \code{x} coordinates
#' @param y the vector of \code{y} coordinates
#' @param learners the list of regression-based learner functions
#' @param representations the list of data representations, or \code{NULL} if
#'   fitting should take place only on the raw data
#' @param metric the metric generator function
#' @importFrom dataTransformeR Transformation.applyDefault2D
#' @importFrom regressoR.quality RegressionQualityMetric.default
#' @export regression.applyLearners
regression.applyLearners <- function(x, y,
                                     learners,
                                     representations=dataTransformeR::Transformation.applyDefault2D(x, y),
                                     metric=regressoR.quality::RegressionQualityMetric.default) {

  if(base::is.null(x) || base::is.null(y) ||
     (!(base::is.vector(x) && base::is.vector(y)))) {
    base::stop("x and y must be vectors.");
  }
  .data.size <- base::length(x);
  if((.data.size<= 0L) || (.data.size != base::length(y))) {
    base::stop("x and y must be vectors of same, non-zero length.");
  }

  # Check all learners.
  learners <- base::force(learners);
  learners <- base::lapply(X = learners, FUN = .prepare.learner);
  learners <- base::force(learners);

  # Now prepare the .data.
  .data <- NULL;

  # check the representations
  if(!(base::is.null(representations))) {
    if(base::is.list(representations)) {
      # OK, it did return a list
      if(base::length(representations) <= 0L) {
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
            base::stop("All representations must be instances of TransformedData2D.");
          }
        }
      }
    } else {
      base::stop("'representations' must be a list.")
    }
  }

  if(base::is.null(.data)) {
    # We did not have a raw .data record.
    .data <- dataTransformeR::Transformation.identity2D(x, y);
  }

  if(base::is.null(representations)) {
    # No representations yet? OK, let's create one.
    representations <- base::list(.data);
  }

  .env <- base::new.env();
  base::assign(x="i", value=NULL, pos=.env);
  base::assign(x="r", value=NULL, pos=.env);
  base::assign(x="j", value=NULL, pos=.env);
  base::assign(x="s", value=NULL, pos=.env);

  # Now we have data and representations, we are ready to do the learning.
  .selector <- function(data, selection, index) {

    # if we just start the iteration, first compute the true metric
    if(!(base::identical(base::get(x="i", pos=.env, inherits=FALSE), index))) {
      # At the beginning of each iteration, first generate the 'raw' metric
      base::assign(x="i", value=index, pos=.env);
      .r <- .make.selection(.data, selection, metricGenerator=metric, metric=NULL);
      .r <- base::force(.r);
      base::assign(x="r", value=.r, pos=.env);
    }

    # Now extract the original metric
    m <- base::get(x="r", pos=.env, inherits=FALSE);
    m <- base::force(m);
    if(base::identical(data, .data)) {
      # if the data is the same as the original data, use the raw metric
      return(m);
    }
    # otherwise, create the selection
    .r <- .make.selection(data, selection, metricGenerator=metric, metric=m$metric);
    base::force(.r);
    return(.r);
  }

  # Now we have data and representations, we are ready to do the learning.
  .test.selector <- function(data, selection, index) {
    if(!(base::identical(base::get(x="j", pos=.env, inherits=FALSE), index))) {
      # At the beginning of each iteration, first generate the 'raw' metric
      base::assign(x="j", value=index, pos=.env);
      if(base::is.null(selection)) {
        .s <- .data;
      } else {
        .s <- dataTransformeR::TransformedData.select2D(data=.data, selection=selection);
      }
      .s <- base::force(.s);
      .s <- metric(.s@x@data, .s@y@data);
      .s <- base::force(.s);
      base::assign(x="s", value=.s, pos=.env);
    } else {
      .s <- base::get(x="s", pos=.env, inherits=FALSE);
    }
    .s <- base::force(.s);
    return(.s);
  };

  return(learnerSelectoR::learning.learn(data = .data,
                                         data.size = .data.size,
                                         learners = learners,
                                         test.quality = .test.quality,
                                         selector = .selector,
                                         representations = representations,
                                         test.selector = .test.selector));
}
