
#' @title Protected a Fitted Model from Producing Non-Finite Results
#'
#' @description This function can protect a model which was trained on a certain
#'   metric against non-finite results. The idea is that we use the model
#'   directly for any situation where the results are finite. When they are not
#'   finite, we try to extrapolate the model values linearly between the two
#'   closest finite results. If we fail in doing so, we simply extrapolate
#'   results from the \code{x-y} data directly.
#'
#'   Using protected models may be useful if a model function \code{f} includes,
#'   e.g., a log-scaled \code{x}-axis and is fed with an x-coordinate where the
#'   log would receive a zero or negative parameter.
#'
#'   \emph{Notice:} This function and using the produced model may lead to many
#'   warnings.
#'
#' @param f the trained model
#' @param x the \code{x} coordinates on which the model was trained
#' @param y the \code{y} coordinates on which the model was trained
#' @return a new model function which can be as drop-in replacement for \code{f}
#'   and will produce the same results as \code{f} for all inputs where \code{f}
#'   has finite results \emph{and} finite output results where \code{f} does
#'   yield non-finite results.
#' @export model.protect
#' @importFrom utilizeR find.finite
model.protect <- function(f, x, y) {

  # first, we want to get the minimum and maximum x coordinates and their
  # corresponding y coordinates.
  x.min <- +Inf;
  x.max <- -Inf;
  x.min.y <- 0;
  x.max.y <- 0;
  for(i in 1L:length(x)) {
    xx <- x[i];
    if(xx < x.min) {
      x.min <- xx;
      x.min.y <- y[i];
    }
    if(xx > x.max) {
      x.max <- xx;
      x.max.y <- y[i];
    }
  }

  # ok, got the starting and end point. actually, every x value between x.min
  # and x.max should be OK - but those smaller than x.min or x.max may lead to
  # headache.
  x.min <- force(x.min);
  x.min.y <- force(x.min.y);
  x.max <- force(x.max);
  x.max.y <- force(x.max.y);
  f <- force(f);

  # create a new, wrapped/protected model
  f.ret <- function(x) {
    # first compute the original model
    res <- f(x);

    # if the original result is finite, we use it directly
    if(is.finite(res)) { return(res); }

    if(x <= x.min) {
      # we are before the start.
      # let us see how far we can go there coming from x.min
      res <- find.finite(x, x.min, f)[2L];
      if(is.finite(res)) {
        # ok, we could make some progress, so we use the approximation
        return(res);
      }

      # if we are before the start and cannot extrapolate at all, we use the
      # initial value as best guess
      return(x.min.y);
    }

    if(x >= x.max) {
      # we are after the end
      # let us see how far we can go there coming from x.max
      res <- find.finite(x, x.max, f)[2L];
      if(is.finite(res)) {
        # ok, we could make some progress, so we use the approximation
        return(res);
      }

      # if we are after the end and cannot extrapolate at all, we use the last
      # recorded value as best guess
      return(x.max.y);
    }

    # we are somewhere between x.min and x.max
    # so we try to approach the dangerous value from both ends

    # first from the left end
    res.1 <- find.finite(x, x.min, f);
    if(is.finite(res.1[2L])){
      # if we could make some progress on the left side, we use the
      # extrapolation
      x.1 <- res.1[1L];
      y.1 <- res.1[2L];
    } else {
      # if not, we use the first recorded valid value
      x.1 <- x.min;
      y.1 <- x.min.y;
    }

    # now we do the same coming from the right
    res.2 <- find.finite(x, x.max, f);
    if(is.finite(res.2[2L])){
      # ok, we could make some approximation and can use it
      x.2 <- res.2[1L];
      y.2 <- res.2[2L];
    } else {
      # otherwise, use last recorded value
      x.2 <- x.max;
      y.2 <- x.max.y;
    }

    # finally, we try to linearly extrapolate the result
    res <- (y.1 + ((y.2 - y.1) * (x - x.1) / (x.2 - x.1)));
    if(is.finite(res)) { return(res); }

    # otherwise, we are somewhere between x.min and x.max and things are totally
    # dodgy.
    # in this case, we just linearly extrapolate to get a finite result
    return(x.min.y + ((x.max.y - x.min.y) * (x - x.min) / (x.max - x.min)));
  }
  f.ret <- force(f.ret);

  # finally, we vectorize the function properly
  final <- function(x) {
    if(length(x) == 1L) f.ret(x)
    else vapply(X=x, FUN=f.ret, FUN.VALUE = NaN);
  }

  final <- force(final);
  return(final);
}
