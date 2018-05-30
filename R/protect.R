
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
  fix <- function(x) {
    y   <- vector(mode="double", length=length(x));

    # fix lower bound
    sel <- (x <= x.min);
    remaining <- !sel;
    if(any(sel)) {
      end <- find.finite(min(x[sel]), x.min, f);
      sel2 <- sel & (x <= end[1L]);
      y[sel2] <- end[2L];
      sel <- sel & (!sel2);
      if(any(sel)) {
        y[sel] <- vapply(X=x[sel], FUN=function(x) find.finite(x, x.min, f)[2L], FUN.VALUE=NaN);
        sel[sel] <- sel[sel] & (!(is.finite(y[sel])));
        y[sel] <- x.min.y;
      }
    }

    # fix upperbound
    sel <- remaining & (x >= x.max);
    remaining <- (remaining & (!sel));
    if(any(sel)) {
      end <- find.finite(max(x[sel]), x.max, f);
      sel2 <- sel & (x >= end[1L]);
      y[sel2] <- end[2L];
      sel <- sel & (!sel2);
      if(any(sel)) {
        y[sel] <- vapply(X=x[sel], FUN=function(x) find.finite(x, x.max, f)[2L], FUN.VALUE=NaN);
        sel[sel] <- sel[sel] & (!(is.finite(y[sel])));
        y[sel] <- x.max.y;
      }
    }

    # linearly extrapolate everything in beteween
    y[remaining] <- (x.min.y + ((x.max.y - x.min.y) * (x[remaining] - x.min) / (x.max - x.min)));
    return(y);
  }
  fix <- force(fix);
  x <- NULL; y <- NULL;

  # finally, we vectorize the function properly
  final <- function(x) {
    y <- f(x);
    finite <- is.finite(y);
    if(all(finite)) { return(y); }
    finite <- !finite;
    y[finite] <- fix(x[finite]);
    return(y);
  }

  final <- force(final);
  return(final);
}
