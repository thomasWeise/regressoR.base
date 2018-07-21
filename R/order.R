#' @title Order Vectors of X and Y Coordinates
#' @description Order a vector \code{x} and \code{y} first based on \code{x} and
#'   then break ties based on the associated \code{y} values.
#' @param x the first vector
#' @param y the second vector
#' @param x.inc should the first vector be sorted in increasing order?
#' @param y.inc should the second vector be sorted in increasing order?
#' @return a vector with the order
#' @export xy.order
xy.order <- function(x, y, x.inc=TRUE, y.inc=FALSE) {
 rx <- rank(x, ties.method="min");
 if(!x.inc) { rx <- (-rx); }
 ry <- rank(y, ties.method="min");
 if(y.inc) { ry <- (-ry); }
 return(order(rx + (0.999 / ry)));
}
