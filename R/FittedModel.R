#' @title A Fitted Model
#' @description This class holds a fully parameterized, fitted model. The model
#'   function itself can encapsulate any type of behavior or nature. It could be
#'   polynomial, an ANN, whatever you want.
#' @slot f a function accepting one parameter and returning a value
#' @slot quality the quality of the model on the original data, computed by a
#'   quality metric, smaller values are better
#' @exportClass FittedModel
#' @importFrom methods setClass representation
#' @importClassesFrom learnerSelectoR learning.Result
FittedModel <- setClass(
  Class = "FittedModel",
  contains = "learning.Result",
  representation = representation(f="function"),
  validity = function(object) {
    if(is.null(object@f) || (!(is.function(object@f)))) {
      return("The model must be a proper non-null function.");
    }
    if(is.primitive(object@f)) {
      f.args <- formals(args(object@f));
    } else {
      f.args <- formals(object@f);
    }
    if ((length(f.args) != 1L) || (!(identical(names(f.args), c("x"))))) {
      return("Model function must take exactly one arguments named 'x'.");
    }
    return(TRUE);
  }
)


#' @title Create a New Instance of \code{\link{FittedModel}}.
#' @description Create a New Instance of \code{\link{FittedModel}}.
#' @param f a function accepting one parameter and returning a value
#' @param quality the quality of the model on the original data, computed by a
#'   quality metric, smaller values are better
#' @param size the size of the model, i.e., the number of parameters
#' @return the new instance
#' @importFrom methods new validObject
#' @export FittedModel.new
FittedModel.new <- function(f, quality, size) {
  result <- new("FittedModel", f=f, quality=quality, size=size);
  result <- force(result);
  result@f <- force(result@f);
  result@quality <- force(result@quality);
  result@size <- force(result@size);
  result <- force(result);
  validObject(result);
  return(result);
}

