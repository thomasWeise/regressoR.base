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
FittedModel <- methods::setClass(
  Class = "FittedModel",
  contains = "learning.Result",
  representation = methods::representation(f="function"),
  validity = function(object) {
    if(base::is.null(object@f) || (!(base::is.function(object@f)))) {
      return("The model must be a proper non-null function.");
    }
    if(base::is.primitive(object@f)) {
      f.args <- base::formals(base::args(object@f));
    } else {
      f.args <- base::formals(object@f);
    }
    if ((base::length(f.args) != 1L) || (!(base::identical(base::names(f.args), base::c("x"))))) {
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
  result <- methods::new("FittedModel", f=f, quality=quality, size=size);
  result <- base::force(result);
  result@f <- base::force(result@f);
  result@quality <- base::force(result@quality);
  result@size <- base::force(result@size);
  result <- base::force(result);
  methods::validObject(result);
  return(result);
}

