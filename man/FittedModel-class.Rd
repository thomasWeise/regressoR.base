% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FittedModel.R
\docType{class}
\name{FittedModel-class}
\alias{FittedModel-class}
\alias{FittedModel}
\title{A Fitted Model}
\description{
This class holds a fully parameterized, fitted model. The model
  function itself can encapsulate any type of behavior or nature. It could be
  polynomial, an ANN, whatever you want.
}
\section{Slots}{

\describe{
\item{\code{f}}{a function accepting one parameter and returning a value}

\item{\code{quality}}{the quality of the model on the original data, computed by a
quality metric, smaller values are better}
}}

