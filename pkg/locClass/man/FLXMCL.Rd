\docType{class}
\name{FLXMCL}
\alias{FLXMCL}
\alias{FLXMCL-class}
\alias{FLXMCLconstant}
\alias{FLXMCLconstant-class}
\alias{FLXMCLlda}
\alias{FLXMCLlda-class}
\alias{FLXMCLmultinom}
\alias{FLXMCLmultinom-class}
\alias{FLXMCLnnet}
\alias{FLXMCLnnet-class}
\alias{FLXMCLqda}
\alias{FLXMCLqda-class}
\alias{FLXMCLsvm}
\alias{FLXMCLsvm-class}
\title{Class "FLXMCL"}
\description{
  Model specification for mixtures of classifiers.
}
\details{
  Objects can be created by calls of the form
  \code{new("FLXMCL", ...)}, typically inside driver
  functions like \code{\link{FLXMCLlda}} or
  \code{\link{FLXMCLmultinom}}. Class FLXMCL extends class
  \code{\link[flexmix]{FLXM-class}} from package
  \pkg{flexmix} directly.
}
\section{Slots}{
  \describe{ \item{\code{fit}:}{Function returning an
  \code{FLXcomponent} object.}
  \item{\code{defineComponent}:}{Expression to determine
  the \code{FLXcomponent} object given the parameters.}
  \item{\code{weighted}:}{Logical indicating whether
  \code{fit} can do weighted likelihood maximization.}
  \item{\code{name}:}{Character string used in print
  methods.} \item{\code{formula}:}{Formula describing the
  model.} \item{\code{fullformula}:}{Resulting formula from
  updating the model formula with the formula specified in
  the call to \code{flexmix}.} \item{\code{x}:}{Model
  matrix.} \item{\code{y}:}{Model response.}
  \item{\code{terms}, \code{xlevels},
  \code{contrasts}:}{Additional information for model
  matrix.} \item{\code{preproc.x}:}{Function for
  preprocessing matrix \code{x} before the EM algorithm
  starts, by default the identity function.}
  \item{\code{preproc.y}:}{Function for preprocessing
  matrix \code{y} before the EM algorithm starts, by
  default the identity function.} }
}
\seealso{
  \code{\link[flexmix]{FLXM-class}}.
}
\keyword{classes}

