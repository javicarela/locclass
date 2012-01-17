\name{predict.osmultinom}
\alias{predict.osmultinom}
\title{Predict New Examples Based on Observation Specific Multinomial Log-linear Models}
\usage{\method{predict}{osmultinom}(object, newdata, type=c("class", "probs"), ...)
}
\description{Predict new examples in conjunction with \code{\link{osmultinom}}.}
\details{This function is a method for the generic function \code{predict()} for class 
\code{"osmultinom"}. 
It can be invoked by calling \code{predict(x)} for an object \code{x} of the 
appropriate class, or directly by calling \code{predict.osmultinom(x)} regardless of 
the class of the object.}
\value{If \code{type = "class"}, ;
if \code{type = "probs"}, .}
\seealso{\code{\link{osmultinom}}, \code{\link{osnnet}}, \code{\link{predict.osnnet}}, \code{\link[nnet]{predict.nnet}}, \code{\link[nnet]{nnet}}.}
\keyword{neural}
\arguments{\item{object}{Object of class \code{"osmultinom"}.}
\item{newdata}{A \code{data.frame} of cases to be classified or, if \code{object} has a
\code{formula}, a \code{data.frame} with columns of the same names as the
variables used. A vector will be interpreted as a row
vector. If \code{newdata} is missing, an attempt will be made to
retrieve the data used in \code{\link{osmultinom}}.}
\item{type}{Type of output.}
\item{\dots}{Arguments passed to or from other methods.}
}

