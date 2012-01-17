\name{predict.kda}
\alias{predict.kda}
\title{Classify Multivariate Observations Based on Kernel Rules}
\usage{\method{predict}{kda}(object, newdata, ...)
}
\description{Classify multivariate observations in conjunction with \code{\link{kda}}.}
\details{This function is a method for the generic function \code{predict()} for class 
\code{"kda"}. 
It can be invoked by calling \code{predict(x)} for an object \code{x} of the 
appropriate class, or directly by calling \code{predict.kda(x)} regardless of 
the class of the object.}
\value{A \code{list} with components:
\item{class}{The predicted class labels (a \code{factor}).}
\item{posteriors}{Matrix of class posterior probabilities.}}
\references{Hastie, T., Tibshirani, R. and Friedman, J. (2009), The Elements of Statistical Learning: 
Data Mining, Inference and Prediction. Second edition, Springer, New York.}
\seealso{\code{\link{kda}}.}
\keyword{classif}
\arguments{\item{object}{Object of class \code{"kda"}.}
\item{newdata}{A \code{data.frame} of cases to be classified or, if \code{object} has a
\code{formula}, a \code{data.frame} with columns of the same names as the
variables used. A vector will be interpreted as a row
vector. If \code{newdata} is missing, an attempt will be made to
retrieve the data used to fit the \code{kda} object.}
\item{\dots}{Further arguments.}
}

