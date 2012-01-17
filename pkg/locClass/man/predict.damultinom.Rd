\name{predict.damultinom}
\alias{predict.damultinom}
\title{Predict New Examples by a Trained Discriminant Adaptive Multinomial Model}
\usage{\method{predict}{damultinom}(object, newdata, ...)
}
\description{Predict new examples by a trained discriminant adaptive multinomial model.}
\details{This function is a method for the generic function \code{predict()} for class 
\code{"damultinom"}. 
It can be invoked by calling \code{predict(x)} for an object \code{x} of the 
appropriate class, or directly by calling \code{predict.damultinom(x)} regardless of 
the class of the object. 

In contrast to \code{\link[nnet]{predict.multinom}} \code{predict.damultinom} does not have
a type argument. Both, the predicted posterior probabilities and the class labels, are returned.}
\value{A \code{list} with components:
\item{class}{The predicted class labels (a \code{factor}).}
\item{posterior}{Matrix of class posterior probabilities.}}
\references{Hand, D. J., Vinciotti, V. (2003), Local versus global models for classification problems: 
Fitting models where it matters, \emph{The American Statistician}, \bold{57(2)} 124--130.

Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S}. Fourth edition. Springer.}
\seealso{\code{\link{damultinom}}, \code{\link{dannet}}, \code{\link[nnet]{multinom}}, \code{\link[nnet]{nnet}}.}
\keyword{classif}
\keyword{neural}
\keyword{model}
\arguments{\item{object}{Object of class \code{"damultinom"}.}
\item{newdata}{A \code{matrix} or \code{data.frame} of test examples. A vector is considered to be
a row vector comprising a single case.}
\item{\dots}{Arguments passed to or from other methods.}
}

