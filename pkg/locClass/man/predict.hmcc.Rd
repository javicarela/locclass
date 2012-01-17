\name{predict.hmcc}
\alias{predict.hmcc}
\title{Classify Multivariate Observations Based on the Hierarchical Mixture Model or the Common Components Model}
\usage{\method{predict}{hmcc}(object, newdata, ...)
}
\description{Classify multivariate observations in conjunction with the hierarchical mixture model or the common components model.}
\details{This function is a method for the generic function \code{predict()} for class \code{"hmcc"}. 
It can be invoked by calling \code{predict(x)} for an object \code{x} of the appropriate class, or directly by calling 
\code{predict.hmcc(x)} regardless of the class of the object.

?Missing values in \code{newdata} are handled by returning \code{NA} if the linear discriminants cannot be evaluated. 
?If \code{newdata} is omitted and the \code{na.action} of the fit omitted cases, these will be omitted on the prediction.}
\value{A \code{list} with components:
\item{class}{The predicted class labels (a \code{factor}).}
\item{posterior}{Matrix of class posterior probabilities.}}
\references{Titsias, M. K., Likas, A. C. (2001), Shared kernel models for class conditional density estimation.
\emph{IEEE Transactions on Neural Networks}, \bold{12(5)}, 987--997.

Titsias, M. K., Likas, A. C. (2002), Mixture of experts classification using a hierarchical mixture model.
\emph{Neural Computation}, \bold{14}, 2221--2244.}
\seealso{\code{\link{hmcc}}, \code{\link{hmccStart}}.}
\keyword{classif}
\keyword{cluster}
\keyword{multivariate}
\arguments{\item{object}{An object of class \code{"hmcc"}.}
\item{newdata}{A \code{data.frame} of cases to be classified or, if \code{object} has a \code{formula}, a \code{data.frame} with columns 
of the same names as the variables used. A vector will be interpreted as a row vector. If \code{newdata} is missing, 
an attempt will be made to retrieve the data used to fit the \code{hmcc} object.}
\item{\dots}{Further arguments.}
}
\examples{library(mlbench)
data.train <- as.data.frame(mlbench.waveform(300))
data.test <- as.data.frame(mlbench.waveform(200))

## "cc"
fit <- hmcc(classes ~ ., data = data.train, J = 5, method = "cc")
pred <- predict(fit, data.test)
mean(pred$class != data.test$classes)

## "hm1"
fit <- hmcc(classes ~ ., data = data.train, J = 3, method = "hm1")
pred <- predict(fit, data.test)
mean(pred$class != data.test$classes)

## "hm2"
fit <- hmcc(classes ~ ., data = data.train, J = 3, method = "hm2")
pred <- predict(fit, data.test)
mean(pred$class != data.test$classes)}

