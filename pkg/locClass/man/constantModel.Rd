\docType{data}
\name{constantModel}
\alias{constantModel}
\alias{estfun.constant}
\alias{predict.constantModel}
\alias{reweight.constantModel}
\title{Combine Model-based Recursive Partitioning with a Constant Classifier}
\format{Formal class 'StatModel' [package "modeltools"] with 5 slots}
\usage{
  constantModel

  \method{reweight}{constantModel} (object, weights, ...)

  \method{estfun}{constant} (x, ...)

  \method{predict}{constantModel} (object,
    out = c("class", "posterior"), ...)
}
\arguments{
  \item{object}{An object of class "constantModel" and
  "constant", respectively.}

  \item{x}{An object of class "constant".}

  \item{weights}{A vector of observation weights.}

  \item{out}{Should class labels or posterior probabilities
  be returned?}

  \item{\dots}{Further arguments.}
}
\value{
  \code{reweight}: The re-weighted fitted "constant Model"
  object. \cr \code{deviance}: The value of the deviance
  for the Constant Classifier extracted from \code{object},
  i.e. the log-likelihood. \cr \code{estfun}: The empirical
  estimating (or score) function for the Constant
  Classifier, i.e. the derivatives of the log-likelihood
  with respect to the parameters, evaluated at the training
  data. \cr \code{predict}: Either a vector of predicted
  class labels or a matrix of class posterior
  probabilities.
}
\description{
  Combine Model-Based Recursive Partitioning with a
  Constant Classifier.
}
\details{
  This page lists all ingredients to combine a Constant
  Classifier with Model-Based Recursive Partitioning
  (\code{\link[party]{mob}} from package \pkg{party}). See
  the example for how to do that.

  \code{constantModel} is an object of class
  \code{\link[modeltools]{StatModel-class}} implemented in
  package \pkg{modeltools} that provides an infra-structure
  for an unfitted \code{\link{constant}} model.

  Moreover, methods for \code{\link{constant}} and
  \code{constantModel} objects for the generic functions
  \code{\link[party]{reweight}},
  \code{\link[stats]{deviance}},
  \code{\link[sandwich]{estfun}}, and
  \code{\link[stats]{predict}} are provided.
}
\examples{
library(locClassData)
library(party)

data <- vData(500)
x <- seq(0,1,0.05)
grid <- expand.grid(x.1 = x, x.2 = x)

fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = constantModel,
control = mob_control(objfun = deviance, minsplit = 20))

## predict posterior probabilities
pred <- predict(fit, newdata = grid, out = "posterior")
post <- matrix(0, length(pred), 2)
colnames(post) = 1:2
for (i in seq_along(pred))
    post[i, colnames(pred[[i]])] = pred[[i]]

image(x, x, matrix(as.numeric(post[,1]), length(x)), xlab = "x.1", ylab = "x.2")
contour(x, x, matrix(as.numeric(post[,1]), length(x)), levels = 0.5, add = TRUE)
points(data$x, pch = as.character(data$y))

## predict node membership
splits <- predict(fit, newdata = grid, type = "node")
contour(x, x, matrix(splits, length(x)), levels = min(splits):max(splits), add = TRUE, lty = 2)
}
\references{
  Zeileis, A., Hothorn, T. and Kornik, K. (2008),
  Model-based recursive partitioning. \emph{Journal of
  Computational and Graphical Statistics}, \bold{17(2)}
  492--514.
}
\seealso{
  \code{\link[party]{reweight}},
  \code{\link[stats]{deviance}},
  \code{\link[sandwich]{estfun}},
  \code{\link[stats]{predict}}.
}
\keyword{datasets}

