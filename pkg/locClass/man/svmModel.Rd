\docType{data}
\name{svmModel}
\alias{deviance.wsvm}
\alias{estfun.wsvm}
\alias{predict.svmModel}
\alias{reweight.svmModel}
\alias{svmModel}
\title{Combine Model-based Recursive Partitioning with Support Vector Machines}
\format{Formal class 'StatModel' [package "modeltools"] with 5 slots}
\usage{
  svmModel

  \method{reweight}{svmModel} (object, weights, ...)

  \method{deviance}{wsvm} (object, ...)

  \method{estfun}{wsvm} (x, ...)

  \method{predict}{svmModel} (object,
    out = c("class", "posterior", "decision"), ...)
}
\arguments{
  \item{object}{An object of class "svmModel" and "wsvm",
  respectively.}

  \item{x}{An object of class "wsvm".}

  \item{weights}{A vector of observation weights.}

  \item{out}{Should class labels or posterior probabilities
  be returned?}

  \item{\dots}{Further arguments.}
}
\value{
  \code{reweight}: The re-weighted fitted "svmModel"
  object. \cr \code{deviance}: The value of the objective
  function extracted from \code{object}. \cr \code{estfun}:
  The empirical estimating (or score) function, i.e. the
  derivatives of the objective function with respect to the
  parameters, evaluated at the training data. \cr
  \code{predict}: Either a vector of predicted class
  labels, a matrix of decision values or a matrix of class
  posterior probabilities.
}
\description{
  Combine Model-based Recursive Partitioning with Support
  Vector Machines.
}
\details{
  This page lists all ingredients to combine Support Vector
  Machines with Model-Based Recursive Partitioning
  (\code{\link[party]{mob}} from package \pkg{party}). See
  the example for how to do that.

  \code{svmModel} is an object of class
  \code{\link[modeltools]{StatModel-class}} implemented in
  package \pkg{modeltools} that provides an infra-structure
  for an unfitted \code{\link{wsvm}} model.

  Moreover, methods for \code{\link{wsvm}} and
  \code{svmModel} objects for the generic functions
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

fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = svmModel, kernel = "linear",
control = mob_control(objfun = deviance, minsplit = 200))

## predict decision values
dec <- predict(fit, newdata = grid, out = "decision")

image(x, x, matrix(dec, length(x)), xlab = "x.1", ylab = "x.2")
contour(x, x, matrix(dec, length(x)), levels = 0, add = TRUE)
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

