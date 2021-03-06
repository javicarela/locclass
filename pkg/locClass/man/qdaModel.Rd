\docType{data}
\name{qdaModel}
\alias{deviance.wqda}
\alias{estfun.wqda}
\alias{predict.qdaModel}
\alias{qdaModel}
\alias{reweight.qdaModel}
\title{Combine Model-based Recursive Partitioning with Quadratic Discriminant Analysis}
\format{Formal class 'StatModel' [package "modeltools"] with 5 slots}
\usage{
  qdaModel

  \method{reweight}{qdaModel} (object, weights, ...)

  \method{deviance}{wqda} (object, ...)

  \method{estfun}{wqda} (x, ...)

  \method{predict}{qdaModel} (object,
    out = c("class", "posterior"), ...)
}
\arguments{
  \item{object}{An object of class "qdaModel" and "wqda",
  respectively.}

  \item{x}{An object of class "wqda".}

  \item{weights}{A vector of observation weights.}

  \item{out}{Should class labels or posterior probabilities
  be returned?}

  \item{\dots}{Further arguments.}
}
\value{
  \code{reweight}: The re-weighted fitted "qdaModel"
  object. \cr \code{deviance}: The value of the deviance
  for Quadratic Discriminant Analysis extracted from
  \code{object}, i.e. the log-likelihood. \cr
  \code{estfun}: The empirical estimating (or score)
  function for Quadratic Discriminant Analysis, i.e. the
  derivatives of the log-likelihood with respect to the
  parameters, evaluated at the training data. \cr
  \code{predict}: Either a vector of predicted class labels
  or a matrix of class posterior probabilities.
}
\description{
  Combine Model-based Recursive Partitioning with Quadratic
  Discriminant Analysis.
}
\details{
  This page lists all ingredients to combine Quadratic
  Discriminant Analysis with Model-Based Recursive
  Partitioning (\code{\link[party]{mob}} from package
  \pkg{party}). See the example for how to do that.

  \code{qdaModel} is an object of class
  \code{\link[modeltools]{StatModel-class}} implemented in
  package \pkg{modeltools} that provides an infra-structure
  for an unfitted \code{\link{wqda}} model.

  Moreover, methods for \code{\link{wqda}} and
  \code{qdaModel} objects for the generic functions
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

fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = qdaModel,
control = mob_control(objfun = deviance, minsplit = 200))

## predict posterior probabilities
pred <- predict(fit, newdata = grid, out = "posterior")
post <- do.call("rbind", pred)

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

