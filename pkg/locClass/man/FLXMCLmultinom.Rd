\docType{methods}
\name{FLXMCLmultinom}
\alias{FLXgetModelmatrix}
\alias{FLXgetModelmatrix,FLXMCLmultinom-method}
\alias{FLXMCLmultinom}
\title{Mixtures of Multinomial Regression Models}
\usage{
  FLXMCLmultinom(formula = . ~ ., ...)
}
\arguments{
  \item{formula}{A formula which is interpreted relative to
  the formula specified in the call to
  \code{\link[flexmix]{flexmix}} using
  \code{\link[stats]{update.formula}}.  Only the left-hand
  side (response) of the formula is used. Default is to use
  the original \code{\link[flexmix]{flexmix}} model
  formula.}

  \item{\dots}{Further arguments to and from other
  methods.}
}
\value{
  Returns an object of class \code{FLXMCLmultinom}
  inheriting from \code{FLXMCL}.
}
\description{
  This is a model driver for \code{\link[flexmix]{flexmix}}
  implementing mixtures of Multinomial Regression Models.
}
\examples{
library(locClassData)
data <- flashData(1000)
grid <- expand.grid(x.1=seq(-6,6,0.2), x.2=seq(-4,4,0.2))

cluster <- kmeans(data$x, center = 2)$cluster
model <- FLXMCLmultinom(trace = FALSE)
fit <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = model, cluster = cluster)

## prediction for single component models without aggregation
pred.grid <- predict(fit, newdata = grid)
image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add = TRUE)
points(data$x, pch = as.character(data$y))

image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))))
contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))), add = TRUE)
points(data$x, pch = as.character(data$y))

## prediction with aggregation depending on membership in mixture components
pred.grid <- mypredict(fit, newdata = grid, aggregate = TRUE)
image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add  = TRUE)
points(data$x, pch = as.character(data$y))

## local memberhsip
loc.grid <- prior(fit, newdata = grid)
contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(loc.grid[,1], length(seq(-6,6,0.2))), add  = TRUE)
}

