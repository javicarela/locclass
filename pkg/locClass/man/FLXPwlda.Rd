\docType{methods}
\name{FLXPwlda}
\alias{FLXgetModelmatrix}
\alias{FLXgetModelmatrix,FLXPwlda-method}
\alias{FLXPwlda}
\title{Creator Function for the Concomitant Variable Model based on Linear Discriminant Analysis}
\usage{
  FLXPwlda(formula = ~.)
}
\arguments{
  \item{formula}{A formula for determining the model matrix
  of the concomitant variables.}
}
\value{
  Object of class \code{FLXPwlda} which extends class
  \code{FLXP} directly and is used for method dispatching.
}
\description{
  Creator function for the concomitant variable model.
  Priors are modeled by Linear Discriminant Analysis.
}
\examples{
library(locClassData)
data <- flashData(1000)
grid <- expand.grid(x.1=seq(-6,6,0.2), x.2=seq(-4,4,0.2))

cluster <- kmeans(data$x, center = 2)$cluster
model <- FLXMCLlda()
fit <- flexmix(y ~ x.1 + x.2, data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = model, cluster = cluster)

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

## local membership
loc.grid <- prior(fit, newdata = grid)
contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(loc.grid[,1], length(seq(-6,6,0.2))), add  = TRUE)
}
\seealso{
  \code{\link[flexmix]{FLXPmultinom}}.
}

