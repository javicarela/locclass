\name{predict.dalr}
\alias{predict.dalr}
\title{Classify Multivariate Observations Based on Discriminant Adaptive Logistic Regression}
\usage{
  \method{predict}{dalr} (object, newdata = NULL, ...)
}
\arguments{
  \item{object}{An object of class \code{"dalr"} inheriting
  from \code{"glm"}.}

  \item{newdata}{Optionally, a \code{data.frame} in which
  to look for variables with which to predict. If omitted,
  the fitted linear predictors are used.}

  \item{...}{Further arguments to be passed from or to
  other methods, especially to
  \code{\link{predict.glm}}???. See also the Details
  section.}
}
\value{
  A \code{list} with components: \item{class}{The predicted
  class labels (a \code{factor}).} \item{posterior}{Matrix
  of class posterior probabilities.}
}
\description{
  Obtains predicted class labels and posterior
  probabilities from a locally fitted logistic regression
  model.
}
\examples{
# generate data set:
x1 <- runif(500, min = 0, max = 1)
x2 <- runif(500, min = 0, max = 1)
x <- data.frame(x1 = x1, x2 = x2)
prob <- x$x2/(x$x1 + x$x2)
y <- as.factor(sapply(prob, function(x) sample(0:1, size = 1,
    prob = c(1-x,x))))
x <- data.frame(x, y = y)

# fit dalr on training set and predict on test set:
train <- sample(500, 300)
fit <- dalr(y ~ ., data = x, thr = 0.3, wf = "rectangular", bw = 100,
    subset = train)
pred <- predict(fit, newdata = x[-train,])
mean(y[-train] != pred$class)
}
\references{
  Hand, D. J., Vinciotti, V. (2003), Local versus global
  models for classification problems: Fitting models where
  it matters, \emph{The American Statistician},
  \bold{57(2)} 124--130.
}
\seealso{
  \code{\link{dalr}}, \code{\link{predict.glm}},
  \code{\link{glm}}.
}
\keyword{classif}
\keyword{model}
\keyword{multivariate}

