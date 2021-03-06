\name{predict.dasvm}
\alias{predict.dasvm}
\title{Predict Method for Discriminant Adaptive Support Vector Machines}
\usage{
  \method{predict}{dasvm} (object, newdata, ...)
}
\arguments{
  \item{object}{Object of class \code{"dasvm"}, created by
  \code{dasvm}.}

  \item{newdata}{An object containing the new input data:
  either a matrix or a sparse matrix (object of class
  \code{\link[Matrix]{Matrix}} provided by the \pkg{Matrix}
  package, or of class \code{\link[SparseM]{matrix.csr}}
  provided by the \pkg{SparseM} package, or of class
  \code{\link[slam]{simple_triplet_matrix}} provided by the
  \pkg{slam} package). A vector will be transformed to a n
  x 1 matrix.}

  \item{\dots}{Further arguments to \code{\link{wsvm}},
  e.g. \code{decision.values}, \code{probability},
  \code{na.action}.}
}
\value{
  A vector of predicted values (for classification: a
  vector of labels, for density estimation: a logical
  vector). If \code{decision.value} is \code{TRUE}, the
  vector gets a \code{"decision.values"} attribute
  containing a \eqn{n * c} matrix (n number of predicted
  values, \eqn{c} number of classifiers) of all \eqn{c}
  binary classifiers' decision values. There are \eqn{k *
  (k - 1) / 2} classifiers (\eqn{k} number of classes). The
  colnames of the matrix indicate the labels of the two
  classes. If \code{probability} is \code{TRUE}, the vector
  gets a \code{"probabilities"} attribute containing a
  \eqn{n * k} matrix (\eqn{n} number of predicted values,
  \eqn{k} number of classes) of the class probabilities.
}
\description{
  This function predicts values based upon a model trained
  by \code{\link{dasvm}}.
}
\note{
  If the training set was scaled by \code{\link{dasvm}}
  (done by default), the new data is scaled accordingly
  using scale and center of the training data.
}
\examples{
fit1 <- wsvm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = "linear",
	  probability = TRUE)
pred <- predict(fit1)
mean(pred != iris$Species)

fit2 <- dasvm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = "linear",
    wf = "gaussian", bw = 0.5, method = "prob", probability = TRUE)
pred <- predict(fit2)
mean(pred != iris$Species)

fit3 <- dasvm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = "linear",
    wf = "gaussian", bw = 0.8, method = "decision", probability = TRUE)
pred <- predict(fit3)
mean(pred != iris$Species)

## plot of decision boundary (maximum posterior probabilities):
grid <- expand.grid(Sepal.Length = seq(4,8,0.1), Sepal.Width = seq(2,5,0.1))

probgrid1 <- attr(predict(fit1, newdata = grid, probability = TRUE), "probabilities")
decgrid1 <- attr(predict(fit1, newdata = grid, decision.values = TRUE), "decision.values")
probgrid2 <- attr(predict(fit2, newdata = grid, probability = TRUE), "probabilities")
decgrid3 <- attr(predict(fit3, newdata = grid, decision.values = TRUE), "decision.values")

par(mfrow = c(1,2))
contour(seq(4,8,0.1), seq(2,5,0.1),
    matrix(as.numeric(apply(probgrid1, 1, max)), nrow = length(seq(4,8,0.1))))
contour(seq(4,8,0.1), seq(2,5,0.1),
    matrix(as.numeric(apply(probgrid2, 1, max)), nrow = length(seq(4,8,0.1))))
points(iris$Sepal.Length, iris$Sepal.Width, pch = 19,
    cex = fit2$case.weights[[3]]*2, col = as.numeric(iris$Species))

par(mfrow = c(1,2))
contour(seq(4,8,0.1), seq(2,5,0.1),
    matrix(as.numeric(decgrid1[,1]), nrow = length(seq(4,8,0.1))))
contour(seq(4,8,0.1), seq(2,5,0.1),
    matrix(as.numeric(decgrid3[,1]), nrow = length(seq(4,8,0.1))))
points(iris$Sepal.Length, iris$Sepal.Width, pch = 19,
    cex = fit3$case.weights[[3]]*2, col = as.numeric(iris$Species))

contour(seq(4,8,0.1), seq(2,5,0.1),
    matrix(as.numeric(decgrid1[,2]), nrow = length(seq(4,8,0.1))))
contour(seq(4,8,0.1), seq(2,5,0.1),
    matrix(as.numeric(decgrid3[,2]), nrow = length(seq(4,8,0.1))))
points(iris$Sepal.Length, iris$Sepal.Width, pch = 19,
    cex = fit3$case.weights[[3]]*2, col = as.numeric(iris$Species))

contour(seq(4,8,0.1), seq(2,5,0.1),
    matrix(as.numeric(decgrid1[,3]), nrow = length(seq(4,8,0.1))))
contour(seq(4,8,0.1), seq(2,5,0.1),
    matrix(as.numeric(decgrid3[,3]), nrow = length(seq(4,8,0.1))))
points(iris$Sepal.Length, iris$Sepal.Width, pch = 19,
    cex = fit3$case.weights[[3]]*2, col = as.numeric(iris$Species))
}
\references{
  Hand, D. J., Vinciotti, V. (2003), Local versus global
  models for classification problems: Fitting models where
  it matters, \emph{The American Statistician},
  \bold{57(2)} 124--130.
}
\seealso{
  \code{\link{dasvm}}, \code{\link{wsvm}},
  \code{\link[e1071]{svm}}.
}
\keyword{classif}
\keyword{neural,}
\keyword{nonlinear,}

