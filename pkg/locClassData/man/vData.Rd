\name{vData}
\alias{vBayesClass}
\alias{vData}
\alias{vLabels}
\alias{vPosterior}
\title{Create a Binary Classification Problem with V-shaped Decision Boundary}
\usage{
  vData(n, d = 2, k = 1)

  vLabels(data, k = 1)

  vPosterior(data, k = 1)

  vBayesClass(data, k = 1)
}
\arguments{
  \item{n}{Number of observations.}

  \item{d}{The dimensionality.}

  \item{k}{Parameter to adjust the noise level.}

  \item{data}{A \code{data.frame}.}
}
\value{
  \code{vData} returns an object of class
  \code{"locClass"}, a list with components: \item{x}{(A
  matrix.) The explanatory variables.} \item{y}{(A factor.)
  The class labels.}

  \code{vLabels} returns a factor of class labels.

  \code{vPosterior} returns a matrix of posterior
  probabilities.

  \code{vBayesClass} returns a factor of Bayes predictions.
}
\description{
  Create a binary classification problem with V-shaped
  decision boundary.
}
\examples{
# Generate a training and a test set
train <- vData(1000)
test <- vData(1000)

# Generate a grid of points
x.1 <- x.2 <- seq(0.01,1,0.01)
grid <- expand.grid(x.1 = x.1, x.2 = x.2)

# Calculate the posterior probablities for all grid points
gridPosterior <- vPosterior(grid)

# Draw contour lines of posterior probabilities and plot training observations
contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray")
points(train$x, col = train$y)

# Calculate Bayes error
ybayes <- vBayesClass(test$x)
mean(ybayes != test$y)

if (require(MASS)) {

    # Fit an LDA model and calculate misclassification rate on the test data set
    tr <- lda(y ~ ., data = as.data.frame(train))
    pred <- predict(tr, as.data.frame(test))
    mean(pred$class != test$y)

    # Draw decision boundary
    gridPred <- predict(tr, grid)
    contour(x.1, x.2, matrix(gridPred$posterior[,1], length(x.1)), col = "red", levels = 0.5, add = TRUE)

}
}

