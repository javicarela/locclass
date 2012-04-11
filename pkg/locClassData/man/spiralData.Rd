\name{spiralData}
\alias{spiralBayesClass}
\alias{spiralData}
\alias{spiralLabels}
\alias{spiralPosterior}
\title{Create Spiral Data}
\usage{
  spiralData(n = 100, cycles = 1)

  spiralLabels(data, cycles = 1)

  spiralPosterior(data, cycles = 1)

  spiralBayesClass(data, cycles = 1)
}
\arguments{
  \item{n}{Number of observations.}

  \item{cycles}{Number of cycles the spiral makes.}

  \item{data}{A \code{data.frame}.}
}
\value{
  \code{spiralData} returns an object of class
  \code{"locClass"}, a list with components: \item{x}{(A
  matrix.) The explanatory variables.} \item{y}{(A factor.)
  The class labels.}

  \code{spiralLabels} returns a factor of class labels.

  \code{spiralPosterior} returns a matrix of posterior
  probabilities.

  \code{spiralBayesClass} returns a factor of Bayes
  predictions.
}
\description{
  Create a binary classification problem with spiral-shaped
  decision boundary.
}
\details{
  These functions are based on the function
  \code{\link[mlbench]{mlbench.1spiral}} from package
  \pkg{mlbench}.
}
\examples{
# Generate a training and a test set
train <- spiralData(1000)
test <- spiralData(1000)

# Generate a grid of points
x.1 <- x.2 <- seq(-2,2,0.05)
grid <- expand.grid(x.1 = x.1, x.2 = x.2)

# Calculate the posterior probablities for all grid points
gridPosterior <- spiralPosterior(grid)

# Draw contour lines of posterior probabilities and plot training observations
contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray")
points(train$x, col = train$y)

# Calculate Bayes error
ybayes <- spiralBayesClass(test$x)
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

