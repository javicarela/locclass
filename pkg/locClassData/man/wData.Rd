\name{wData}
\alias{wBayesClass}
\alias{wData}
\alias{wLabels}
\alias{wPosterior}
\title{Create an Artificial Classification Problem with W-shaped decision boundary}
\usage{
  wData(n, d = 2, k = 1)

  wLabels(data, k = 2)

  wPosterior(data, k = 2)

  wBayesClass(data, k = 2)
}
\arguments{
  \item{n}{Number of observations.}

  \item{d}{The dimensionality.}

  \item{k}{Parameter to adjust .}

  \item{data}{A \code{data.frame}.}
}
\value{
  returns an object of class \code{"locClass"}, a list with
  components: \item{x}{(A matrix.) The explanatory
  variables.} \item{y}{(A factor.) The class labels.}

  returns a factor of class labels.

  returns a matrix of posterior probabilities.

  returns a factor of Bayes predictions.
}
\description{
  description
}
\details{
  details
}
\examples{
# Generate a training and a test set
train <- wData(1000)
test <- wData(1000)

# Generate a grid of points
x.1 <- x.2 <- seq(0.01,1,0.01)
grid <- expand.grid(x.1 = 3 * x.1, x.2 = x.2)

# Calculate the posterior probablities for all grid points
gridPosterior <- wPosterior(grid)

# Draw contour lines of posterior probabilities and plot training observations
contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray")
points(train$x, col = train$y)

# Calculate Bayes error
ybayes <- wBayesClass(test$x)
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

