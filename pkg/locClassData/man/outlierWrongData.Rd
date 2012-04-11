\name{outlierWrongData}
\alias{outlierWrongBayesClass}
\alias{outlierWrongData}
\alias{outlierWrongLabels}
\alias{outlierWrongPosterior}
\title{Generation of a Binary Classification Problem with Outliers}
\usage{
  outlierWrongData(n, alpha = 5, beta = 5, prop = 0.05,
    prior = rep(0.5, 2))

  outlierWrongLabels(data, alpha = 5, beta = 5,
    prop = 0.05, prior = rep(0.5, 2))

  outlierWrongPosterior(data, alpha = 5, beta = 5,
    prop = 0.05, prior = rep(0.5, 2))

  outlierWrongBayesClass(data, alpha = 5, beta = 5,
    prop = 0.05, prior = rep(0.5, 2))
}
\arguments{
  \item{n}{Number of observations.}

  \item{alpha}{Distance from class center to the outliers
  in the x-coordinate.}

  \item{beta}{Distance from class center to the outliers in
  the y-coordinate.}

  \item{prop}{Proportion of outliers. Defaults to 0.05.}

  \item{prior}{Vector of class prior probabilities.}

  \item{data}{A \code{data.frame}.}
}
\value{
  \code{outlierWrongData} returns an object of class
  \code{"locClass"}, a list with components: \item{x}{(A
  matrix.) The explanatory variables.} \item{y}{(A factor.)
  The class labels.}

  \code{outlierWrongLabels} returns a factor of class
  labels.

  \code{outlierWrongPosterior} returns a matrix of
  posterior probabilities.

  \code{outlierWrongBayesClass} returns a factor of Bayes
  predictions.
}
\description{
  Generation of a binary classification problem with
  outliers on the wrong side of the decision boundary.
}
\examples{
# Generate a training and a test set
train <- outlierWrongData(n = 1000, alpha = 10, beta = 10)
test <- outlierWrongData(n = 1000, alpha = 10, beta = 10)

# Generate a grid of points
x.1 <- x.2 <- seq(-7,15,0.1)
grid <- expand.grid(x.1 = x.1, x.2 = x.2)

# Calculate the posterior probablities for all grid points
gridPosterior <- outlierWrongPosterior(grid, alpha = 10, beta = 10)

# Draw contour lines of posterior probabilities and plot training observations
plot(train$x, col = train$y)
contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray", add = TRUE, levels = 0.5)

# Calculate Bayes error
ybayes <- outlierWrongBayesClass(test$x, alpha = 10, beta = 10)
mean(ybayes != test$y)

if (require(MASS)) {

    # Fit an LDA model and calculate misclassification rate on the test data set
    tr <- lda(y ~ ., data = as.data.frame(train))
    pred <- predict(tr, as.data.frame(test))
    print(mean(pred$class != test$y))

    # Draw decision boundary
    gridPred <- predict(tr, grid)
    contour(x.1, x.2, matrix(gridPred$posterior[,1], length(x.1)), col = "red", levels = 0.5, add = TRUE)

}
}

