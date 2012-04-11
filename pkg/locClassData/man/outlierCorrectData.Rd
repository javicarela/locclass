\name{outlierCorrectData}
\alias{outlierCorrectBayesClass}
\alias{outlierCorrectData}
\alias{outlierCorrectLabels}
\alias{outlierCorrectPosterior}
\title{Generation of a Binary Classification Problem with Outliers}
\usage{
  outlierCorrectData(n, alpha = 5, beta = 5, prop = 0.05,
    prior = rep(0.5, 2))

  outlierCorrectLabels(data, alpha = 5, beta = 5,
    prop = 0.05, prior = rep(0.5, 2))

  outlierCorrectPosterior(data, alpha = 5, beta = 5,
    prop = 0.05, prior = rep(0.5, 2))

  outlierCorrectBayesClass(data, alpha = 5, beta = 5,
    prop = 0.05, prior = rep(0.5, 2))
}
\arguments{
  \item{n}{Number of observations.}

  \item{alpha}{Distance from class center to the outliers
  in the x-coordinate.}

  \item{beta}{Distance from class center to the outliers in
  the y-coordinate.}

  \item{prop}{Proportion of outliers. Defaults to 0.05.}

  \item{prior}{Vector of class prior probabilities.
  Defaults to equal class priors.}

  \item{data}{A \code{data.frame}.}
}
\value{
  \code{outlierCorrectData} returns an object of class
  \code{"locClass"}, a list with components: \item{x}{(A
  matrix.) The explanatory variables.} \item{y}{(A factor.)
  The class labels.}

  \code{outlierCorrectLabels} returns a factor of class
  labels.

  \code{outlierCorrectPosterior} returns a matrix of
  posterior probabilities.

  \code{outlierCorrectBayesClass} returns a factor of Bayes
  predictions.
}
\description{
  Generation of a binary classification problem with
  outliers on the correct side of the decision boundary.
}
\examples{
# Generate a training and a test set
train <- outlierCorrectData(n = 1000)
test <- outlierCorrectData(n = 1000)

# Generate a grid of points
x.1 <- x.2 <- seq(-7,15,0.1)
grid <- expand.grid(x.1 = x.1, x.2 = x.2)

# Calculate the posterior probablities for all grid points
gridPosterior <- outlierCorrectPosterior(grid)

# Draw contour lines of posterior probabilities and plot training observations
plot(train$x, col = train$y)
contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray", add = TRUE)

# Calculate Bayes error
ybayes <- outlierCorrectBayesClass(test$x)
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

