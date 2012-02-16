\name{outlierWrongData}
\alias{outlierWrongBayesClass}
\alias{outlierWrongData}
\alias{outlierWrongLabels}
\alias{outlierWrongPosterior}
\title{Generation of a Classification Problem with Outliers From The Wrong Class}
\usage{
  outlierWrongData(n, alpha = 5, prior = rep(0.5, 2))

  outlierWrongLabels(data, alpha = 5, prior = rep(0.5, 2))

  outlierWrongPosterior(data, alpha = 5,
    prior = rep(0.5, 2))

  outlierWrongBayesClass(data, alpha = 5,
    prior = rep(0.5, 2))
}
\arguments{
  \item{n}{Number of observations.}

  \item{alpha}{Distance from class center to the outliers.}

  \item{prior}{Vector of class prior probabilities.}

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
train <- outlierWrongData(n = 1000, alpha = 20)
test <- outlierWrongData(n = 1000, alpha = 20)

# Generate a grid of points
x.1 <- x.2 <- seq(-7,15,0.1)
grid <- expand.grid(x.1 = x.1, x.2 = x.2)

# Calculate the posterior probablities for all grid points
gridPosterior <- outlierWrongPosterior(grid, alpha = 20)

# Draw contour lines of posterior probabilities and plot training observations
plot(train$x, col = train$y)
contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray", add = TRUE, levels = 0.5)

# Calculate Bayes error
ybayes <- outlierWrongBayesClass(test$x, alpha = 20)
mean(ybayes != test$y)

if (require(MASS)) {

	   # Fit an LDA model and calculate misclassification rate on the test data set
    tr <- qda(y ~ ., data = as.data.frame(train))
    pred <- predict(tr, as.data.frame(test))
    mean(pred$class != test$y)

    # Draw decision boundary
    gridPred <- predict(tr, grid)
    contour(x.1, x.2, matrix(gridPred$posterior[,1], length(x.1)), col = "red", levels = 0.5, add = TRUE)

}
}

