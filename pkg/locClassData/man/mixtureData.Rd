\name{mixtureData}
\alias{mixtureBayesClass}
\alias{mixtureData}
\alias{mixtureLabels}
\alias{mixturePosterior}
\title{Generation of Gaussian Mixture Data for Classification}
\usage{
  mixtureData(n, prior, lambda, mu, sigma)

  mixtureLabels(data, prior, lambda, mu, sigma)

  mixturePosterior(data, prior, lambda, mu, sigma)

  mixtureBayesClass(data, prior, lambda, mu, sigma)
}
\arguments{
  \item{n}{Number of observations.}

  \item{prior}{Vector of class prior probabilities.}

  \item{lambda}{The conditional probabilities for the
  mixture components given the class. Either a vector (if
  the same number \eqn{m} of mixture components is desired
  for each class and the conditional probabilities for each
  class should be equal) or a list as long as the number of
  classes containing one vector of probabilities for every
  class. The length of the \eqn{k}-th element is the
  desired number of mixture components for the \eqn{k}-th
  class.}

  \item{mu}{The centers of the mixture components. A list
  containing one \eqn{m_k \times d}{m_k x d} matrix of
  centers for each class where \eqn{d} is the desired
  dimensionality of the data set.}

  \item{sigma}{The covariance matrices of the mixture
  components. Either one single matrix that is used for
  each mixture component or a list as long as the number of
  classes. List elements can be matrices (in case that for
  all mixture components forming one class the same
  covariance matrix shall be used) or lists of matrices as
  long as the number of mixture components in the
  corresponding class.}

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
## Simplest case:
# lambda vector, sigma matrix
# Generate a training and a test set
mu <- list(matrix(c(-1,-1),1), matrix(rep(c(1,4.5,12),2),3))
train <- mixtureData(n = 1000, prior = rep(0.5,2), lambda = list(1, rep(1/3,3)), mu = mu, sigma = 3*diag(2))
test <- mixtureData(n = 1000, prior = rep(0.5,2), lambda = list(1, rep(1/3,3)), mu = mu, sigma = 3*diag(2))

# Generate a grid of points
x.1 <- x.2 <- seq(-7,15,0.1)
grid <- expand.grid(x.1 = x.1, x.2 = x.2)

# Calculate the posterior probablities for all grid points
gridPosterior <- mixturePosterior(grid, prior = rep(0.5,2), lambda = list(1, rep(1/3,3)), mu = mu, sigma = 3*diag(2))

# Draw contour lines of posterior probabilities and plot training observations
contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray")
points(train$x, col = train$y)

# Calculate Bayes error
ybayes <- mixtureBayesClass(test$x, prior = rep(0.5,2), lambda = list(1, rep(1/3,3)), mu = mu, sigma = 3*diag(2))
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

## lambda list, sigma list of lists of matrices
mu <- list()
mu[[1]] <- matrix(c(1,5,1,5),2)
mu[[2]] <- matrix(c(8,11,8,11),2)
lambda <- list(c(0.5, 0.5), c(0.1, 0.9))
sigma <- list()
sigma[[1]] <- diag(2)
sigma[[2]] <- list(diag(2), 3*diag(2))
data <- mixtureData(n = 100, prior = c(0.3, 0.7), lambda, mu, sigma)
plot(data$x, col = data$y)
}

