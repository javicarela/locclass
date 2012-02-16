\name{hvQuadraticData}
\alias{hvQuadraticBayesClass}
\alias{hvQuadraticData}
\alias{hvQuadraticLabels}
\alias{hvQuadraticPosterior}
\title{Create the Artificial Dataset of Hand and Vinciotti (2003)}
\usage{
  hvQuadraticData(n, d = 2, k = 2)

  hvQuadraticLabels(data, k = 2)

  hvQuadraticPosterior(data, k = 2)

  hvQuadraticBayesClass(data, k = 2)
}
\arguments{
  \item{n}{Number of observations.}

  \item{d}{The dimensionality.}

  \item{k}{Parameter to adjust the class prior
  probabilities.}

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
train <- hvQuadraticData(1000)
test <- hvQuadraticData(1000)

# Generate a grid of points
x.1 <- x.2 <- seq(0.01,1,0.01)
grid <- expand.grid(x.1 = x.1, x.2 = x.2)

# Calculate the posterior probablities for all grid points
gridPosterior <- hvQuadraticPosterior(grid)

# Draw contour lines of posterior probabilities and plot training observations
contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray")
points(train$x, col = train$y)

# Calculate Bayes error
ybayes <- hvQuadraticBayesClass(test$x)
mean(ybayes != test$y)

if (require(MASS)) {

	   # Fit a QDA model and calculate misclassification rate on the test data set
    tr <- qda(y ~ ., data = as.data.frame(train))
    pred <- predict(tr, as.data.frame(test))
    mean(pred$class != test$y)

    # Draw decision boundary
    gridPred <- predict(tr, grid)
    contour(x.1, x.2, matrix(gridPred$posterior[,1], length(x.1)), col = "red", levels = 0.5, add = TRUE)

}
}
\references{
  Hand, D. J., Vinciotti, V. (2003), Local versus global
  models for classification problems: Fitting models where
  it matters, \emph{The American Statistician},
  \bold{57(2)} 124--130.
}

