\name{hvData}
\alias{hvBayesClass}
\alias{hvData}
\alias{hvLabels}
\alias{hvPosterior}
\title{Create the Artificial Dataset of Hand and Vinciotti (2003)}
\usage{
  hvData(n, d = 2, k = 2)

  hvLabels(data, k = 2)

  hvPosterior(data, k = 2)

  hvBayesClass(data, k = 2)
}
\arguments{
  \item{n}{Number of observations.}

  \item{d}{The dimensionality.}

  \item{k}{Parameter to adjust the class prior
  probabilities.}

  \item{data}{A \code{data.frame}.}
}
\value{
  \code{hvData} returns an object of class
  \code{"locClass"}, a list with components: \item{x}{(A
  matrix.) The explanatory variables.} \item{y}{(A factor.)
  The class labels.}

  \code{hvLabels} returns a factor of class labels.

  \code{hvPosterior} returns a matrix of posterior
  probabilities.

  \code{hvBayesClass} returns a factor of Bayes
  predictions.
}
\description{
  Create the artificial data set of Hand and Vinciotti
  (2003).
}
\examples{
# Generate a training and a test set
train <- hvData(1000)
test <- hvData(1000)

# Generate a grid of points
x.1 <- x.2 <- seq(0.01,1,0.01)
grid <- expand.grid(x.1 = x.1, x.2 = x.2)

# Calculate the posterior probablities for all grid points
gridPosterior <- hvPosterior(grid)

# Draw contour lines of posterior probabilities and plot training observations
contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray")
points(train$x, col = train$y)

# Calculate Bayes error
ybayes <- hvBayesClass(test$x)
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
\references{
  Hand, D. J., Vinciotti, V. (2003), Local versus global
  models for classification problems: Fitting models where
  it matters, \emph{The American Statistician},
  \bold{57(2)} 124--130.
}

