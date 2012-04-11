\name{flashData}
\alias{flashBayesClass}
\alias{flashData}
\alias{flashLabels}
\alias{flashPosterior}
\title{Generation of a Binary Classification Problem with a Lightning-Shaped Decision Boundary}
\usage{
  flashData(n, prior = rep(0.5, 2), lambda = rep(0.5, 2),
    sigma = diag(2))

  flashLabels(data, prior = rep(0.5, 2),
    lambda = rep(0.5, 2), sigma = diag(2))

  flashPosterior(data, prior = rep(0.5, 2),
    lambda = rep(0.5, 2), sigma = diag(2))

  flashBayesClass(data, prior = rep(0.5, 2),
    lambda = rep(0.5, 2), sigma = diag(2))
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
  \code{flashData} returns an object of class
  \code{"locClass"}, a list with components: \item{x}{(A
  matrix.) The explanatory variables.} \item{y}{(A factor.)
  The class labels.}

  \code{flashLabels} returns a factor of class labels.

  \code{flashPosterior} returns a matrix of posterior
  probabilities.

  \code{flashBayesClass} returns a factor of Bayes
  predictions.
}
\description{
  Generation of a binary classification problem with a
  lightning-shaped decision boundary.
}

