\name{twonormLinearData}
\alias{twonormLinearBayesClass}
\alias{twonormLinearData}
\alias{twonormLinearLabels}
\alias{twonormLinearPosterior}
\title{Generation of Two-Class Classification Problem with two Gaussian distributions}
\usage{
  twonormLinearData(n, prior = rep(0.5, 2), mu1 = c(1, 0),
    mu2 = c(0, -1), sigma = diag(2))

  twonormLinearLabels(data, prior = rep(0.5, 2),
    mu1 = c(1, 0), mu2 = c(0, -1), sigma = diag(2))

  twonormLinearPosterior(data, prior = rep(0.5, 2),
    mu1 = c(1, 0), mu2 = c(0, -1), sigma = diag(2))

  twonormLinearBayesClass(data, prior = rep(0.5, 2),
    mu1 = c(1, 0), mu2 = c(0, -1), sigma = diag(2))
}
\arguments{
  \item{n}{Number of observations.}

  \item{prior}{Vector of class prior probabilities.}

  \item{mu1}{Class center of first class, a vector.}

  \item{mu2}{Class center of second class, a vector.}

  \item{sigma}{Covariance matrix for classes 1 and 2.}

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

