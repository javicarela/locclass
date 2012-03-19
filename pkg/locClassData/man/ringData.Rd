\name{ringData}
\alias{ringBayesClass}
\alias{ringData}
\alias{ringLabels}
\alias{ringPosterior}
\title{Generation of Two-Class Classification Problem with two Gaussian distributions}
\usage{
  ringData(n, prior = rep(0.5, 2), sigma1 = diag(2),
    sigma2 = 2 * diag(2))

  ringLabels(data, prior = rep(0.5, 2), sigma1 = diag(2),
    sigma2 = 2 * diag(2))

  ringPosterior(data, prior = rep(0.5, 2),
    sigma1 = diag(2), sigma2 = 2 * diag(2))

  ringBayesClass(data, prior = rep(0.5, 2),
    sigma1 = diag(2), sigma2 = 2 * diag(2))
}
\arguments{
  \item{n}{Number of observations.}

  \item{prior}{Vector of class prior probabilities.}

  \item{sigma1}{Covariance matrix for class 1.}

  \item{sigma2}{Covariance matrix for class 2.}

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

