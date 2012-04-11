\name{ringData}
\alias{ringBayesClass}
\alias{ringData}
\alias{ringLabels}
\alias{ringPosterior}
\title{Generation of a Binary Classification Problem with Two Gaussian Distributions}
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
  \code{ringData} returns an object of class
  \code{"locClass"}, a list with components: \item{x}{(A
  matrix.) The explanatory variables.} \item{y}{(A factor.)
  The class labels.}

  \code{ringLabels} returns a factor of class labels.

  \code{ringPosterior} returns a matrix of posterior
  probabilities.

  \code{ringBayesClass} returns a factor of Bayes
  predictions.
}
\description{
  Generation of a binary classification problem with two
  Gaussian distributions with euqal means and different
  covariance matrices.
}

