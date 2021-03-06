\name{twonormQuadraticData}
\alias{twonormQuadraticBayesClass}
\alias{twonormQuadraticData}
\alias{twonormQuadraticLabels}
\alias{twonormQuadraticPosterior}
\title{Generate a Binary Classification Problem with Two Gaussian Distributions}
\usage{
  twonormQuadraticData(n, prior = rep(0.5, 2),
    mu1 = c(1, 0), mu2 = c(0, -1), sigma1 = diag(2),
    sigma2 = matrix(c(1, 0.5, 0.5, 1), 2))

  twonormQuadraticLabels(data, prior = rep(0.5, 2),
    mu1 = c(1, 0), mu2 = c(0, -1), sigma1 = diag(2),
    sigma2 = matrix(c(1, 0.5, 0.5, 1), 2))

  twonormQuadraticPosterior(data, prior = rep(0.5, 2),
    mu1 = c(1, 0), mu2 = c(0, -1), sigma1 = diag(2),
    sigma2 = matrix(c(1, 0.5, 0.5, 1), 2))

  twonormQuadraticBayesClass(data, prior = rep(0.5, 2),
    mu1 = c(1, 0), mu2 = c(0, -1), sigma1 = diag(2),
    sigma2 = matrix(c(1, 0.5, 0.5, 1), 2))
}
\arguments{
  \item{n}{Number of observations.}

  \item{prior}{Vector of class prior probabilities.}

  \item{mu1}{Class center of first class, a vector.}

  \item{mu2}{Class center of second class, a vector.}

  \item{sigma1}{Covariance matrix for class 1.}

  \item{sigma2}{Covariance matrix for class 2.}

  \item{data}{A \code{data.frame}.}
}
\value{
  \code{twonormQuadraticData} returns an object of class
  \code{"locClass"}, a list with components: \item{x}{(A
  matrix.) The explanatory variables.} \item{y}{(A factor.)
  The class labels.}

  \code{twonormQuadraticLabels} returns a factor of class
  labels.

  \code{twonormQuadraticPosterior} returns a matrix of
  posterior probabilities.

  \code{twonormQuadraticBayesClass} returns a factor of
  Bayes predictions.
}
\description{
  Generate a binary classification problem with two
  Gaussian distributions with different means and
  covariance matrices.
}

