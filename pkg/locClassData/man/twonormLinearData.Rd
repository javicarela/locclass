\name{twonormLinearData}
\alias{twonormLinearBayesClass}
\alias{twonormLinearData}
\alias{twonormLinearLabels}
\alias{twonormLinearPosterior}
\title{Generate a Binary Classification Problem with Two Gaussian Distributions}
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
  \code{twonormLinearData} returns an object of class
  \code{"locClass"}, a list with components: \item{x}{(A
  matrix.) The explanatory variables.} \item{y}{(A factor.)
  The class labels.}

  \code{twonormLinearLabels} returns a factor of class
  labels.

  \code{twonormLinearPosterior} returns a matrix of
  posterior probabilities.

  \code{twonormLinearBayesClass} returns a factor of Bayes
  predictions.
}
\description{
  Generate a binary classification problem with two
  Gaussian distributions with different means and equal
  covariance matrices.
}

