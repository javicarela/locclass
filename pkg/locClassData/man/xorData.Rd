\name{xorData}
\alias{xorBayesClass}
\alias{xorData}
\alias{xorLabels}
\alias{xorPosterior}
\title{Generation of an xor like Classification Problem}
\usage{
  xorData(n, prior = rep(0.5, 2), lambda = rep(0.5, 2),
    mu11 = c(2, 2), mu12 = c(-2, -2), mu21 = c(-2, 2),
    mu22 = c(2, -2), sigma = diag(2))

  xorLabels(data, prior = rep(0.5, 2),
    lambda = rep(0.5, 2), mu11 = c(2, 2), mu12 = c(-2, -2),
    mu21 = c(-2, 2), mu22 = c(2, -2), sigma = diag(2))

  xorPosterior(data, prior = rep(0.5, 2),
    lambda = rep(0.5, 2), mu11 = c(2, 2), mu12 = c(-2, -2),
    mu21 = c(-2, 2), mu22 = c(2, -2), sigma = diag(2))

  xorBayesClass(data, prior = rep(0.5, 2),
    lambda = rep(0.5, 2), mu11 = c(2, 2), mu12 = c(-2, -2),
    mu21 = c(-2, 2), mu22 = c(2, -2), sigma = diag(2))
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

  \item{mu11}{Class center of first class, a vector.}

  \item{mu12}{Class center of first class, a vector.}

  \item{mu21}{Class center of second class, a vector.}

  \item{mu22}{Class center of second class, a vector.}

  \item{sigma}{Covariance matrices for class 1 and 2.}

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

