\name{xor3Data}
\alias{xor3BayesClass}
\alias{xor3Data}
\alias{xor3Labels}
\alias{xor3Posterior}
\title{Generation of a Checkerboard Like Classification Problem with 3 Classes}
\usage{
  xor3Data(n, prior = rep(1/3, 3), lambda = rep(1/3, 3),
    mu11 = c(-4, 4), mu12 = c(0, -4), mu13 = c(4, 0),
    mu21 = c(-4, 0), mu22 = c(0, 4), mu23 = c(4, -4),
    mu31 = c(-4, -4), mu32 = c(0, 0), mu33 = c(4, 4),
    sigma = diag(2))

  xor3Labels(data, prior = rep(1/3, 3),
    lambda = rep(1/3, 3), mu11 = c(-4, 4), mu12 = c(0, -4),
    mu13 = c(4, 0), mu21 = c(-4, 0), mu22 = c(0, 4),
    mu23 = c(4, -4), mu31 = c(-4, -4), mu32 = c(0, 0),
    mu33 = c(4, 4), sigma = diag(2))

  xor3Posterior(data, prior = rep(1/3, 3),
    lambda = rep(1/3, 3), mu11 = c(-4, 4), mu12 = c(0, -4),
    mu13 = c(4, 0), mu21 = c(-4, 0), mu22 = c(0, 4),
    mu23 = c(4, -4), mu31 = c(-4, -4), mu32 = c(0, 0),
    mu33 = c(4, 4), sigma = diag(2))

  xor3BayesClass(data, prior = rep(1/3, 3),
    lambda = rep(1/3, 3), mu11 = c(-4, 4), mu12 = c(0, -4),
    mu13 = c(4, 0), mu21 = c(-4, 0), mu22 = c(0, 4),
    mu23 = c(4, -4), mu31 = c(-4, -4), mu32 = c(0, 0),
    mu33 = c(4, 4), sigma = diag(2))
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

  \item{mu13}{Class center of first class, a vector.}

  \item{mu21}{Class center of second class, a vector.}

  \item{mu22}{Class center of second class, a vector.}

  \item{mu23}{Class center of second class, a vector.}

  \item{mu31}{Class center of second class, a vector.}

  \item{mu32}{Class center of second class, a vector.}

  \item{mu33}{Class center of second class, a vector.}

  \item{sigma}{Covariance matrix for classes 1, 2, and 3.}

  \item{data}{A \code{data.frame}.}
}
\value{
  \code{xor3Data} returns an object of class
  \code{"locClass"}, a list with components: \item{x}{(A
  matrix.) The explanatory variables.} \item{y}{(A factor.)
  The class labels.}

  \code{xor3Labels} returns a factor of class labels.

  \code{xor3Posterior} returns a matrix of posterior
  probabilities.

  \code{xor3BayesClass} returns a factor of Bayes
  predictions.
}
\description{
  Generation of a checkerboard like classification problem
  with 3 classes.
}

