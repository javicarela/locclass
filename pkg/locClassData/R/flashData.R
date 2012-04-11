#  Copyright (C) 2011 Julia Schiffner
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

#' Generation of a binary classification problem with a lightning-shaped decision boundary.
#'
# details
#'
#' @title Generation of a Binary Classification Problem with a Lightning-Shaped Decision Boundary
#' 
#' @param n Number of observations.
#' @param prior Vector of class prior probabilities.
#' @param lambda The conditional probabilities for the mixture components given the class. Either a vector (if the same number \eqn{m} 
#'   of mixture components is desired for each class and the conditional probabilities for each class should be equal) or a list as long
#'   as the number of classes containing one vector of probabilities for every class. The length of the \eqn{k}-th element is the desired 
#'   number of mixture components for the \eqn{k}-th class.
#' @param sigma The covariance matrices of the mixture components. Either one single matrix that is used for each mixture
#'   component or a list as long as the number of classes. List elements can be matrices (in case that for all mixture components forming
#'   one class the same covariance matrix shall be used) or lists of matrices as long as the number of mixture components in the corresponding 
#'   class.
#' @param data A \code{data.frame}.
#'
#' @return
#' \code{flashData} returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#'
#'
#' @aliases flashData flashLabels flashPosterior flashBayesClass
#'
#' @rdname flashData
#'
#' @import mvtnorm
#'
#' @export

flashData <- function(n, prior = rep(0.5,2), lambda = rep(0.5,2), sigma = diag(2)) {
	mu <- list(rbind(c(-2,2), c(1,-1)), rbind(c(0,1), c(3,-2)))
	data <- mixtureData(n, prior, lambda, mu, sigma)
	class(data) <- c("locClass.flashData", class(data))
	return(data)
}



#' @return \code{flashLabels} returns a factor of class labels.
#'
#' @rdname flashData
#'
#' @export

flashLabels <- function(data, prior = rep(0.5,2), lambda = rep(0.5,2), sigma = diag(2)) {
	mu <- list(rbind(c(-2,2), c(1,-1)), rbind(c(0,1), c(3,-2)))
	return(mixtureLabels(data, prior, lambda, mu, sigma))
}	



#' @return \code{flashPosterior} returns a matrix of posterior probabilities.
#'
#' @rdname flashData
#'
#' @export

flashPosterior <- function(data, prior = rep(0.5,2), lambda = rep(0.5,2), sigma = diag(2)) {
	mu <- list(rbind(c(-2,2), c(1,-1)), rbind(c(0,1), c(3,-2)))
	return(mixturePosterior(data, prior, lambda, mu, sigma))
}



#' @return \code{flashBayesClass} returns a factor of Bayes predictions.
#'
#' @rdname flashData
#'
#' @export

flashBayesClass <- function(data, prior = rep(0.5,2), lambda = rep(0.5,2), sigma = diag(2)) {
	mu <- list(rbind(c(-2,2), c(1,-1)), rbind(c(0,1), c(3,-2)))
	return(mixtureBayesClass(data, prior, lambda, mu, sigma))
}
