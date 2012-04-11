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

#' Generation of an xor like classification problem.
#'
# details
#'
#' @title Generation of an xor Like Classification Problem
#' 
#' @param n Number of observations.
#' @param prior Vector of class prior probabilities.
#' @param lambda The conditional probabilities for the mixture components given the class. Either a vector (if the same number \eqn{m} 
#'   of mixture components is desired for each class and the conditional probabilities for each class should be equal) or a list as long
#'   as the number of classes containing one vector of probabilities for every class. The length of the \eqn{k}-th element is the desired 
#'   number of mixture components for the \eqn{k}-th class.
#' @param mu11 Class center of first class, a vector.
#' @param mu12 Class center of first class, a vector.
#' @param mu21 Class center of second class, a vector.
#' @param mu22 Class center of second class, a vector.
#' @param sigma Covariance matrix for class 1 and 2. 
#' @param data A \code{data.frame}.
#'
#' @return
#' \code{xorData} returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#'
#'
#' @aliases xorData xorLabels xorPosterior xorBayesClass
#'
#' @rdname xorData
#'
#' @import mvtnorm
#'
#' @export

xorData <- function(n, prior = rep(0.5,2), lambda = rep(0.5,2), mu11 = c(2,2), mu12 = c(-2,-2), mu21 = c(-2,2), mu22 = c(2,-2), sigma = diag(2)) {
	mu <- list(rbind(mu11,mu12), rbind(mu21, mu22))
	sigma <- list(sigma, sigma)
	data <- mixtureData(n, prior, lambda, mu, sigma)
	attr(data, "class") <- c("locClass.xorData", attr(data, "class"))
	return(data)
}



#' @return \code{xorLabels} returns a factor of class labels.
#'
#' @rdname xorData
#'
#' @export

xorLabels <- function(data, prior = rep(0.5,2), lambda = rep(0.5,2), mu11 = c(2,2), mu12 = c(-2,-2), mu21 = c(-2,2), mu22 = c(2,-2), sigma = diag(2)) {
	mu <- list(rbind(mu11,mu12), rbind(mu21, mu22))
	sigma <- list(sigma, sigma)
	return(mixtureLabels(data, prior, lambda, mu, sigma))
}	



#' @return \code{xorPosterior} returns a matrix of posterior probabilities.
#'
#' @rdname xorData
#'
#' @export

xorPosterior <- function(data, prior = rep(0.5,2), lambda = rep(0.5,2), mu11 = c(2,2), mu12 = c(-2,-2), mu21 = c(-2,2), mu22 = c(2,-2), sigma = diag(2)) {
	mu <- list(rbind(mu11,mu12), rbind(mu21, mu22))
	sigma <- list(sigma, sigma)
	return(mixturePosterior(data, prior, lambda, mu, sigma))
}



#' @return \code{xorBayesClass} returns a factor of Bayes predictions.
#'
#' @rdname xorData
#'
#' @export

xorBayesClass <- function(data, prior = rep(0.5,2), lambda = rep(0.5,2), mu11 = c(2,2), mu12 = c(-2,-2), mu21 = c(-2,2), mu22 = c(2,-2), sigma = diag(2)) {
	mu <- list(rbind(mu11,mu12), rbind(mu21, mu22))
	sigma <- list(sigma, sigma)	
	return(mixtureBayesClass(data, prior, lambda, mu, sigma))
}
