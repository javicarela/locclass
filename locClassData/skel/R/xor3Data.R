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

#' Generation of a checkerboard like classification problem with 3 classes.
#'
# details
#'
#' @title Generation of a Checkerboard Like Classification Problem with 3 Classes
#' 
#' @param n Number of observations.
#' @param prior Vector of class prior probabilities.
#' @param lambda The conditional probabilities for the mixture components given the class. Either a vector (if the same number \eqn{m} 
#'   of mixture components is desired for each class and the conditional probabilities for each class should be equal) or a list as long
#'   as the number of classes containing one vector of probabilities for every class. The length of the \eqn{k}-th element is the desired 
#'   number of mixture components for the \eqn{k}-th class.
#' @param mu11 Class center of first class, a vector.
#' @param mu12 Class center of first class, a vector.
#' @param mu13 Class center of first class, a vector.
#' @param mu21 Class center of second class, a vector.
#' @param mu22 Class center of second class, a vector.
#' @param mu23 Class center of second class, a vector.
#' @param mu31 Class center of third class, a vector.
#' @param mu32 Class center of third class, a vector.
#' @param mu33 Class center of third class, a vector.
#' @param sigma Covariance matrix for classes 1, 2, and 3. 
#' @param data A \code{data.frame}.
#'
#' @return
#' \code{xor3Data} returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#'
#'
#' @aliases xor3Data xor3Labels xor3Posterior xor3BayesClass
#'
#' @rdname xor3Data
#'
#' @import mvtnorm
#'
#' @export

xor3Data <- function(n, prior = rep(1/3,3), lambda = rep(1/3,3), mu11 = c(-4,4), mu12 = c(0,-4), mu13 = c(4,0), mu21 = c(-4,0), mu22 = c(0,4), mu23 = c(4,-4),
	mu31 = c(-4,-4), mu32 = c(0,0), mu33 = c(4,4), sigma = diag(2)) {
	mu <- list(rbind(mu11,mu12,mu13), rbind(mu21, mu22,mu23), rbind(mu31,mu32,mu33))
	sigma <- list(sigma, sigma, sigma)
	data <- mixtureData(n, prior, lambda, mu, sigma)
	attr(data, "class") <- c("locClass.xor3Data", attr(data, "class"))
	return(data)
}



#' @return \code{xor3Labels} returns a factor of class labels.
#'
#' @rdname xor3Data
#'
#' @export

xor3Labels <- function(data, prior = rep(1/3,3), lambda = rep(1/3,3), mu11 = c(-4,4), mu12 = c(0,-4), mu13 = c(4,0), mu21 = c(-4,0), mu22 = c(0,4), mu23 = c(4,-4),
	mu31 = c(-4,-4), mu32 = c(0,0), mu33 = c(4,4), sigma = diag(2)) {
	mu <- list(rbind(mu11,mu12,mu13), rbind(mu21, mu22,mu23), rbind(mu31,mu32,mu33))
	sigma <- list(sigma, sigma, sigma)
	return(mixtureLabels(data, prior, lambda, mu, sigma))
}	



#' @return \code{xor3Posterior} returns a matrix of posterior probabilities.
#'
#' @rdname xor3Data
#'
#' @export

xor3Posterior <- function(data, prior = rep(1/3,3), lambda = rep(1/3,3), mu11 = c(-4,4), mu12 = c(0,-4), mu13 = c(4,0), mu21 = c(-4,0), mu22 = c(0,4), mu23 = c(4,-4),
	mu31 = c(-4,-4), mu32 = c(0,0), mu33 = c(4,4), sigma = diag(2)) {
	mu <- list(rbind(mu11,mu12,mu13), rbind(mu21, mu22,mu23), rbind(mu31,mu32,mu33))
	sigma <- list(sigma, sigma, sigma)
	return(mixturePosterior(data, prior, lambda, mu, sigma))
}



#' @return \code{xor3BayesClass} returns a factor of Bayes predictions.
#'
#' @rdname xor3Data
#'
#' @export

xor3BayesClass <- function(data, prior = rep(1/3,3), lambda = rep(1/3,3), mu11 = c(-4,4), mu12 = c(0,-4), mu13 = c(4,0), mu21 = c(-4,0), mu22 = c(0,4), mu23 = c(4,-4),
	mu31 = c(-4,-4), mu32 = c(0,0), mu33 = c(4,4), sigma = diag(2)) {
	mu <- list(rbind(mu11,mu12,mu13), rbind(mu21, mu22,mu23), rbind(mu31,mu32,mu33))
	sigma <- list(sigma, sigma, sigma)	
	return(mixtureBayesClass(data, prior, lambda, mu, sigma))
}
