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

#' description
#'
#' details
#'
#' @title Generation of Two-Class Classification Problem with two Gaussian distributions
#' 
#' @param n Number of observations.
#' @param prior Vector of class prior probabilities.
#' @param mu1 Class center of first class, a vector.
#' @param mu2 Class center of second class, a vector.
#' @param sigma1 Covariance matrix for class 1. 
#' @param sigma2 Covariance matrix for class 2. 
#' @param data A \code{data.frame}.
#'
#' @return
#' returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#'
#'
#' @aliases twonormQuadraticData twonormQuadraticLabels twonormQuadraticPosterior twonormQuadraticBayesClass
#'
#' @rdname twonormQuadraticData
#'
#' @import mvtnorm
#'
#' @export

twonormQuadraticData <- function(n, prior = rep(0.5,2), mu1 = c(1,0), mu2 = c(0,-1), sigma1 = diag(2), sigma2 = matrix(c(1,0.5,0.5,1), 2)) {
	mu <- list(matrix(mu1,1), matrix(mu2,1))
	sigma <- list(sigma1, sigma2)
	data <- mixtureData(n, prior, lambda = list(1,1), mu, sigma)
	class(data) <- c("locClass.twonormQuadraticData", class(data))
	return(data)
}



#' @return returns a factor of class labels.
#'
#' @rdname twonormQuadraticData
#'
#' @export

twonormQuadraticLabels <- function(data, prior = rep(0.5,2), mu1 = c(1,0), mu2 = c(0,-1), sigma1 = diag(2), sigma2 = matrix(c(1,0.5,0.5,1), 2)) {
	mu <- list(matrix(mu1,1), matrix(mu2,1))
	sigma <- list(sigma1, sigma2)
	return(mixtureLabels(data, prior, lambda = list(1,1), mu, sigma))
}	



#' @return returns a matrix of posterior probabilities.
#'
#' @rdname twonormQuadraticData
#'
#' @export

twonormQuadraticPosterior <- function(data, prior = rep(0.5,2), mu1 = c(1,0), mu2 = c(0,-1), sigma1 = diag(2), sigma2 = matrix(c(1,0.5,0.5,1), 2)) {
	mu <- list(matrix(mu1,1), matrix(mu2,1))
	sigma <- list(sigma1, sigma2)
	return(mixturePosterior(data, prior, lambda = list(1,1), mu, sigma))
}



#' @return returns a factor of Bayes predictions.
#'
#' @rdname twonormQuadraticData
#'
#' @export

twonormQuadraticBayesClass <- function(data, prior = rep(0.5,2), mu1 = c(1,0), mu2 = c(0,-1), sigma1 = diag(2), sigma2 = matrix(c(1,0.5,0.5,1), 2)) {
	mu <- list(matrix(mu1,1), matrix(mu2,1))
	sigma <- list(sigma1, sigma2)
	return(mixtureBayesClass(data, prior, lambda = list(1,1), mu, sigma))
}
