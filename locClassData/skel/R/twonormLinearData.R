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

#' Generate a binary classification problem with two Gaussian distributions with different means and equal covariance matrices.
#'
# details
#'
#' @title Generate a Binary Classification Problem with Two Gaussian Distributions
#' 
#' @param n Number of observations.
#' @param prior Vector of class prior probabilities.
#' @param mu1 Class center of first class, a vector.
#' @param mu2 Class center of second class, a vector.
#' @param sigma Covariance matrix for classes 1 and 2. 
#' @param data A \code{data.frame}.
#'
#' @return
#' \code{twonormLinearData} returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#'
#'
#' @aliases twonormLinearData twonormLinearLabels twonormLinearPosterior twonormLinearBayesClass
#'
#' @rdname twonormLinearData
#'
#' @import mvtnorm
#'
#' @export

twonormLinearData <- function(n, prior = rep(0.5,2), mu1 = c(1,0), mu2 = c(0,-1), sigma = diag(2)) {
	mu <- list(matrix(mu1,1), matrix(mu2,1))
	data <- mixtureData(n, prior, lambda = list(1,1), mu, sigma)
	class(data) <- c("locClass.twonormLinearData", class(data))
	return(data)
}



#' @return \code{twonormLinearLabels} returns a factor of class labels.
#'
#' @rdname twonormLinearData
#'
#' @export

twonormLinearLabels <- function(data, prior = rep(0.5,2), mu1 = c(1,0), mu2 = c(0,-1), sigma = diag(2)) {
	mu <- list(matrix(mu1,1), matrix(mu2,1))
	return(mixtureLabels(data, prior, lambda = list(1,1), mu, sigma))
}	



#' @return \code{twonormLinearPosterior} returns a matrix of posterior probabilities.
#'
#' @rdname twonormLinearData
#'
#' @export

twonormLinearPosterior <- function(data, prior = rep(0.5,2), mu1 = c(1,0), mu2 = c(0,-1), sigma = diag(2)) {
	mu <- list(matrix(mu1,1), matrix(mu2,1))
	return(mixturePosterior(data, prior, lambda = list(1,1), mu, sigma))
}



#' @return \code{twonormLinearBayesClass} returns a factor of Bayes predictions.
#'
#' @rdname twonormLinearData
#'
#' @export

twonormLinearBayesClass <- function(data, prior = rep(0.5,2), mu1 = c(1,0), mu2 = c(0,-1), sigma = diag(2)) {
	mu <- list(matrix(mu1,1), matrix(mu2,1))
	return(mixtureBayesClass(data, prior, lambda = list(1,1), mu, sigma))
}
