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

#' Generation of a binary classification problem with two Gaussian distributions with euqal means and different covariance matrices.
#'
# details
#'
#' @title Generation of a Binary Classification Problem with Two Gaussian Distributions
#' 
#' @param n Number of observations.
#' @param prior Vector of class prior probabilities.
#' @param sigma1 Covariance matrix for class 1. 
#' @param sigma2 Covariance matrix for class 2. 
#' @param data A \code{data.frame}.
#'
#' @return
#' \code{ringData} returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#'
#'
#' @aliases ringData ringLabels ringPosterior ringBayesClass
#'
#' @rdname ringData
#'
#' @import mvtnorm
#'
#' @export

ringData <- function(n, prior = rep(0.5,2), sigma1 = diag(2), sigma2 = 2*diag(2)) {
	mu <- list(matrix(rep(0,2),1), matrix(rep(0,2),1))
	sigma <- list(sigma1, sigma2)
	data <- mixtureData(n, prior, lambda = list(1,1), mu, sigma)
	class(data) <- c("locClass.ringData", class(data))
}



#' @return \code{ringLabels} returns a factor of class labels.
#'
#' @rdname ringData
#'
#' @export

ringLabels <- function(data, prior = rep(0.5,2), sigma1 = diag(2), sigma2 = 2*diag(2)) {
	mu <- list(matrix(rep(0,2),1), matrix(rep(0,2),1))
	sigma <- list(sigma1, sigma2)
	return(mixtureLabels(data, prior, lambda = list(1,1), mu, sigma))
}	



#' @return \code{ringPosterior} returns a matrix of posterior probabilities.
#'
#' @rdname ringData
#'
#' @export

ringPosterior <- function(data, prior = rep(0.5,2), sigma1 = diag(2), sigma2 = 2*diag(2)) {
	mu <- list(matrix(rep(0,2),1), matrix(rep(0,2),1))
	sigma <- list(sigma1, sigma2)
	return(mixturePosterior(data, prior, lambda = list(1,1), mu, sigma))
}



#' @return \code{ringBayesClass} returns a factor of Bayes predictions.
#'
#' @rdname ringData
#'
#' @export

ringBayesClass <- function(data, prior = rep(0.5,2), sigma1 = diag(2), sigma2 = 2*diag(2)) {
	mu <- list(matrix(rep(0,2),1), matrix(rep(0,2),1))
	sigma <- list(sigma1, sigma2)
	return(mixtureBayesClass(data, prior, lambda = list(1,1), mu, sigma))
}
