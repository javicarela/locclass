#  Copyright (C) 2012 Julia Schiffner
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

#' Generation a large variety of classification problems.
#'
#' \code{n} observations are drawn from two Gaussian mixture distributions. 
#' The first mixture distribution from which \code{rbinom(1, size = n, prob = probMix)} observations are drawn determines the class
#' posteriors and hence the class labels. The \code{centersMix} mixture component centers are drawn from a uniform
#' distribution on [-1,1]^\code{dUseful}. The centers corresponding to irrelevant variables are set to zero.
#' The covariance matrix is always chosen as \code{sigmaMix} times the identity matrix. \code{sigmaMix} defaults to 0.2.
# The class labels of the component centers are sampled according to the class \code{prior}s.
#' The conditional probabilities of the mixture components given the priors are chosen as constant.
#' The class labels are sampled according to the posterior probabilities determined from the selected parameters.
#' 
#' The remaining number of observations is drawn from the second Gaussian mixture distribution.
#' This distribution is used for disturbance.
# to prevent that a mixture based classification method is always optimal.
#' The \code{centersOther} mixture component centers are drawn from a uniform distribution on [-1,1]^\code{d}.
#' The covariance matrix is always chosen as \code{sigmaOther} times the identity matrix where \code{sigmaOther} defaults to 0.5. 
#' The second Gaussian mixture distribution is only used to generate additional realizations of the explanatory 
#' variables. The class labels are sampled according to the posteriors determined from the first mixture distribution.
#' 
#' @title Generate a Large Variety of Classification Problems
#'
#' @param n Number of observations.
#' @param probMix Probability for the first Gaussian mixture distribution.
#' @param centersMix Number of mixture components.
#' @param sigmaMix Variance. Defaults to 0.2.
#' @param centersOther Number of mixture components.
#' @param sigmaOther Variance. Defaults to 0.5.
#' @param d Dimensionality.
#' @param propUseless Proportion of variables useless for separating the classes.
#' @param prior Vector of class prior probabilities.
#' @param seed A seed to reproduce generated data. Defaults to NULL.
# @param data A \code{data.frame}.
#'
#' @return
#' \code{flexibleData} returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#' Additionally, several attributes that contain information about the distribution parameters:
#' \item{prior}{The class prior probabilities.}
#' \item{dUseless}{The number of irrelevant variables that do not separate the classes.}
#' \item{nMix}{The number of observations drawn from the first distribution.}
#' \item{muMix}{A \code{list} as long as the number of classes containing the mixture component centers.}
#' \item{sigmaMix}{The \code{sigmaMix}-argument.}
#' \item{lambdaMix}{A \code{list} as long as the number of classes containing the conditional probabilities for the mixture components given the class.}
#' \item{nOther}{The number of observations drawn from the second distribution.}
#' Optionally, if \code{nOther} is positive:
#' \item{muOther}{A \code{matrix} of mixture component centers.}
#' \item{sigmaOther}{The \code{sigmaOther}-argument}
#' \item{lambdaOther}{A \code{vector} containing the probabilities of the mixture components.}
#'
#'
#' @aliases flexibleData 
#flexibleLabels flexiblePosterior flexibleBayesClass
#'
#' @rdname flexibleData
#'
#' @import mvtnorm
#'
#' @export


# FIXME: 0 für useless? useless for localities and useless for classes?
# FIXME. seeds
flexibleData <- function(n, probMix, centersMix, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d, propUseless, prior, seed = NULL) {
	## determine distribution parameters
	K <- length(prior)										# number of classes
	classes <- numeric(centersMix)							# class labels of mixture centers
	classes[1:K] <- 1:K
	if (!is.null(seed)) set.seed(seed)						# seed for rbinom
	nMix <- rbinom(1, size = n, prob = probMix)				# number of observations from first distribution
	nOther <- n - nMix										# number of observations from first second distribution
	dUseless <- floor(d * propUseless)						# number of useless variables
	dUseful <- d - dUseless									# number of useful variables
	muMix <- matrix(0, centersMix, d)						# centers for first distribution
	if (dUseful > 0) {										# otherwise all entries of muMix stay zero
		if (!is.null(seed)) set.seed(seed+1)				# seed for runif
		muMix[, 1:dUseful] <- runif(centersMix*dUseful, -1, 1)		# sample centers from uniform distribution
		if (centersMix > K) {						# the first K centers get labels 1:K; if centersMix > K the remaining centers have to be labeled
			## centers near to each other get a higher probability to belong to the same class
			prob <- sapply(1:K, function(k) dmvnorm(muMix[(K+1):centersMix, 1:dUseful, drop = FALSE], mean = muMix[k, 1:dUseful, drop = FALSE]))
			prob <- prior * prob/rowSums(prior * prob)
			if (!is.null(seed)) set.seed(seed+2)			# seed for sample
			classes[(K+1):centersMix] <- apply(prob, 1, function(x) sample(1:K, size = 1, prob = x))
		}
	} else {												# if dUseful = 0 the class labels of the mixture components are sampled randomly
		if (centersMix > K)
			classes[(K+1):centersMix] <- sample(1:K, size = centersMix - K, replace = TRUE)
	} ## FIXME: does not make much sense
	muMix <- lapply(1:K, function(k) muMix[classes == k, , drop = FALSE])	
	lambdaMix <- lapply(muMix, function(x) return(rep(1/nrow(x), nrow(x))))
	if (nOther > 0) {
		if (!is.null(seed)) set.seed(seed+3)				# seed for runif
		muOther <- matrix(runif(centersOther*d, -1, 1), centersOther, d)
		lambdaOther <- rep(1/centersOther, centersOther)
		## generate data
		if (!is.null(seed)) set.seed(seed+4)				# seed for flexibleDataHelper
		data <- flexibleDataHelper(prior, K, d, nMix, muMix, sigmaMix, lambdaMix, nOther, muOther, sigmaOther, lambdaOther)		
	} else {
		## generate data
		if (!is.null(seed)) set.seed(seed+5)				# seed for flexibleDataHelper
		data <- flexibleDataHelper(prior, K, d, nMix, muMix, sigmaMix, lambdaMix, nOther)
	}
	## class and attributes
	class(data) <- c("locClass.flexibleData", "locClass")
	attr(data, "prior") <- prior
	attr(data, "dUseless") <- dUseless
	attr(data, "probMix") <- probMix	
	attr(data, "nMix") <- nMix
	attr(data, "muMix") <- muMix
	attr(data, "sigmaMix") <- sigmaMix
	attr(data, "lambdaMix") <- lambdaMix
	attr(data, "nOther") <- nOther
	if (nOther > 0) {
		attr(data, "muOther") <- muOther
		attr(data, "sigmaOther") <- sigmaOther
		attr(data, "lambdaOther") <- lambdaOther
	}
	return(data)
}



#' @noRd

flexibleDataHelper <- function(prior, K, d, nMix, muMix, sigmaMix, lambdaMix, nOther, muOther = NULL, sigmaOther = NULL, lambdaOther = NULL) {
	mixData <- otherData <- NULL											# initialize data sets
	if (nMix > 0) {															# first distribution
		mixData <- mixtureData(n = nMix, prior = prior, mu = muMix, sigma = sigmaMix*diag(d), lambda = lambdaMix)
	}
	if (nOther > 0) {														# second distribution
		nOtherk <- as.vector(rmultinom(1, size = nOther, prob = prior))		# required number of observations in single classes
		otherDataPool <- otherData <- list()
		otherDataPool$x <- mixtureData(n = nOther*K, prior = 1, mu = list(muOther), sigma = sigmaOther*diag(d), lambda = lambdaOther)$x
		otherDataPool$y <- mixtureLabels(otherDataPool$x, prior = prior, mu = muMix, sigma = sigmaMix*diag(d), lambda = lambdaMix)
#print(otherDataPool$y)
		nk <- as.vector(table(otherDataPool$y))
#print(nk)
#print(length(nk) == length(nOtherk))
		z <- 0
		while (any(nk < nOtherk)) {		# if there are too few observations from at least one class draw more observations until there are enough
			z <- z + 1
			xnew <- mixtureData(n = nOther, prior = 1, mu = list(muOther), sigma = sigmaOther*diag(d), lambda = lambdaOther)$x
			ynew <- mixtureLabels(xnew, prior = prior, mu = muMix, sigma = sigmaMix*diag(d), lambda = lambdaMix)
			otherDataPool$x <- rbind(otherDataPool$x, xnew)
			otherDataPool$y <- c(otherDataPool$y, ynew)
			nk <- as.vector(table(otherDataPool$y))
			if (z == 20) stop("adequate sampling from second distribution not feasible")			
		}
#print(z)
		indexk <- sapply(1:K, function(k) sample(nk[k], size = nOtherk[k]))		
		otherData$x <- do.call("rbind", lapply(1:K, function(k) otherDataPool$x[otherDataPool$y == k,, drop = FALSE][indexk[[k]],,drop=FALSE]))
		otherData$y <- factor(rep(1:K, nOtherk), levels = 1:K) 
	}
	data <- list()
	data$x <- rbind(mixData$x, otherData$x)
	data$y <- factor(c(mixData$y, otherData$y), levels = 1:K)
	return(data)
}
