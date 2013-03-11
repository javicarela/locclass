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

#' Generation of a large variety of classification problems.
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
#' This distribution is used for contamination, i.e. to change the class conditional distributions while the class posteriors remain unchanged.
#' The \code{centersOther} mixture component centers are drawn from a uniform distribution on [-1,1]^\code{d}.
#' The covariance matrix is always chosen as \code{sigmaOther} times the identity matrix where \code{sigmaOther} defaults to 0.5. 
#' The second Gaussian mixture distribution is only used to generate additional realizations of the explanatory 
#' variables. The class labels are sampled according to the posteriors determined from the first mixture distribution.
#'
# This function produces data in a two-stage procedure: First, distribution parameters are determined, second, the data are sampled according to
# the distribution parameters. If the argument \code{paramSeed} is given, the actual state of the RNG is saved?, then the distribution parameters 
# are determined according to the \code{paramSeed}, then we return to the previously saved state of the RNG and sample the data.
# Hence the argument \code{paramSeed} is useful if one wants to generate several data sets with the same distributions parameters.
#' 
#' @title Generate a Large Variety of Classification Problems
#'
#' @param n Number of observations.
#' @param probMix Probability that data come from the first Gaussian mixture distribution.
#' @param centersMix Number of mixture components.
#' @param sigmaMix Variance of class-conditional distribution. Either a scalar or a vector as long as the number of classes. Defaults to 0.2.
#' @param centersOther Number of mixture components.
#' @param sigmaOther Variance. Defaults to 0.5.
#' @param d Dimensionality.
#' @param propUseless Proportion of variables useless for separating the classes.
#' @param prior Vector of class prior probabilities.
#'
#' @return
#' \code{flexibleDataParams} returns a list with components:
#' \item{prior}{The class prior probabilities.}
#' \item{K}{The number of classes.}
#' \item{d}{Dimensionality.}
#' \item{dUseless}{The number of irrelevant variables that do not separate the classes.}
#' \item{probMix}{The number of observations drawn from the first distribution.}
#' \item{nMix}{The number of observations drawn from the first distribution.}
#' \item{muMix}{A \code{list} as long as the number of classes containing the mixture component centers.}
#' \item{sigmaMix}{The \code{sigmaMix}-argument.}
#' \item{lambdaMix}{A \code{list} as long as the number of classes containing the conditional probabilities for the mixture components given the class.}
#' \item{nOther}{The number of observations drawn from the second distribution.}
#' Optionally, if \code{probMix} is smaller than 1:
#' \item{muOther}{A \code{matrix} of mixture component centers.}
#' \item{sigmaOther}{The \code{sigmaOther}-argument}
#\item{lambdaOther}{A \code{vector} containing the probabilities of the mixture components.}
#'
#' \code{flexibleData} returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#' Additionally, several attributes that contain information about the distribution parameters
#'
#' @aliases flexibleDataParams flexibleData flexibleData.default flexibleData.list flexibleLabels flexibleBayesClass flexiblePosterior
#'
#' @rdname flexibleData
#'
#' @name flexibleData
#'
#' @import mvtnorm
#'
#' @export flexibleDataParams


# FIXME: 0 for useless? useless for localities and useless for classes?
# FIXME: how determine class labels of muMix?

flexibleDataParams <- function(n, probMix, centersMix, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d, propUseless, prior) {
	## argument checks
	if (n <= 0)
		stop("'n' must be positive")
    if (abs(n - round(n)) > .Machine$double.eps^0.5)
		warning("'n' should be a natural number and is rounded off")
	if (probMix < 0 || probMix > 1)
		stop("'probMix' must lie between 0 and 1")
	if (centersMix < 0)
		stop ("'centersMix' must be positive")
    if (abs(centersMix - round(centersMix)) > .Machine$double.eps^0.5)
		warning("'centersMix' should be a natural number and is rounded off")
	if (centersOther < 0)
		stop ("'centersOther' must be positive")
    if (abs(centersOther - round(centersOther)) > .Machine$double.eps^0.5)
		warning("'centersOther' should be a natural number and is rounded off")
	if (sigmaOther <= 0)
		stop("'sigmaOther' must be positive")
	if (d <= 0)
		stop("'d' must be positive")
	if (abs(d - round(d)) > .Machine$double.eps^0.5)
		warning("'d' should be a natural number and is rounded off")
	if (propUseless < 0 || propUseless > 1)
		stop("'propUseless' must lie between 0 and 1")

	K <- length(prior)										# number of classes
	if (any(prior < 0) || any(prior > 1))
		stop("'prior' probabilities must lie between 0 and 1")
	if (sum(prior) != 1) {
		warning("elements of 'prior' do not sum to 1")
		prior <- prior/sum(prior)
	}
	Ks <- length(sigmaMix)
	if (Ks > 1) {
		if (K != Ks)
			stop("'length(prior)' and 'length(sigmaMix)' must be equal")
		if (any(sigmaMix <= 0))
			stop("all elements of 'sigmaMix' must be positive")
	} else {
		if (sigmaMix <= 0)
			stop("'sigmaMix' must be positive")
		sigmaMix <- rep(sigmaMix, K)
	}
	
	## determine distribution parameters - fixed
	classes <- numeric(centersMix)							# vector for class labels of mixture centers
	classes[1:K] <- 1:K										# first K centers get labels 1:K
	dUseless <- floor(d * propUseless)	# number of useless variables, floor() guarantees that there is at least one useful variable if propUseless < 1
	dUseful <- d - dUseless									# number of useful variables

	## determine distribution parameters - stochastic
	nMix <- rbinom(1, size = n, prob = probMix)				# number of observations from first distribution
	nOther <- n - nMix										# number of observations from second distribution
	muMix <- matrix(0, centersMix, d)						# centers for first distribution
	if (dUseful > 0) {										# determine entries of muMix, otherwise all entries of muMix stay zero
		muMix[, 1:dUseful] <- runif(centersMix*dUseful, -0.5, 0.5)		# sample centers from uniform distribution on [-0.5,0.5]
		if (centersMix > K) {						# the first K centers get labels 1:K; if centersMix > K the remaining centers have to be labeled
			## centers near to each other get a higher probability to belong to the same class
			prob <- matrix(sapply(1:K, function(k) dmvnorm(muMix[(K+1):centersMix, 1:dUseful, drop = FALSE], mean = muMix[k, 1:dUseful, drop = FALSE])), ncol = K)
			prob <- prior * prob/rowSums(prior * prob)
			classes[(K+1):centersMix] <- apply(prob, 1, function(x) sample(1:K, size = 1, prob = x))			
## FIXME alternative: sample, entspricht gleichverteilung, je größer man die varianz beim nv-ansatz wählt, desto näher kommt man der gleichverteilung...
## neuer parameter der funktion? im vp?
#			classes[(K+1):centersMix] <- sample(1:K, size = centersMix-K, replace = TRUE, prob = prior)
		}
	} else {												# if dUseful = 0 the class labels of the mixture components are sampled randomly
		if (centersMix > K)
			classes[(K+1):centersMix] <- sample(1:K, size = centersMix - K, replace = TRUE)
	} ## FIXME: does not make much sense because all centers are zero
	muMix <- lapply(1:K, function(k) muMix[classes == k, , drop = FALSE])	# split muMix according to class labels of centers
	## function that generates random numbers from Dirichlet distribution
	f <- function(x) {
		u <- runif(nrow(x), 0, 1)
		u <- qexp(u)
		return(u/sum(u))
	}
	lambdaMix <- lapply(muMix, f)							# draw lambdaMix from Dirichlet distribution
# print(lambdaMix)

	if (probMix < 1) {										# determine parameters of second distribution and generate data
		muOther <- matrix(0, centersOther, d)
		nc <- floor(centersOther * 0.5)
		## FIXME: centersOther = 1
		muOther[1:nc,] <- matrix(runif(nc*d, -0.25, 0.25), nc, d)
		muOther[(nc+1):centersOther,] <- matrix(runif((centersOther-nc)*d, -1, 1), centersOther-nc, d)		
		return(list(prior = prior, K = K, d = d, dUseless = dUseless, probMix = probMix, nMix = nMix, muMix = muMix, sigmaMix = sigmaMix, lambdaMix = lambdaMix, nOther = nOther, muOther = muOther, sigmaOther = sigmaOther))
	} else {
		return(list(prior = prior, K = K, d = d, dUseless = dUseless, probMix = probMix, nMix = nMix, muMix = muMix, sigmaMix = sigmaMix, lambdaMix = lambdaMix, nOther = nOther))
	}
}



#' @param p Object where the method dispatch is based on. Either a \code{list} that contains the distribution parameters or a vector of class prior probabilities.
#' @param \dots Further arguments.
#'
#' @rdname flexibleData
#' @export

flexibleData <- function(p, ...) {
	UseMethod("flexibleData")
}



#' @method flexibleData list
#' @S3method flexibleData list
#'
#' @rdname flexibleData

flexibleData.list <- function(p, ...) {
	data <- flexibleData.default(p$prior, p$nMix, p$muMix, p$sigmaMix, p$lambdaMix, p$nOther, p$muOther, p$sigmaOther)
	attributes(data) <- c(list(names = names(data), class = class(data)), p)
	return(data)
}



#' @param nMix The number of observations drawn from the first distribution.
#' @param muMix A \code{list} as long as the number of classes containing the mixture component centers.
#' @param lambdaMix A \code{list} as long as the number of classes containing the conditional probabilities for the mixture components given the class.
#' @param nOther The number of observations drawn from the second distribution.
#' @param muOther (Optional, required if \code{nOther} is positive.) A \code{matrix} of mixture component centers.
#'
#' @method flexibleData default
#' @S3method flexibleData default
#'
#' @rdname flexibleData

flexibleData.default <- function(p, nMix, muMix, sigmaMix, lambdaMix, nOther, muOther = NULL, sigmaOther = NULL, ...) {
	## check arguments
	K <- length(p)
	if (any(p < 0) || any(p > 1))
		stop("all elements of 'p' must lie between 0 and 1")
	if (sum(p) != 1) {
		warning("elements of 'p' do not sum to 1")
		p <- p/sum(p)
	}
	if (nMix < 0)
		stop("'nMix' must be positive")
    if (abs(nMix - round(nMix)) > .Machine$double.eps^0.5)
		warning("'nMix' should be a natural number and is rounded off")
	if (length(muMix) != K)
		stop("'length(p)' and 'length(muMix)' are not equal")
	d <- unique(lapply(muMix, ncol))
	if (length(d) > 1)
		stop("all elements of 'muMix' must have the same number of columns")

	Ks <- length(sigmaMix)
	if (Ks > 1) {
		if (K != Ks)
			stop("'length(p)' and 'length(sigmaMix)' must be equal")
		if (any(sigmaMix <= 0))
			stop("all elements of 'sigmaMix' must be positive")
	} else {
		if (sigmaMix <= 0)
			stop("'sigmaMix' must be positive")
		sigmaMix <- rep(sigmaMix, K)
	}

	if (nOther < 0)
		stop("'nOther' must be positive")
    if (abs(nOther - round(nOther)) > .Machine$double.eps^0.5)
		warning("'nOther' should be a natural number and is rounded off")
	
	## generate data
	if (nOther > 0) {
		if (is.null(muOther))
			stop("'muOther' is not specified")
		if (is.null(sigmaOther))
			stop("'sigmaOther' is not specified")
		if (ncol(muOther) != d)
			stop("'ncol(muOther)' and 'ncol(muMix[[1]])' are not equal")
		if (sigmaOther <= 0)
			stop("'sigmaOther' must be positive")
		centersOther <- nrow(muOther)
		lambdaOther <- rep(1/centersOther, centersOther)
		data <- flexibleDataHelper(p, K, d, nMix, muMix, sigmaMix, lambdaMix, nOther, muOther, sigmaOther, lambdaOther)		
	} else {
		data <- flexibleDataHelper(p, K, d, nMix, muMix, sigmaMix, lambdaMix, nOther)
	}

	## set class and attributes
	class(data) <- c("locClass.flexibleData", "locClass")
	attr(data, "prior") <- p
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
		mixData <- mixtureData(n = nMix, prior = prior, mu = muMix, sigma = lapply(sigmaMix, function(x) x*diag(d)), lambda = lambdaMix)
	}
	if (nOther > 0) {														# second distribution
		nOtherk <- as.vector(rmultinom(1, size = nOther, prob = prior))		# required number of observations in single classes
# print("nOtherk")
# print(nOtherk)
		otherDataPool <- otherData <- list()
		otherDataPool$x <- mixtureData(n = nOther*K, prior = 1, mu = list(muOther), sigma = sigmaOther*diag(d), lambda = lambdaOther)$x
		otherDataPool$y <- mixtureLabels(otherDataPool$x, prior = prior, mu = muMix, sigma = lapply(sigmaMix, function(x) x*diag(d)), lambda = lambdaMix)
		nk <- as.vector(table(otherDataPool$y))
# print("nk")
# print(nk)
		bayesClassOther <- mixturePosterior(muOther, prior = prior, mu = muMix, sigma = lapply(sigmaMix, function(x) x*diag(d)), lambda = lambdaMix)
		z <- 0
		while (any(nk < nOtherk)) {		# if there are too few observations from at least one class draw more observations until there are enough
			z <- z + 1
			if (z > 25)
				stop("adequate sampling from second distribution not feasible")			
			maxk <- which(nk < nOtherk) # classes where observations are missing
# print(maxk)
			lambdaOther <- rowSums(bayesClassOther[, maxk, drop = FALSE])
# print(lambdaOther)
			lambdaOther <- lambdaOther/sum(lambdaOther)
			xnew <- mixtureData(n = nOther, prior = 1, mu = list(muOther), sigma = sigmaOther*diag(d), lambda = lambdaOther)$x
			ynew <- mixtureLabels(xnew, prior = prior, mu = muMix, sigma = lapply(sigmaMix, function(x) x*diag(d)), lambda = lambdaMix)
			otherDataPool$x <- rbind(otherDataPool$x, xnew)
			otherDataPool$y <- factor(c(otherDataPool$y, ynew), levels = 1:K)
			nk <- as.vector(table(otherDataPool$y))
# print("nk")
# print(nk)
# print("nOtherk - nk")
# print(nOtherk - nk)
		}
# print(z)
		indexk <- lapply(1:K, function(k) sample(nk[k], size = nOtherk[k]))
# print("indexk")		
# print(head(indexk))
		otherData$x <- do.call("rbind", lapply(1:K, function(k) otherDataPool$x[otherDataPool$y == k,, drop = FALSE][indexk[[k]],,drop=FALSE]))
# print("otherData$x")		
		otherData$y <- factor(rep(1:K, nOtherk), levels = 1:K) 
# print("otherData$y")		
	}
	data <- list()
	data$x <- rbind(mixData$x, otherData$x)
# print("data$x")		
	data$y <- factor(c(mixData$y, otherData$y), levels = 1:K)
# print("data$y")		
	return(data)
}



#' @param data A \code{data.frame}.
#'
#' @return \code{flexibleLabels} returns a \code{factor} of class labels.
#'
#' @rdname flexibleData
#'
#' @export

flexibleLabels <- function(data, p) {
	d <- ncol(data)
	nclass <- length(p$prior)
	posterior <- mixturePosterior(data, prior = p$prior, mu = p$muMix, sigma = lapply(p$sigmaMix, function(x) x*diag(d)), lambda = p$lambdaMix)
	y <- factor(apply(posterior, 1, function(x) sample(1:nclass, size = 1, prob = x)), levels = 1:nclass)
	return(y)
}	



#' @return \code{flexiblePosterior} returns a \code{matrix} of posterior probabilities.
#'
#' @rdname flexibleData
#'
#' @export

flexiblePosterior <- function(data, p) {
	d <- ncol(data)
	nclass <- length(p$prior)
	posterior <- mixturePosterior(data, prior = p$prior, mu = p$muMix, sigma = lapply(p$sigmaMix, function(x) x*diag(d)), lambda = p$lambdaMix)
    colnames(posterior) <- paste("posterior", 1:nclass, sep = ".")
	return(posterior)	
}



#' @return \code{flexibleBayesClass} returns a \code{factor} of Bayes predictions.
#'
#' @rdname flexibleData
#'
#' @export

flexibleBayesClass <- function(data, p) {
	d <- ncol(data)
	nclass <- length(p$prior)
	posterior <- mixturePosterior(data, prior = p$prior, mu = p$muMix, sigma = lapply(p$sigmaMix, function(x) x*diag(d)), lambda = p$lambdaMix)
	bayesclass <- factor(max.col(posterior), levels = 1:nclass)
	return(bayesclass)
}
