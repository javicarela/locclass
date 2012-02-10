# Copyright (C) 2011 Julia Schiffner
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

#' Description
#'
#' Details
#'
#' @title Calculate Posterior Probabilities and Bayes Predictions 
#'
#' @param object An object of class \code{"locClass"}.
#' @param \dots Currently unused.
#'
#' @return
#' A list with components:
#' \item{posterior}{A matrix of posterior probabilities.}
#' \item{ybayes}{A vector of Bayes predictions.}
#'
#' @aliases bayes bayes.locClass.hvData bayes.locClass.mixtureData bayes.locClass.twonormLinearData
#'    bayes.locClass.twonormQuadraticData bayes.locClass.ringData bayes.locClass.xorData
#'    bayes.locClass.outlierCorrectData bayes.locClass.outlierWrongData bayes.locClass.vData
#'	  bayes.locClass.vNormalData
#'
#' @export

bayes <- function(object, ...)
	UseMethod("bayes")
	


#' @rdname bayes
#' @method bayes locClass.hvData
#'
#' @S3method bayes locClass.hvData

bayes.locClass.hvData <- function(object, ...) {
	if (!inherits(object, "locClass.hvData"))
		stop("'object' not of class \"locClass.hvData\"")
    x <- object$x
    k <- attr(object, "k")
    d <- ncol(x)
	posterior.2 <- k*x[,d]/t(c(rep(1,d-1),k) %*% t(x))
    posterior.1 <- 1 - posterior.2
    posterior <- cbind(posterior.1, posterior.2)
    ybayes <- factor(max.col(posterior), labels = as.character(1:2), levels = 1:2)
    return(list(ybayes = ybayes, posterior = posterior))
}



#' @rdname bayes
#' @method bayes locClass.mixtureData
#'
#' @S3method bayes locClass.mixtureData
#'
#' @import mvtnorm

bayes.locClass.mixtureData <- function(object, ...) {
	if (!inherits(object, "locClass.mixtureData"))
		stop("'object' not of class \"locClass.mixtureData\"")
    x <- object$x
	prior <- attr(object, "prior")
	lambda <- attr(object, "lambda")
    mu <- attr(object, "mu")
    sigma <- attr(object, "sigma")
	nclass <- length(prior)
	d <- ncol(x)
	# if (length(mu) != nclass)
		# stop("'length(mu)' is not 'nclass'")
	# d <- sapply(mu, ncol)
	# if (any(d[1] != d))
		# stop("numbers of columns for elements in 'mu' differs")
	# d <- d[1]										# dimensionality
	ncomp <- sapply(mu, nrow)						# # of mixture components per class	
	if (is.list(sigma)) {							# sigma list or list of lists
		if (length(sigma) != nclass)
			stop("'length(sigma)' and 'nclass' differ")
		if (is.list(lambda)) {						# lambda list
			if (length(lambda) != nclass)
				stop("'length(lambda)' is not 'nclass'")
			if (any(sapply(lambda, length) != ncomp))
				stop("length of 'lambda' and 'mu' does not match")
			dens <- sapply(1:nclass, function(k) prior[k] * mixturePosteriorHelper(x, lambda[[k]], mu[[k]], sigma[[k]]))
		} else if (is.vector(lambda)) {
			if (any(length(lambda) != ncomp))
				stop("length of 'lambda' and 'mu' does not match")
			dens <- sapply(1:nclass, function(k) prior[k] * mixturePosteriorHelper(x, lambda, mu[[k]], sigma[[k]]))
		} else {
			stop("'lambda' is neither list nor vector")
		}
	} else if (is.matrix(sigma)) {					# sigma matrix
		if (is.list(lambda)) {
			if (length(lambda) != nclass)
				stop("'length(lambda)' is not 'nclass'")
			if (any(sapply(lambda, length) != ncomp))
				stop("length of 'lambda' and 'mu' does not match")
			dens <- sapply(1:nclass, function(k) prior[k] * mixturePosteriorHelper(x, lambda[[k]], mu[[k]], sigma))
		} else if (is.vector(lambda)) {
			if (any(length(lambda) != ncomp))
				stop("length of 'lambda' and 'mu' does not match")
			dens <- sapply(1:nclass, function(k) prior[k] * mixturePosteriorHelper(x, lambda, mu[[k]], sigma))
		} else {
			stop("'lambda' is neither list nor vector")
		}
	}
	colnames(dens) <- paste("posterior", 1:nclass, sep = ".")
	rownames(dens) <- rownames(x)
	posterior <- dens/rowSums(dens)
	ybayes <- factor(max.col(posterior), levels = 1:nclass)
    return(list(ybayes = ybayes, posterior = posterior))
}



#' @rdname bayes
#' @method bayes locClass.twonormLinearData
#'
#' @S3method bayes locClass.twonormLinearData

bayes.locClass.twonormLinearData <- function(object, ...) {
	if (!inherits(object, "locClass.twonormLinearData"))
		stop("object not of class \"locClass.twonormLinearData\"")
	return(NextMethod(object, ...))
}



#' @rdname bayes
#' @method bayes locClass.twonormQuadraticData
#'
#' @S3method bayes locClass.twonormQuadraticData

bayes.locClass.twonormQuadraticData <- function(object, ...) {
	if (!inherits(object, "locClass.twonormQuadraticData"))
		stop("object not of class \"locClass.twonormQuadraticData\"")
	return(NextMethod(object, ...))
}



#' @rdname bayes
#' @method bayes locClass.ringData
#'
#' @S3method bayes locClass.ringData

bayes.locClass.ringData <- function(object, ...) {
	if (!inherits(object, "locClass.ringData"))
		stop("object not of class \"locClass.ringData\"")
	return(NextMethod(object, ...))
}



#' @rdname bayes
#' @method bayes locClass.xorData
#'
#' @S3method bayes locClass.xorData

bayes.locClass.xorData <- function(object, ...) {
	if (!inherits(object, "locClass.xorData"))
		stop("object not of class \"locClass.xorData\"")
	return(NextMethod(object, ...))
}



#' @rdname bayes
#' @method bayes locClass.outlierCorrectData
#'
#' @S3method bayes locClass.outlierCorrectData

bayes.locClass.outlierCorrectData <- function(object, ...) {
	if (!inherits(object, "locClass.outlierCorrectData"))
		stop("object not of class \"locClass.outlierCorrectData\"")
	return(NextMethod(object, ...))
}



#' @rdname bayes
#' @method bayes locClass.outlierWrongData
#'
#' @S3method bayes locClass.outlierWrongData

bayes.locClass.outlierWrongData <- function(object, ...) {
	if (!inherits(object, "locClass.outlierWrongData"))
		stop("object not of class \"locClass.outlierWrongData\"")
	return(NextMethod(object, ...))
}



#' @rdname bayes
#' @method bayes locClass.vData
#'
#' @S3method bayes locClass.vData

bayes.locClass.vData <- function(object, ...) {
	if (!inherits(object, "locClass.vData"))
		stop("object not of class \"locClass.vData\"")
    x <- object$x
    k <- attr(object, "k")
    d <- ncol(x)
	posterior.2 <- 0.5 + k * (data[,2] - 2 * abs(data[,1] - 0.5))
	posterior.2[posterior.2 < 0] <- 0 
	posterior.2[posterior.2 > 1] <- 1 
    posterior.1 <- 1 - posterior.2
    posterior <- cbind(posterior.1, posterior.2)
    ybayes <- factor(max.col(posterior), labels = as.character(1:2), levels = 1:2)
    return(list(ybayes = ybayes, posterior = posterior))
}



#' @rdname bayes
#' @method bayes locClass.vNormalData
#'
#' @S3method bayes locClass.vNormalData

bayes.locClass.vNormalData <- function(object, ...) {
	if (!inherits(object, "locClass.vNormalData"))
		stop("object not of class \"locClass.vNormalData\"")
	return(NextMethod(object, ...))
}