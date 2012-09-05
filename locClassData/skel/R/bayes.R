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

#' Calculate posterior probabilities and Bayes predictions.
#'
# Details
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
#' \item{noise}{The estimated Bayes error.}
#'
#' @aliases bayes 
#'		bayes.locClass.flashData 
#'		bayes.locClass.flexibleData
#'		bayes.locClass.hvData 
#'		bayes.locClass.hvQuadraticData 
#'		bayes.locClass.mixtureData
#'		bayes.locClass.outlierCorrectData
#'		bayes.locClass.outlierWrongData
#'		bayes.locClass.ringData
#'		bayes.locClass.spiralData
#'		bayes.locClass.twonormLinearData
#'		bayes.locClass.twonormQuadraticData  
#'		bayes.locClass.vData
#'		bayes.locClass.vNormalLinearData
#'		bayes.locClass.vNormalQuadraticData
#'		bayes.locClass.wData
#'		bayes.locClass.xorData
#'		bayes.locClass.xor3Data
#'
#' @export

bayes <- function(object, ...)
	UseMethod("bayes")
	


#' @rdname bayes
#' @method bayes locClass.flashData
#'
#' @S3method bayes locClass.flashData

bayes.locClass.flashData <- function(object, ...) {
	if (!inherits(object, "locClass.flashData"))
		stop("object not of class \"locClass.flashData\"")
	return(NextMethod(object, ...))
}



#' @rdname bayes
#' @method bayes locClass.flexibleData
#'
#' @S3method bayes locClass.flexibleData

bayes.locClass.flexibleData <- function(object, ...) {
	if (!inherits(object, "locClass.flexibleData"))
		stop("object not of class \"locClass.flexibleData\"")
	d <- ncol(object$x)
	n <- nrow(object$x)
	posterior <- mixturePosterior(object$x, prior = attr(object, "prior"), mu = attr(object, "muMix"), sigma = attr(object, "sigmaMix")*diag(d), lambda = attr(object, "lambdaMix"))
	nclass <- length(attr(object, "prior"))
	ybayes <- factor(max.col(posterior), levels = 1:nclass)
	noise <- mean(1 - posterior[cbind(1:n, ybayes)])
    return(list(ybayes = ybayes, posterior = posterior, noise = noise))
}



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
    n <- nrow(x)
	posterior <- k*x[,d]/t(c(rep(1,d-1),k) %*% t(x))
    posterior <- cbind(1 - posterior, posterior)
    colnames(posterior) <- paste("posterior", 1:2, sep = ".")
	ybayes <- factor(max.col(posterior), labels = as.character(1:2), levels = 1:2)
	noise <- mean(1 - posterior[cbind(1:n, ybayes)])
    return(list(ybayes = ybayes, posterior = posterior, noise = noise))
}



#' @rdname bayes
#' @method bayes locClass.hvQuadraticData
#'
#' @S3method bayes locClass.hvQuadraticData

bayes.locClass.hvQuadraticData <- function(object, ...) {
	if (!inherits(object, "locClass.hvQuadraticData"))
		stop("'object' not of class \"locClass.hvQuadraticData\"")
    x <- object$x
    k <- attr(object, "k")
    d <- ncol(x)
    n <- nrow(x)
    x[,d] <- x[,d]^2
	posterior <- k*x[,d]/t(rep(k,d) %*% t(x))
    posterior <- cbind(1 - posterior, posterior)
    colnames(posterior) <- paste("posterior", 1:2, sep = ".")
	ybayes <- factor(max.col(posterior), labels = as.character(1:2), levels = 1:2)
	noise <- mean(1 - posterior[cbind(1:n, ybayes)])
    return(list(ybayes = ybayes, posterior = posterior, noise = noise))
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
	n <- nrow(x)
	# if (length(mu) != nclass)
		# stop("'length(mu)' is not 'nclass'")
	# d <- sapply(mu, ncol)
	# if (any(d[1] != d))
		# stop("numbers of columns for elements in 'mu' differs")
	# d <- d[1]										# dimensionality
	ncomp <- sapply(mu, nrow)						# # of mixture components per class	
	if (is.list(sigma)) {							# sigma list or list of lists
		if (length(sigma) != nclass)
			stop("'length(sigma)' and 'length(prior)' differ")
		if (is.list(lambda)) {						# lambda list
			if (length(lambda) != nclass)
				stop("'length(lambda)' and 'length(prior)' do not match'")
			if (any(sapply(lambda, length) != ncomp))
				stop("number of mixture components in 'lambda' and 'mu' do not match")
			dens <- matrix(sapply(1:nclass, function(k) prior[k] * mixturePosteriorHelper(x, lambda[[k]], mu[[k]], sigma[[k]])), ncol = nclass)
		} else if (is.vector(lambda)) {
			if (any(length(lambda) != ncomp))
				stop("number of mixture components in 'lambda' and 'mu' do not match")
			dens <- matrix(sapply(1:nclass, function(k) prior[k] * mixturePosteriorHelper(x, lambda, mu[[k]], sigma[[k]])), ncol = nclass)
		} else {
			stop("'lambda' is neither list nor vector")
		}
	} else if (is.matrix(sigma)) {					# sigma matrix
		if (is.list(lambda)) {
			if (length(lambda) != nclass)
				stop("'length(lambda)' and 'length(prior)' do not match")
			if (any(sapply(lambda, length) != ncomp))
				stop("number of mixture components in 'lambda' and 'mu' do not match")
			dens <- matrix(sapply(1:nclass, function(k) prior[k] * mixturePosteriorHelper(x, lambda[[k]], mu[[k]], sigma)), ncol = nclass)
		} else if (is.vector(lambda)) {
			if (any(length(lambda) != ncomp))
				stop("number of mixture components in 'lambda' and 'mu' do not match")
			dens <- matrix(sapply(1:nclass, function(k) prior[k] * mixturePosteriorHelper(x, lambda, mu[[k]], sigma)), ncol = nclass)
		} else {
			stop("'lambda' is neither list nor vector")
		}
	} else {
		stop("'sigma' is neither list nor matrix")
	}
	colnames(dens) <- paste("posterior", 1:nclass, sep = ".")
	rownames(dens) <- rownames(x)
	posterior <- dens/rowSums(dens)
	ybayes <- factor(max.col(posterior), levels = 1:nclass)
	noise <- mean(1 - posterior[cbind(1:n, ybayes)])
    return(list(ybayes = ybayes, posterior = posterior, noise = noise))
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
#' @method bayes locClass.ringData
#'
#' @S3method bayes locClass.ringData

bayes.locClass.ringData <- function(object, ...) {
	if (!inherits(object, "locClass.ringData"))
		stop("object not of class \"locClass.ringData\"")
	return(NextMethod(object, ...))
}



#' @rdname bayes
#' @method bayes locClass.spiralData
#'
#' @S3method bayes locClass.spiralData

bayes.locClass.spiralData <- function(object, ...) {
	if (!inherits(object, "locClass.spiralData"))
		stop("'object' not of class \"locClass.spiralData\"")
    x <- object$x
    n <- nrow(x)
    cycles <- attr(object, "cycles")
	sp <- mlbench:::mlbench.1spiral(n = 1000, cycles = cycles, sd = 0)
	# center spiral at 0
	sp[,1] <- sp[,1] - 1/6
	sp[,2] <- sp[,2] + 1/6
	posterior <- apply(x, 1, function(z) min(sqrt(colSums((t(sp) - z)^2))))
	posterior[posterior > 1/3] <- 1/3
	posterior <- 3 * posterior
    posterior <- cbind(1 - posterior, posterior)
    colnames(posterior) <- paste("posterior", 1:2, sep = ".")
    ybayes <- factor(max.col(posterior), labels = as.character(1:2), levels = 1:2)
	noise <- mean(1 - posterior[cbind(1:n, ybayes)])
    return(list(ybayes = ybayes, posterior = posterior, noise = noise))
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
#' @method bayes locClass.vData
#'
#' @S3method bayes locClass.vData

bayes.locClass.vData <- function(object, ...) {
	if (!inherits(object, "locClass.vData"))
		stop("object not of class \"locClass.vData\"")
    x <- object$x
    k <- attr(object, "k")
    d <- ncol(x)
    n <- nrow(x)
	posterior <- 0.5 + k * (x[,2] - 2 * abs(x[,1] - 0.5))
	posterior[posterior < 0] <- 0 
	posterior[posterior > 1] <- 1 
    posterior <- cbind(1 - posterior, posterior)
    colnames(posterior) <- paste("posterior", 1:2, sep = ".")
    ybayes <- factor(max.col(posterior), labels = as.character(1:2), levels = 1:2)
	noise <- mean(1 - posterior[cbind(1:n, ybayes)])
    return(list(ybayes = ybayes, posterior = posterior, noise = noise))
}



#' @rdname bayes
#' @method bayes locClass.vNormalLinearData
#'
#' @S3method bayes locClass.vNormalLinearData

bayes.locClass.vNormalLinearData <- function(object, ...) {
	if (!inherits(object, "locClass.vNormalLinearData"))
		stop("object not of class \"locClass.vNormalLinearData\"")
	return(NextMethod(object, ...))
}



#' @rdname bayes
#' @method bayes locClass.vNormalQuadraticData
#'
#' @S3method bayes locClass.vNormalQuadraticData

bayes.locClass.vNormalQuadraticData <- function(object, ...) {
	if (!inherits(object, "locClass.vNormalQuadraticData"))
		stop("object not of class \"locClass.vNormalQuadraticData\"")
	return(NextMethod(object, ...))
}



#' @rdname bayes
#' @method bayes locClass.wData
#'
#' @S3method bayes locClass.wData

bayes.locClass.wData <- function(object, ...) {
	if (!inherits(object, "locClass.wData"))
		stop("object not of class \"locClass.wData\"")
    x <- object$x
    k <- attr(object, "k")
    d <- ncol(x)
    n <- nrow(x)
	posterior <- 0.5 + k * (x[,2] - 2 * abs(x[,1] %% 1 - 0.5))
	posterior[posterior < 0] <- 0 
	posterior[posterior > 1] <- 1 
    posterior <- cbind(1 - posterior, posterior)
    ybayes <- factor(max.col(posterior), labels = as.character(1:2), levels = 1:2)
	noise <- mean(1 - posterior[cbind(1:n, ybayes)])
    return(list(ybayes = ybayes, posterior = posterior, noise = noise))
}



#' @rdname bayes
#' @method bayes locClass.xor3Data
#'
#' @S3method bayes locClass.xor3Data

bayes.locClass.xor3Data <- function(object, ...) {
	if (!inherits(object, "locClass.xor3Data"))
		stop("object not of class \"locClass.xor3Data\"")
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
