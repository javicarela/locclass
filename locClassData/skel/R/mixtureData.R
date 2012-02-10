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
#' @title Generation of Gaussian Mixture Data for Classification
#' 
#' @param n Number of observations.
#' @param prior Vector of class prior probabilities.
#' @param lambda The conditional probabilities for the mixture components given the class. Either a vector (if the same number \eqn{m} 
#'   of mixture components is desired for each class and the conditional probabilities for each class should be equal) or a list as long
#'   as the number of classes containing one vector of probabilities for every class. The length of the \eqn{k}-th element is the desired 
#'   number of mixture components for the \eqn{k}-th class.
#' @param mu The centers of the mixture components. A list containing one \eqn{m_k \times d}{m_k x d} matrix of centers for
#'   each class where \eqn{d} is the desired dimensionality of the data set.
#' @param sigma The covariance matrices of the mixture components. Either one single matrix that is used for each mixture
#'   component or a list as long as the number of classes. List elements can be matrices (in case that for all mixture components forming
#'   one class the same covariance matrix shall be used) or lists of matrices as long as the number of mixture components in the corresponding 
#'   class.
#' @param data A \code{data.frame}.
#'
#' @return
#' returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#'
#' @examples 
#' ## Simplest case: 
#' # lambda vector, sigma matrix
#' # Generate a training and a test set
#' mu <- list(matrix(c(-1,-1),1), matrix(rep(c(1,4.5,12),2),3))
#' train <- mixtureData(n = 1000, prior = rep(0.5,2), lambda = list(1, rep(1/3,3)), mu = mu, sigma = 3*diag(2))
#' test <- mixtureData(n = 1000, prior = rep(0.5,2), lambda = list(1, rep(1/3,3)), mu = mu, sigma = 3*diag(2))
#'  
#' # Generate a grid of points
#' x.1 <- x.2 <- seq(-7,15,0.1)
#' grid <- expand.grid(x.1 = x.1, x.2 = x.2)
#'
#' # Calculate the posterior probablities for all grid points
#' gridPosterior <- mixturePosterior(grid, prior = rep(0.5,2), lambda = list(1, rep(1/3,3)), mu = mu, sigma = 3*diag(2))
#'
#' # Draw contour lines of posterior probabilities and plot training observations
#' contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray")
#' points(train$x, col = train$y)
#'
#' # Calculate Bayes error
#' ybayes <- mixtureBayesClass(test$x, prior = rep(0.5,2), lambda = list(1, rep(1/3,3)), mu = mu, sigma = 3*diag(2))
#' mean(ybayes != test$y)
#'
#' if (require(MASS)) {
#'	
#' 	   # Fit an LDA model and calculate misclassification rate on the test data set
#'     tr <- lda(y ~ ., data = as.data.frame(train))	
#'     pred <- predict(tr, as.data.frame(test))	
#'     mean(pred$class != test$y)
#' 
#'     # Draw decision boundary
#'     gridPred <- predict(tr, grid)
#'     contour(x.1, x.2, matrix(gridPred$posterior[,1], length(x.1)), col = "red", levels = 0.5, add = TRUE)
#'
#' }
#'
#' ## lambda list, sigma list of lists of matrices
#' mu <- list()
#' mu[[1]] <- matrix(c(1,5,1,5),2)
#' mu[[2]] <- matrix(c(8,11,8,11),2)
#' lambda <- list(c(0.5, 0.5), c(0.1, 0.9))
#' sigma <- list()
#' sigma[[1]] <- diag(2)
#' sigma[[2]] <- list(diag(2), 3*diag(2))
#' data <- mixtureData(n = 100, prior = c(0.3, 0.7), lambda, mu, sigma)
#' plot(data$x, col = data$y)
#'
#' @aliases mixtureData mixtureLabels mixturePosterior mixtureBayesClass
#'
#' @rdname mixtureData
#'
#' @import mvtnorm
#'
#' @export

mixtureData <- function(n, prior, lambda, mu, sigma) {
	nclass <- length(prior)
	y <- factor(sample(1:nclass, size = n, replace = TRUE, prob = prior), levels = 1:nclass)	# class labels
    ncl <- as.vector(table(y))																	# # observations per class
	if (length(mu) != nclass)
		stop("'length(mu)' is not 'nclass'")
	d <- sapply(mu, ncol)
	if (any(d[1] != d))
		stop("numbers of columns for elements in 'mu' differs")
	d <- d[1]										# dimensionality
	ncomp <- sapply(mu, nrow)						# # of mixture components per class	
	x <- matrix(0, n, d)
	if (is.list(sigma)) {							# sigma list or list of lists
		if (length(sigma) != nclass)
			stop("'length(sigma)' and 'nclass' differ")
		if (is.list(lambda)) {						# lambda list
			if (length(lambda) != nclass)
				stop("'length(lambda)' is not 'nclass'")
			if (any(sapply(lambda, length) != ncomp))
				stop("length of 'lambda' and 'mu' does not match")
			for (k in 1:nclass)
				x[y == k,] <- mixtureDataHelper(ncl[k], lambda[[k]], mu[[k]], sigma[[k]])
		} else if (is.vector(lambda)) {
			if (any(length(lambda) != ncomp))
				stop("length of 'lambda' and 'mu' does not match")
			for (k in 1:nclass)
				x[y == k,] <- mixtureDataHelper(ncl[k], lambda, mu[[k]], sigma[[k]])
		} else {
			stop("'lambda' is neither list nor vector")
		}
	} else if (is.matrix(sigma)) {					# sigma matrix
		if (is.list(lambda)) {
			if (length(lambda) != nclass)
				stop("'length(lambda)' is not 'nclass'")
			if (any(sapply(lambda, length) != ncomp))
				stop("length of 'lambda' and 'mu' does not match")
			for (k in 1:nclass)
				x[y == k,] <- mixtureDataHelper(ncl[k], lambda[[k]], mu[[k]], sigma)
		} else if (is.vector(lambda)) {
			if (any(length(lambda) != ncomp))
				stop("length of 'lambda' and 'mu' does not match")
			for (k in 1:nclass)
				x[y == k,] <- mixtureDataHelper(ncl[k], lambda, mu[[k]], sigma)
		} else {
			stop("'lambda' is neither list nor vector")
		}
	}
	data <- list(x = x, y = y)
	class(data) <- c("locClass.mixtureData", "locClass")
	attr(data, "prior") <- prior
	attr(data, "lambda") <- lambda
	attr(data, "mu") <- mu
	attr(data, "sigma") <- sigma
	return(data)
}



#' @nord

mixtureDataHelper <- function(n_k, lambda_k, mu_k, sigma_k) {
	ncomp_k <- length(lambda_k)
	comp_k <- sample(1:ncomp_k, size = n_k, replace = TRUE, prob = lambda_k)
	tab <- as.vector(table(comp_k))
	d <- ncol(mu_k)
    x_k <- matrix(0, n_k, d)
    if (is.list(sigma_k)) {
    	if (length(sigma_k) != ncomp_k)
    		stop("lengths of 'lambda' and 'sigma' do not fit")
    	if (any(as.vector(sapply(sigma_k, dim)) != d))
			stop("dimensionality of elements in 'sigma' incorrect")
		for (i in 1:ncomp_k)
    		x_k[comp_k == i,] <- rmvnorm(tab[i], mu_k[i,], sigma_k[[i]])
	} else if (is.matrix(sigma_k)) {
		if (any(dim(sigma_k) != d))
			stop("dimensionality of 'sigma_k' incorrect")
		for (i in 1:ncomp_k)
    		x_k[comp_k == i,] <- rmvnorm(tab[i], mu_k[i,], sigma_k)
	} else
		stop("'sigma_k' is neither list nor matrix")
	return(x_k)    
}



#' @return returns a factor of class labels.
#'
#' @rdname mixtureData
#'
#' @export

mixtureLabels <- function(data, prior, lambda, mu, sigma) {
	nclass <- length(prior)
	posterior <- mixturePosterior(data, prior, lambda, mu, sigma)
	y <- factor(apply(posterior, 1, function(x) sample(1:nclass, size = 1, prob = x)), levels = 1:nclass)
	return(y)
}	



#' @return returns a matrix of posterior probabilities.
#'
#' @rdname mixtureData
#'
#' @export

mixturePosterior <- function(data, prior, lambda, mu, sigma) {
	nclass <- length(prior)
	if (length(mu) != nclass)
		stop("'length(mu)' is not 'nclass'")
	d <- sapply(mu, ncol)
	if (any(d[1] != d))
		stop("numbers of columns for elements in 'mu' differs")
	d <- d[1]										# dimensionality
	ncomp <- sapply(mu, nrow)						# # of mixture components per class	
	if (is.list(sigma)) {							# sigma list or list of lists
		if (length(sigma) != nclass)
			stop("'length(sigma)' and 'nclass' differ")
		if (is.list(lambda)) {						# lambda list
			if (length(lambda) != nclass)
				stop("'length(lambda)' is not 'nclass'")
			if (any(sapply(lambda, length) != ncomp))
				stop("length of 'lambda' and 'mu' does not match")
			dens <- sapply(1:nclass, function(k) prior[k] * mixturePosteriorHelper(data, lambda[[k]], mu[[k]], sigma[[k]]))
		} else if (is.vector(lambda)) {
			if (any(length(lambda) != ncomp))
				stop("length of 'lambda' and 'mu' does not match")
			dens <- sapply(1:nclass, function(k) prior[k] * mixturePosteriorHelper(data, lambda, mu[[k]], sigma[[k]]))
		} else {
			stop("'lambda' is neither list nor vector")
		}
	} else if (is.matrix(sigma)) {					# sigma matrix
		if (is.list(lambda)) {
			if (length(lambda) != nclass)
				stop("'length(lambda)' is not 'nclass'")
			if (any(sapply(lambda, length) != ncomp))
				stop("length of 'lambda' and 'mu' does not match")
			dens <- sapply(1:nclass, function(k) prior[k] * mixturePosteriorHelper(data, lambda[[k]], mu[[k]], sigma))
		} else if (is.vector(lambda)) {
			if (any(length(lambda) != ncomp))
				stop("length of 'lambda' and 'mu' does not match")
			dens <- sapply(1:nclass, function(k) prior[k] * mixturePosteriorHelper(data, lambda, mu[[k]], sigma))
		} else {
			stop("'lambda' is neither list nor vector")
		}
	}
	colnames(dens) <- paste("posterior", 1:nclass, sep = ".")
	rownames(dens) <- rownames(data)
	posterior <- dens/rowSums(dens)
    posterior
}
  
  
  
#' @nord
    
mixturePosteriorHelper <- function(data, lambda_k, mu_k, sigma_k) {
	ncomp_k <- length(lambda_k)
	d <- ncol(mu_k)
    if (is.list(sigma_k)) {
    	if (length(sigma_k) != ncomp_k)
    		stop("lengths of 'lambda' and 'sigma' do not fit")
    	if (any(as.vector(sapply(sigma_k, dim)) != d))
			stop("dimensionality of elements in 'sigma' incorrect")
		dens_k <- rowSums(sapply(1:ncomp_k, function(i) lambda_k[i] * dmvnorm(data, mu_k[i,], sigma_k[[i]])))
	} else if (is.matrix(sigma_k)) {
		if (any(dim(sigma_k) != d))
			stop("dimensionality of 'sigma_k' incorrect")
		dens_k <- rowSums(sapply(1:ncomp_k, function(i) lambda_k[i] * dmvnorm(data, mu_k[i,], sigma_k)))
	} else
		stop("'sigma_k' is neither list nor matrix")
	return(dens_k)
}



#' @return returns a factor of Bayes predictions.
#'
#' @rdname mixtureData
#'
#' @export

mixtureBayesClass <- function(data, prior, lambda, mu, sigma) {
	nclass <- length(prior)
	posterior <- mixturePosterior(data, prior, lambda, mu, sigma)
	bayesclass <- factor(max.col(posterior), levels = 1:nclass)
	return(bayesclass)
}
