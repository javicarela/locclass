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
#' @title Create the Artificial Dataset of Hand and Vinciotti (2003) 
#'
#' @param n Number of observations.
#' @param d The dimensionality.
#' @param k Parameter to adjust the class prior probabilities.
#' @param data A \code{data.frame}.
#'
#' @return
#' returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#'
#' @examples
#' # Generate a training and a test set
#' train <- hvQuadraticData(1000)
#' test <- hvQuadraticData(1000)
#'
#' # Generate a grid of points
#' x.1 <- x.2 <- seq(0.01,1,0.01)
#' grid <- expand.grid(x.1 = x.1, x.2 = x.2)
#'
#' # Calculate the posterior probablities for all grid points
#' gridPosterior <- hvQuadraticPosterior(grid)
#'
#' # Draw contour lines of posterior probabilities and plot training observations
#' contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray")
#' points(train$x, col = train$y)
#'
#' # Calculate Bayes error
#' ybayes <- hvQuadraticBayesClass(test$x)
#' mean(ybayes != test$y)
#'
#' if (require(MASS)) {
#'	
#' 	   # Fit a QDA model and calculate misclassification rate on the test data set
#'     tr <- qda(y ~ ., data = as.data.frame(train))	
#'     pred <- predict(tr, as.data.frame(test))	
#'     mean(pred$class != test$y)
#' 
#'     # Draw decision boundary
#'     gridPred <- predict(tr, grid)
#'     contour(x.1, x.2, matrix(gridPred$posterior[,1], length(x.1)), col = "red", levels = 0.5, add = TRUE)
#'
#' }
#'
#' @references
#' Hand, D. J., Vinciotti, V. (2003), Local versus global models for classification problems: 
#' Fitting models where it matters, \emph{The American Statistician}, \bold{57(2)} 124--130.
#'
#' @alias hvQuadraticData hvQuadraticLabels hvQuadraticPosterior hvQuadraticBayesClass
#'
#' @rdname hvQuadraticData
#'
#' @export
#'

hvQuadraticData <- function(n, d = 2, k = 2) {
	data <- matrix(runif(d*n), nrow = n)
	dat <- data
	dat[,d] <- data[,d]^2
	#colnames(data) <- paste("x", 1:d, sep = "")
	posterior <- k*dat[,d]/t(rep(k,d) %*% t(dat))
	#posterior <- d*data[,d]/t(1:d %*% t(data))
	#posterior <- 3*train[,2]^0.5/(3*train[,1]^0.5 + 4*train[,2]^0.5)
	y <- as.factor(sapply(posterior, function(x) sample(1:2, size = 1, prob = c(1-x,x))))
	data <- list(x = data, y = y)
	class(data) <- c("locClass.hvdata", "locClass")
	return(data)    
}



#' @return returns a factor of class labels.
#'
#' @rdname hvQuadraticData
#'
#' @export

hvQuadraticLabels <- function(data, k = 2) {
	d <- ncol(data)
	data[,d] <- data[,d]^2
	posterior <- k*data[,d]/t(rep(k,d) %*% t(data))
	#posterior <- d*data[,d]/t(1:d %*% t(data))
	classes <- as.factor(sapply(posterior, function(x) sample(1:2, size = 1, prob = c(1-x,x))))
	return(classes)
}	



#' @return returns a matrix of posterior probabilities.
#'
#' @rdname hvQuadraticData
#'
#' @export

hvQuadraticPosterior <- function(data, k = 2) {
	d <- ncol(data)
	data[,d] <- data[,d]^2
    posterior.2 <- k*data[,d]/t(rep(k,d) %*% t(data))
	#posterior.2 <- d*data[,d]/t(1:d %*% t(data))
    posterior.1 <- 1 - posterior.2
    posterior <- cbind(posterior.1, posterior.2)
	return(posterior)	
}



#' @return returns a factor of Bayes predictions.
#'
#' @rdname hvQuadraticData
#'
#' @export

hvQuadraticBayesClass <- function(data, k = 2) {
	d <- ncol(data)
	data[,d] <- data[,d]^2
	posterior <- k*data[,d]/t(rep(k,d) %*% t(data))
	#posterior <- d*data[,d]/t(1:d %*% t(data))
	bayesclass <- as.factor(round(posterior) + 1)
	return(bayesclass)
}




        
# setwd("/Users/juliaschiffner/Documents/Dissertation/src")
# source("bayesclass/bayesclass.hvdata.R")

    # set.seed(112)

    # # training sets
	# N.train <- 500
	# D <- 200
	# N.test <- 2000
	# d <- 2


	# post <- function(data) {
    	# d <- ncol(data)-1
    	# train <- data[,1:d]
    	# posterior.1 <- d*train[,d]/t(1:d %*% t(train))
    	# posterior.0 <- 1 - posterior.1
    	# posterior <- cbind(posterior.0, posterior.1)
    	# return(posterior)
    	# #ybayes <- factor(max.col(posterior), labels = as.character(0:1), levels = 1:2)
    	# #return(list(ybayes = ybayes, posterior = posterior))
	# }
    
    # # training sets
    # train <- generator(N.train, d)

	# plot(train[,-3], col = train[,3])

	# # test grid
	# x1 <- x2 <- seq(0.01,1,0.01)
	# test.grid <- expand.grid(x1 = x1, x2 = x2)
	# posterior <- d*test.grid[,d]/t(1:d %*% t(test.grid))
	# #posterior <- 3*test.grid[,2]^0.5/(3*test.grid[,1]^0.5 + 4*test.grid[,2]^0.5)
	# y <- as.factor(sapply(posterior, function(x) sample(0:1, size = 1, prob = c(1-x,x))))
	# test.grid <- data.frame(test.grid, y = y)
	# #SY?
    # # SY <- rep(1, N.test.grid)
    # # SY[posterior[,2] > posterior[,1]] <- 2
    # # SY <- factor(SY, levels = 1:2)
    # # test.grid <- cbind(test.grid, y, SY, posterior)
    # # contour(x1,x2,matrix(posterior[,1], length(x1)), add = TRUE)

	# # test data
	# test <- generator(N.test, d)
	# posterior <- post(test)
    # # SY <- rep(1, N.test)
    # # SY[posterior[,2] > posterior[,1]] <- 2
    # # SY <- factor(SY, levels = 1:2)
    
    # # mean(SY != test$y)
