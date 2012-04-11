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

#' Create the artificial data set of Hand and Vinciotti (2003).
#'
# details
#'
#' @title Create the Artificial Dataset of Hand and Vinciotti (2003)
#'
#' @param n Number of observations.
#' @param d The dimensionality.
#' @param k Parameter to adjust the class prior probabilities.
#' @param data A \code{data.frame}.
#'
#' @return
#' \code{hvData} returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#'
#' @examples
#' # Generate a training and a test set
#' train <- hvData(1000)
#' test <- hvData(1000)
#'
#' # Generate a grid of points
#' x.1 <- x.2 <- seq(0.01,1,0.01)
#' grid <- expand.grid(x.1 = x.1, x.2 = x.2)
#'
#' # Calculate the posterior probablities for all grid points
#' gridPosterior <- hvPosterior(grid)
#'
#' # Draw contour lines of posterior probabilities and plot training observations
#' contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray")
#' points(train$x, col = train$y)
#'
#' # Calculate Bayes error
#' ybayes <- hvBayesClass(test$x)
#' mean(ybayes != test$y)
#'
#' if (require(MASS)) {
#'	
#'     # Fit an LDA model and calculate misclassification rate on the test data set
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
#' @references
#' Hand, D. J., Vinciotti, V. (2003), Local versus global models for classification problems: 
#' Fitting models where it matters, \emph{The American Statistician}, \bold{57(2)} 124--130.
#'
#' @aliases hvData hvLabels hvPosterior hvBayesClass
#'
#' @rdname hvData
#'
#' @export
#'

hvData <- function(n, d = 2, k = 2) {
	data <- matrix(runif(d*n), nrow = n)
	posterior <- k*data[,d]/t(c(rep(1,d-1),k) %*% t(data))
	y <- as.factor(sapply(posterior, function(x) sample(1:2, size = 1, prob = c(1-x,x))))
	data <- list(x = data, y = y)
	class(data) <- c("locClass.hvData", "locClass")
	attr(data, "d") <- d
	attr(data, "k") <- k
	return(data)    
}

# hvData2 <- function(n, d, k) {
	# data <- matrix(runif(d*n), nrow = n)
	# #colnames(data) <- paste("x", 1:d, sep = "")
	# posterior <- k*data[,d]/t(c(rep(1,d-1),k) %*% t(data))
	# #posterior <- d*data[,d]/t(1:d %*% t(data))
	# #posterior <- 3*train[,2]^0.5/(3*train[,1]^0.5 + 4*train[,2]^0.5)
	# y <- as.factor(sapply(posterior, function(x) sample(1:2, size = 1, prob = c(1-x,x))))
	# data <- list(x = data, y = y)
	# class(data) <- c("locClass.hvData", "locClass")
	# return(data)    
# }



#' @return \code{hvLabels} returns a factor of class labels.
#'
#' @rdname hvData
#'
#' @export

hvLabels <- function(data, k = 2) {
	d <- ncol(data)
	posterior <- k*data[,d]/t(c(rep(1,d-1),k) %*% t(data))
	classes <- as.factor(sapply(posterior, function(x) sample(1:2, size = 1, prob = c(1-x,x))))
	return(classes)
}	



#' @return \code{hvPosterior} returns a matrix of posterior probabilities.
#'
#' @rdname hvData
#'
#' @export

hvPosterior <- function(data, k = 2) {
	d <- ncol(data)
    posterior.2 <- k*data[,d]/t(c(rep(1,d-1),k) %*% t(data))
    posterior.1 <- 1 - posterior.2
    posterior <- cbind(posterior.1, posterior.2)
    colnames(posterior) <- paste("posterior", 1:d, sep = ".")
	return(posterior)	
}



#' @return \code{hvBayesClass} returns a factor of Bayes predictions.
#'
#' @rdname hvData
#'
#' @export

hvBayesClass <- function(data, k = 2) {
	d <- ncol(data)
	posterior <- k*data[,d]/t(c(rep(1,d-1),k) %*% t(data))
	bayesclass <- as.factor(round(posterior) + 1)
	return(bayesclass)
}



#' @noRd
#' @method as.data.frame locClass
#'
#' @S3method as.data.frame locClass


as.data.frame.locClass <- function (x, row.names = NULL, optional = FALSE, ...) {
    data.frame(x = x$x, y = x$y)
}
