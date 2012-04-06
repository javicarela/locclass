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
#' @title Generation of a Classification Problem with Outliers From The Wrong Class
#' 
#' @param n Number of observations.
#' @param alpha Distance from class center to the outliers in the x-coordinate.
#' @param beta Distance from class center to the outliers in the y-coordinate.
#' @param prop Proportion of outliers. Defaults to 0.05.
#' @param prior Vector of class prior probabilities.
#' @param data A \code{data.frame}.
#'
#' @return
#' returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#'
#' @examples
#' # Generate a training and a test set
#' train <- outlierWrongData(n = 1000, alpha = 10, beta = 10)
#' test <- outlierWrongData(n = 1000, alpha = 10, beta = 10)
#'  
#' # Generate a grid of points
#' x.1 <- x.2 <- seq(-7,15,0.1)
#' grid <- expand.grid(x.1 = x.1, x.2 = x.2)
#'
#' # Calculate the posterior probablities for all grid points
#' gridPosterior <- outlierWrongPosterior(grid, alpha = 10, beta = 10)
#'
#' # Draw contour lines of posterior probabilities and plot training observations
#' plot(train$x, col = train$y)
#' contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray", add = TRUE, levels = 0.5)
#'
#' # Calculate Bayes error
#' ybayes <- outlierWrongBayesClass(test$x, alpha = 10, beta = 10)
#' mean(ybayes != test$y)
#'
#' if (require(MASS)) {
#'	
#' 	   # Fit an LDA model and calculate misclassification rate on the test data set
#'     tr <- lda(y ~ ., data = as.data.frame(train))	
#'     pred <- predict(tr, as.data.frame(test))	
#'     print(mean(pred$class != test$y))
#' 
#'     # Draw decision boundary
#'     gridPred <- predict(tr, grid)
#'     contour(x.1, x.2, matrix(gridPred$posterior[,1], length(x.1)), col = "red", levels = 0.5, add = TRUE)
#'
#' }
#'
#' @aliases outlierWrongData outlierWrongLabels outlierWrongPosterior outlierWrongBayesClass
#'
#' @rdname outlierWrongData
#'
#' @import mvtnorm
#'
#' @export

outlierWrongData <- function(n, alpha = 5, beta = 5, prop = 0.05, prior = rep(0.5,2)) {
	data <- mixtureData(n, prior, lambda = list(1, c(1-prop, prop)), mu = list(matrix(c(0,1),1), matrix(c(0, alpha, -1, 1 + beta), 2)), sigma = matrix(c(3,0,0,1), 2))
	class(data) <- c("locClass.outlierWrongData", class(data))
	return(data)
}



#' @return returns a factor of class labels.
#'
#' @rdname outlierWrongData
#'
#' @export

outlierWrongLabels <- function(data, alpha = 5, beta = 5, prop = 0.05, prior = rep(0.5,2)) {
	return(mixtureLabels(data, prior, lambda = list(1, c(1-prop, prop)), mu = list(matrix(c(0,1),1), matrix(c(0, alpha, -1, 1 + beta), 2)), sigma = matrix(c(3,0,0,1), 2)))
}	



#' @return returns a matrix of posterior probabilities.
#'
#' @rdname outlierWrongData
#'
#' @export

outlierWrongPosterior <- function(data, alpha = 5, beta = 5, prop = 0.05, prior = rep(0.5,2)) {
	return(mixturePosterior(data, prior, lambda = list(1, c(1-prop, prop)), mu = list(matrix(c(0,1),1), matrix(c(0, alpha, -1, 1 + beta), 2)), sigma = matrix(c(3,0,0,1), 2)))
}



#' @return returns a factor of Bayes predictions.
#'
#' @rdname outlierWrongData
#'
#' @export

outlierWrongBayesClass <- function(data, alpha = 5, beta = 5, prop = 0.05, prior = rep(0.5,2)) {
	return(mixtureBayesClass(data, prior, lambda = list(1, c(1-prop, prop)), mu = list(matrix(c(0,1),1), matrix(c(0, alpha, -1, 1 + beta), 2)), sigma = matrix(c(3,0,0,1), 2)))
}

