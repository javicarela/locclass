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
#' @title Generation of a Classification Problem with Outliers From the Correct Class
#' 
#' @param n Number of observations.
#' @param alpha Distance from class center to the outliers.
#' @param prior Vector of class prior probabilities.
#' @param data A \code{data.frame}.
#'
#' @return
#' returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#'
#' @example
#' # Generate a training and a test set
#' train <- outlierCorrectData(n = 1000, alpha = 20)
#' test <- outlierCorrectData(n = 1000, alpha = 20)
#'  
#' # Generate a grid of points
#' x.1 <- x.2 <- seq(-7,15,0.1)
#' grid <- expand.grid(x.1 = x.1, x.2 = x.2)
#'
#' # Calculate the posterior probablities for all grid points
#' gridPosterior <- outlierCorrectPosterior(grid, alpha = 20)
#'
#' # Draw contour lines of posterior probabilities and plot training observations
#' plot(train$x, col = train$y)
#' contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray", add = TRUE)
#'
#' # Calculate Bayes error
#' ybayes <- outlierCorrectBayesClass(test$x, alpha = 20)
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
#' @aliases outlierCorrectData outlierCorrectLabels outlierCorrectPosterior outlierCorrectBayesClass
#'
#' @rdname outlierCorrectData
#'
#' @import mvtnorm
#'
#' @export

outlierCorrectData <- function(n, alpha = 5, prior = rep(0.5,2)) {
	data <- mixtureData(n, prior, lambda = list(1, c(0.95,0.05)), mu = list(matrix(c(0,-0.5),1), matrix(c(0, 0, 0.5, 0.5 + alpha), 2)), sigma = matrix(c(3,0,0,1), 2))
	class(data) <- c("locClass.outlierCorrectData", class(data))
	return(data)
}



#' @return returns a factor of class labels.
#'
#' @rdname outlierCorrectData
#'
#' @export

outlierCorrectLabels <- function(data, alpha = 5, prior = rep(0.5,2)) {
	return(mixtureLabels(data, prior, lambda = list(1, c(0.95,0.05)), mu = list(matrix(c(0,-0.5),1), matrix(c(0, 0, 0.5, 0.5 + alpha), 2)), sigma = matrix(c(3,0,0,1), 2)))
}	



#' @return returns a matrix of posterior probabilities.
#'
#' @rdname outlierCorrectData
#'
#' @export

outlierCorrectPosterior <- function(data, alpha = 5, prior = rep(0.5,2)) {
	return(mixturePosterior(data, prior, lambda = list(1, c(0.95,0.05)), mu = list(matrix(c(0,-0.5),1), matrix(c(0, 0, 0.5, 0.5 + alpha), 2)), sigma = matrix(c(3,0,0,1), 2)))
}



#' @return returns a factor of Bayes predictions.
#'
#' @rdname outlierCorrectData
#'
#' @export

outlierCorrectBayesClass <- function(data, alpha = 5, prior = rep(0.5,2)) {
	return(mixtureBayesClass(data, prior, lambda = list(1, c(0.95,0.05)), mu = list(matrix(c(0,-0.5),1), matrix(c(0, 0, 0.5, 0.5 + alpha), 2)), sigma = matrix(c(3,0,0,1), 2)))
}

