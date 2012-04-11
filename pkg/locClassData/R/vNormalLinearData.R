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

#' Create a binary classification problem with V-shaped decision boundary.
#'
# details
#'
#' @title Create a Binary Classification Problem with V-shaped Decision Boundary
#'
#' @param n Number of observations.
#' @param prior Vector of class prior probabilities.
#' @param lambda The conditional probabilities for the mixture components given the class. Either a vector (if the same number \eqn{m} 
#'   of mixture components is desired for each class and the conditional probabilities for each class should be equal) or a list as long
#'   as the number of classes containing one vector of probabilities for every class. The length of the \eqn{k}-th element is the desired 
#'   number of mixture components for the \eqn{k}-th class.
#' @param sigma The covariance matrices of the mixture components. Either one single matrix that is used for each mixture
#'   component or a list as long as the number of classes. List elements can be matrices (in case that for all mixture components forming
#'   one class the same covariance matrix shall be used) or lists of matrices as long as the number of mixture components in the corresponding 
#'   class.
#' @param data A \code{data.frame}.
#'
#' @return
#' \code{vNormalLinearData} returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#'
#' @examples
#' # Generate a training and a test set
#' train <- vNormalLinearData(1000)
#' test <- vNormalLinearData(1000)
#'
#' # Generate a grid of points
#' x.1 <- x.2 <- seq(-5,5,0.1)
#' grid <- expand.grid(x.1 = x.1, x.2 = x.2)
#'
#' # Calculate the posterior probablities for all grid points
#' gridPosterior <- vNormalLinearPosterior(grid)
#'
#' # Draw contour lines of posterior probabilities and plot training observations
#' contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray")
#' points(train$x, col = train$y)
#'
#' # Calculate Bayes error
#' ybayes <- vNormalLinearBayesClass(test$x)
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
#' @aliases vNormalLinearData vNormalLinearLabels vNormalLinearPosterior vNormalLinearBayesClass
#'
#' @rdname vNormalLinearData
#'
#' @export
#'

vNormalLinearData <- function(n, prior = rep(0.5,2), lambda = rep(0.5,2), sigma = diag(2)) {
	mu <- list(rbind(c(-3,0), c(3,0)), rbind(c(-2,1), c(2,1)))
	data <- mixtureData(n, prior, lambda, mu, sigma)
	class(data) <- c("locClass.vNormalLinearData", class(data))
	return(data)
}


#' @return \code{vNormalLinearLabels} returns a factor of class labels.
#'
#' @rdname vNormalLinearData
#'
#' @export

vNormalLinearLabels <- function(data, prior = rep(0.5,2), lambda = rep(0.5,2), sigma = diag(2)) {
	mu <- list(rbind(c(-3,0), c(3,0)), rbind(c(-2,1), c(2,1)))
	return(mixtureLabels(data, prior, lambda, mu, sigma))
}	



#' @return \code{vNormalLinearPosterior} returns a matrix of posterior probabilities.
#'
#' @rdname vNormalLinearData
#'
#' @export

vNormalLinearPosterior <- function(data, prior = rep(0.5,2), lambda = rep(0.5,2), sigma = diag(2)) {
	mu <- list(rbind(c(-3,0), c(3,0)), rbind(c(-2,1), c(2,1)))
	return(mixturePosterior(data, prior, lambda, mu, sigma))
}



#' @return \code{vNormalLinearBayesClass} returns a factor of Bayes predictions.
#'
#' @rdname vNormalLinearData
#'
#' @export

vNormalLinearBayesClass <- function(data, prior = rep(0.5,2), lambda = rep(0.5,2), sigma = diag(2)) {
	mu <- list(rbind(c(-3,0), c(3,0)), rbind(c(-2,1), c(2,1)))
	return(mixtureBayesClass(data, prior, lambda, mu, sigma))
}
