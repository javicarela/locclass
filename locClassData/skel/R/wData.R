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
#' @title Create an Artificial Classification Problem with W-shaped decision boundary
#'
#' @param n Number of observations.
#' @param d The dimensionality.
#' @param k Parameter to adjust the noise level.
#' @param data A \code{data.frame}.
#'
#' @return
#' returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#'
#' @examples
#' # Generate a training and a test set
#' train <- wData(1000)
#' test <- wData(1000)
#'
#' # Generate a grid of points
#' x.1 <- x.2 <- seq(0.01,1,0.01)
#' grid <- expand.grid(x.1 = 3 * x.1, x.2 = x.2)
#'
#' # Calculate the posterior probablities for all grid points
#' gridPosterior <- wPosterior(grid)
#'
#' # Draw contour lines of posterior probabilities and plot training observations
#' contour(x.1, x.2, matrix(gridPosterior[,1], length(x.1)), col = "gray")
#' points(train$x, col = train$y)
#'
#' # Calculate Bayes error
#' ybayes <- wBayesClass(test$x)
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
#' @aliases wData wLabels wPosterior wBayesClass
#'
#' @rdname wData
#'
#' @export
#'

wData <- function (n, d = 2, k = 1) {
    data <- matrix(runif(d * n), nrow = n)
    data[,1] <- data[,1] * 3
	posterior <- 0.5 + k * (data[,2] - 2 * abs(data[,1] %% 1 - 0.5))
	posterior[posterior < 0] <- 0 
	posterior[posterior > 1] <- 1 
    y <- as.factor(sapply(posterior, function(x) sample(1:2, 
        size = 1, prob = c(1 - x, x))))
    data <- list(x = data, y = y)
    class(data) <- c("locClass.wData", "locClass")
    attr(data, "d") <- d
    attr(data, "k") <- k
    return(data)
}



#' @return returns a factor of class labels.
#'
#' @rdname wData
#'
#' @export

wLabels <- function(data, k = 1) {
	d <- ncol(data)
	posterior <- 0.5 + k * (data[,2] - 2 * abs(data[,1] %% 1 - 0.5))
	posterior[posterior < 0] <- 0 
	posterior[posterior > 1] <- 1 
	classes <- as.factor(sapply(posterior, function(x) sample(1:2, size = 1, prob = c(1-x,x))))
	return(classes)
}	



#' @return returns a matrix of posterior probabilities.
#'
#' @rdname wData
#'
#' @export

wPosterior <- function(data, k = 1) {
	d <- ncol(data)
	posterior.2 <- 0.5 + k * (data[,2] - 2 * abs(data[,1] %% 1 - 0.5))
	posterior.2[posterior.2 < 0] <- 0 
	posterior.2[posterior.2 > 1] <- 1 
    posterior.1 <- 1 - posterior.2
    posterior <- cbind(posterior.1, posterior.2)
    colnames(posterior) <- paste("posterior", 1:d, sep = ".")
	return(posterior)	
}



#' @return returns a factor of Bayes predictions.
#'
#' @rdname wData
#'
#' @export

wBayesClass <- function(data, k = 1) {
	d <- ncol(data)
	posterior <- 0.5 + k * (data[,2] - 2 * abs(data[,1] %% 1 - 0.5))
	posterior[posterior < 0] <- 0 
	posterior[posterior > 1] <- 1 
	bayesclass <- as.factor(round(posterior) + 1)
	return(bayesclass)
}
