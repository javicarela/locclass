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
#' @title Generation of Two-Class Classification Problem with two Gaussian distributions
#' 
#' @param n Number of observations.
#' @param prior Vector of class prior probabilities.
#' @param sigma1 Covariance matrix for class 1. 
#' @param sigma2 Covariance matrix for class 2. 
#' @param data A \code{data.frame}.
#'
#' @return
#' returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#'
#'
#' @aliases ringData ringLabels ringPosterior ringBayesClass
#'
#' @rdname ringData
#'
#' @import mvtnorm
#'
#' @export

ringData <- function(n, prior = rep(0.5,2), sigma1 = diag(2), sigma2 = 2*diag(2)) {
	mu <- list(matrix(rep(0,2),1), matrix(rep(0,2),1))
	sigma <- list(sigma1, sigma2)
	data <- mixtureData(n, prior, lambda = list(1,1), mu, sigma)
	class(data) <- c("locClass.ringData", class(data))
}



#' @return returns a factor of class labels.
#'
#' @rdname ringData
#'
#' @export

ringLabels <- function(data, prior = rep(0.5,2), sigma1 = diag(2), sigma2 = 2*diag(2)) {
	mu <- list(matrix(rep(0,2),1), matrix(rep(0,2),1))
	sigma <- list(sigma1, sigma2)
	return(mixtureLabels(data, prior, lambda = list(1,1), mu, sigma))
}	



#' @return returns a matrix of posterior probabilities.
#'
#' @rdname ringData
#'
#' @export

ringPosterior <- function(data, prior = rep(0.5,2), sigma1 = diag(2), sigma2 = 2*diag(2)) {
	mu <- list(matrix(rep(0,2),1), matrix(rep(0,2),1))
	sigma <- list(sigma1, sigma2)
	return(mixturePosterior(data, prior, lambda = list(1,1), mu, sigma))
}



#' @return returns a factor of Bayes predictions.
#'
#' @rdname ringData
#'
#' @export

ringBayesClass <- function(data, prior = rep(0.5,2), sigma1 = diag(2), sigma2 = 2*diag(2)) {
	mu <- list(matrix(rep(0,2),1), matrix(rep(0,2),1))
	sigma <- list(sigma1, sigma2)
	return(mixtureBayesClass(data, prior, lambda = list(1,1), mu, sigma))
}







# if(data == "ring") {
    # # Training
    # set.seed(112)
    # library(mvtnorm)
    # N.train <- 400
    # V <- 2
    # D <- 200
    # mu.1 <- c(1,0)
    # mu.2 <- c(0,0)
    # Sigma.1 <- diag(V)
    # Sigma.2 <- matrix(c(4,2,2,4), 2)
    # f <- function(mu.1, mu.2, Sigma.1, Sigma.2, N.train, V) {
        # y <- factor(sample(1:2, size = N.train, replace = TRUE), levels = c(1,2))
        # N.1.2.train <- table(y)
        # daten <- matrix(0, N.train, V, dimnames = list(NULL, c("x1","x2")))
        # daten[y == 1,] <- rmvnorm(N.1.2.train[1], mu.1, Sigma.1)
        # daten[y == 2,] <- rmvnorm(N.1.2.train[2], mu.2, Sigma.2)
        # daten <- as.data.frame(cbind(daten, y))
        # daten
    # }
    # train <- replicate(D, f(mu.1, mu.2, Sigma.1, Sigma.2, N.train, V), simplify = FALSE)
    # save(train, file = "Daten/ring_train.RData")

    # # Plot der Trainingsdaten
    # tu.green <- rgb(0.518,0.722,0.094)
    # farbe <- rep("black", N.train)
    # farbe[train[[1]]$y == 2] <- tu.green
    # pdf(file = "Grafiken/ring_train.pdf", title = "")
        # plot(train[[1]][,1:2], col = farbe, pch = 19, main = "training data set", cex = 1.2, xlab = "", ylab = "", cex.main = 1.8)
    # dev.off()

    # # Test
    # # Testgitter
    # x1 <- seq(-4,6,0.2)
    # x2 <- seq(-4,6,0.2)
    # test <- expand.grid(x1 = x1, x2 = x2)
    # N.test <- nrow(test)
    # dichte.1 <- dmvnorm(test, mu.1, Sigma.1)
    # dichte.2 <- dmvnorm(test, mu.2, Sigma.2)
    # dichte <- cbind(dichte.1, dichte.2)
    # posterior <- dichte/rowSums(dichte)
    # y <- factor(apply(dichte, 1, function(x) sample(c(1,2), size = 1, prob = x)), levels = c(1,2))
    # SY <- rep(1, N.test)
    # SY[dichte.2 > dichte.1] <- 2
    # SY <- factor(SY, levels = c(1,2))
    # test <- cbind(test, y, SY, posterior)
    # save(test, file = "Daten/ring_test.RData")

    # # Plot der Testdaten
    # farbe <- rep("black", N.test)
    # farbe[test$y == 2] <- tu.green
    # pdf(file = "Grafiken/ring_test.pdf", title = "")
        # plot(test[,1:2], col = farbe, pch = 19, main = "test grid", cex = 1.2) # evtl. filled contour
    # dev.off()

    # # Plot der class posteriors
    # pdf(file = "Grafiken/ring_test_posterior.pdf", title = "")
        # filled.contour(x1, x2, matrix(posterior, length(x1), length(x2)), color.palette = colorRampPalette(c(tu.green, "black")))
    # dev.off()


    # filled.contour(x1, x2, matrix(test$y, length(x1), length(x2)), color.palette = colorRampPalette(c(tu.green, "black")))
    # filled.contour(x1, x2, matrix(posterior, length(x1), length(x2)), color.palette = colorRampPalette(c(tu.green, "black")))
    # plot(test[,1:2], col = y.stern, pch = 19)

    # # Testdatensatz
    # test2 <- f(mu.1, mu.2, Sigma.1, Sigma.2, N.train = 1000, V)
    # dichte.1 <- dmvnorm(test2[,1:2], mu.1, Sigma.1)
    # dichte.2 <- dmvnorm(test2[,1:2], mu.2, Sigma.2)
    # dichte <- cbind(dichte.1, dichte.2)
    # posterior <- dichte/rowSums(dichte)
    # SY <- rep(1, 1000)
    # SY[dichte.2 > dichte.1] <- 2
    # SY <- factor(SY, levels = c(1,2))
    # test2 <- cbind(test2, SY, posterior)
    # save(test2, file = "Daten/ring_test2.RData")
# } 