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
#' @title Generation of an xor like Classification Problem
#' 
#' @param n Number of observations.
#' @param prior Vector of class prior probabilities.
#' @param lambda The conditional probabilities for the mixture components given the class. Either a vector (if the same number \eqn{m} 
#'   of mixture components is desired for each class and the conditional probabilities for each class should be equal) or a list as long
#'   as the number of classes containing one vector of probabilities for every class. The length of the \eqn{k}-th element is the desired 
#'   number of mixture components for the \eqn{k}-th class.
#' @param mu11 Class center of first class, a vector.
#' @param mu12 Class center of first class, a vector.
#' @param mu21 Class center of second class, a vector.
#' @param mu22 Class center of second class, a vector.
#' @param sigma Covariance matrices for class 1 and 2. 
#' @param data A \code{data.frame}.
#'
#' @return
#' returns an object of class \code{"locClass"}, a list with components:
#' \item{x}{(A matrix.) The explanatory variables.}
#' \item{y}{(A factor.) The class labels.}
#'
#'
#' @aliases xorData xorLabels xorPosterior xorBayesClass
#'
#' @rdname xorData
#'
#' @import mvtnorm
#'
#' @export

xorData <- function(n, prior = rep(0.5,2), lambda = rep(0.5,2), mu11 = c(2,2), mu12 = c(-2,-2), mu21 = c(-2,2), mu22 = c(2,-2), sigma = diag(2)) {
	mu <- list(rbind(mu11,mu12), rbind(mu21, mu22))
	sigma <- list(sigma, sigma)
	data <- mixtureData(n, prior, lambda, mu, sigma)
	attr(data, "class") <- c("locClass.xorData", attr(data, "class"))
	return(data)
}



#' @return returns a factor of class labels.
#'
#' @rdname xorData
#'
#' @export

xorLabels <- function(data, prior = rep(0.5,2), lambda = rep(0.5,2), mu11 = c(2,2), mu12 = c(-2,-2), mu21 = c(-2,2), mu22 = c(2,-2), sigma = diag(2)) {
	mu <- list(rbind(mu11,mu12), rbind(mu21, mu22))
	sigma <- list(sigma, sigma)
	return(mixtureLabels(data, prior, lambda, mu, sigma))
}	



#' @return returns a matrix of posterior probabilities.
#'
#' @rdname xorData
#'
#' @export

xorPosterior <- function(data, prior = rep(0.5,2), lambda = rep(0.5,2), mu11 = c(2,2), mu12 = c(-2,-2), mu21 = c(-2,2), mu22 = c(2,-2), sigma = diag(2)) {
	mu <- list(rbind(mu11,mu12), rbind(mu21, mu22))
	sigma <- list(sigma, sigma)
	return(mixturePosterior(data, prior, lambda, mu, sigma))
}



#' @return returns a factor of Bayes predictions.
#'
#' @rdname xorData
#'
#' @export

xorBayesClass <- function(data, prior = rep(0.5,2), lambda = rep(0.5,2), mu11 = c(2,2), mu12 = c(-2,-2), mu21 = c(-2,2), mu22 = c(2,-2), sigma = diag(2)) {
	mu <- list(rbind(mu11,mu12), rbind(mu21, mu22))
	sigma <- list(sigma, sigma)	
	return(mixtureBayesClass(data, prior, lambda, mu, sigma))
}




 # if (data == "xor")
    # # Training
    # library(mvtnorm)
    # set.seed(112)
    # N.train <- 400
    # V <- 2
    # D <- 200
    # mu.11 <- c(2,2)
    # mu.12 <- c(-2,-2)
    # mu.21 <- c(-2,2)
    # mu.22 <- c(2,-2)
    # Sigma <- diag(V)

    # f <- function(mu.11, mu.12, mu.21, mu.22, Sigma, N.train, V) {
        # y <- factor(sample(1:2, size = N.train, replace = TRUE), levels = c(1,2))
        # s <- factor(sample(1:2, size = N.train, replace = TRUE), levels = c(1,2))
        # N.1.2.train <- table(y,s)
        # daten <- matrix(0, N.train, V, dimnames = list(NULL, c("x1","x2")))
        # daten[y == 1 & s == 1,] <- rmvnorm(N.1.2.train[1,1], mu.11, Sigma)
        # daten[y == 1 & s == 2,] <- rmvnorm(N.1.2.train[1,2], mu.12, Sigma)
        # daten[y == 2 & s == 1,] <- rmvnorm(N.1.2.train[2,1], mu.21, Sigma)
        # daten[y == 2 & s == 2,] <- rmvnorm(N.1.2.train[2,2], mu.22, Sigma)
        # daten <- as.data.frame(cbind(daten, y))
        # daten
    # }

    # train <- replicate(D, f(mu.11, mu.12, mu.21, mu.22, Sigma, N.train, V), simplify = FALSE)
    # save(train, file = "Daten/xor_train.RData")

    # # Plot der Trainingsdaten
    # tu.green <- rgb(0.518,0.722,0.094)
    # farbe <- rep("black", N.train)
    # farbe[train[[1]]$y == 2] <- tu.green
    # library(car)
    # pdf(file = "Grafiken/xor_train.pdf", title = "")
        # plot(train[[1]][,1:2], col = farbe, pch = 19, main = "training data set", cex = 1.2, xlab = "", ylab = "", cex.main = 1.8)
        # ellipse(center = mu.11, shape = diag(V), radius = 2, col = "red", lwd = 2)
        # ellipse(center = mu.12, shape = diag(V), radius = 2, col = "red", lwd = 2)
        # ellipse(center = mu.21, shape = diag(V), radius = 2, col = "red", lwd = 2)
        # ellipse(center = mu.22, shape = diag(V), radius = 2, col = "red", lwd = 2)
    # dev.off()

    # # Test
    # # Testgitter
    # x1 <- seq(-5,5,0.2)
    # x2 <- seq(-5,5,0.2)
    # test <- expand.grid(x1 = x1, x2 = x2)
    # N.test <- nrow(test)
    # dichte.1 <- 0.5*dmvnorm(test, mu.11, Sigma) + 0.5*dmvnorm(test, mu.12, Sigma)
    # dichte.2 <- 0.5*dmvnorm(test, mu.21, Sigma) + 0.5*dmvnorm(test, mu.22, Sigma)
    # dichte <- cbind(dichte.1, dichte.2)
    # posterior <- dichte/rowSums(dichte)
    # y <- factor(apply(dichte, 1, function(x) sample(c(1,2), size = 1, prob = x)), levels = c(1,2))
    # SY <- rep(1, N.test)
    # SY[dichte.2 > dichte.1] <- 2
    # SY <- factor(SY, levels = c(1,2))
    # test <- cbind(test, y, SY, posterior)

    # save(test, file = "Daten/xor_test.RData")

    # # Plot der Testdaten
    # farbe <- rep("black", N.test)
    # farbe[test$y == 2] <- tu.green
    # pdf(file = "Grafiken/xor_test.pdf", title = "")
        # plot(test[,1:2], col = farbe, pch = 19, main = "test grid", cex = 1.2)
    # dev.off()

    # filled.contour(x1, x2, matrix(y, length(x1), length(x2)), color.palette = colorRampPalette(c(tu.green, "black")))

    # # Plot der class posteriors
    # pdf(file = "Grafiekn/xor_test_posterior.pdf", title = "")
        # filled.contour(x1, x2, matrix(posterior[,1], length(x1), length(x2)), color.palette = colorRampPalette(c(tu.green, "black")))
    # dev.off()

    # plot(test[,1:2], col = y.stern, pch = 19)

    # #contour(x.1, x.2, matrix(dichte.1, length(x.1), length(x.2)))
    # #contour(x.1, x.2, matrix(dichte.2, length(x.1), length(x.2)), add = TRUE)

    # # Testdatensatz:
    # test2 <- f(mu.11, mu.12, mu.21, mu.22, Sigma, N.train = 2000, V)
    # dichte.1 <- 0.5*dmvnorm(test2[,1:2], mu.11, Sigma) + 0.5*dmvnorm(test2[,1:2], mu.12, Sigma)
    # dichte.2 <- 0.5*dmvnorm(test2[,1:2], mu.21, Sigma) + 0.5*dmvnorm(test2[,1:2], mu.22, Sigma)
    # dichte <- cbind(dichte.1, dichte.2)
    # posterior <- dichte/rowSums(dichte)
    # SY <- rep(1, 2000)
    # SY[dichte.2 > dichte.1] <- 2
    # SY <- factor(SY, levels = c(1,2))
    # test2 <- cbind(test.2, SY, posterior)
    # save(test2, file = "Daten/xor_test2.RData")
# }