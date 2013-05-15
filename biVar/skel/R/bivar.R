#  Copyright (C) 2011-2013 J. Schiffner
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
#
#' Computes the bias-variance decomposition of the misclassification rate according to the approaches of James (2003) and Domingos (2000).
#'
#' If \code{posterior} is specified, \code{ybayes} is calculated from the posterior probabilities and the posteriors are used to 
#' calculate/estimate noise, the misclassification rate, systematic effect and variance effect.
#' If \code{ybayes} is specified it is ignored if \code{posterior} is given. Otherwise the empirical distribution of \code{ybayes} is inferred and used
#' to calculate the quantities of interest.
#' If neither \code{posterior} nor \code{ybayes} are specified it is assumed that the noise level is zero and 
#' the remaining quantities are calculated based on this supposition.
#'
#' @param y Predicted class labels on a test data set based on multiple training data sets. \code{y} is supposed to be a list where each element contains 
#'  the predictions for one single test observation. factor??? stimmt das noch?
#' @param grouping Vector of true class labels (a \code{factor}).
#' @param ybayes (Optional.) Bayes prediction. Ignored if \code{posterior} is specified as \code{ybayes} can be easily calculated from the posterior probabilities.
#' @param posterior (Optional.) Matrix of posterior probabilities, either known or estimated. It is assumed that the columns are ordered according
#'  to the factor levels of \code{grouping}.
#' @param ybest Prediction from the best fitting model on the whole population. Used for calculation of model and estimation bias as well as 
#'  systematic model effect and systematic estimation effect.
#' @param \dots Currently unused.
#'
#' @return A \code{data.frame} with the following columns:
#' \item{error}{Estimated misclassification probability.}
#' \item{noise}{(Only if \code{ybayes} or \code{posterior} was specified.) Noise or Bayes error rate.}
#' \item{bias}{Bias.}
#' \item{model.bias}{(Only if \code{ybest} was specified.) Model bias.}
#' \item{estimation.bias}{(Only if \code{ybest} was specified.) Estimation bias.}
#' \item{variance}{Variance.}
#' \item{unbiased.variance}{Unbiased variance.}
#' \item{biased.variance}{Biased variance.}
#' \item{net.variance}{Pointwise net variance.}
#' \item{systematic.effect}{Systematic effect.}
#' \item{systematic.model.effect}{(Only if \code{ybest} was specified.) Systematic model effect.}
#' \item{systematic.estimation.effect}{(Only if \code{ybest} was specified.) Systematic estimation effect.}
#' \item{variance.effect}{Variance effect.}
#' \item{ymain}{Main prediction.}
#' \item{ybayes}{(Only if \code{ybayes} or \code{posterior} was specified.) The optimal prediction.}
#' \item{size}{Numeric vector of the same length as the number of test observations. The number of predictions made for each test observation.}
#'
#' @title Bias-Variance Decomposition of the Misclassification Rate
#'
#' @references
#' James, G. M. (2003). Variance and bias for general loss functions. \emph{Machine Learning}, \bold{51(2)} 115--135.
#'
#' Domingos, P. (2000). A unified bias-variance decomposition for zero-one and squared loss. In
#' Proceedings of the Seventeenth National Conference on Artificial Intelligence and Twelfth Conference on 
#' Innovative Applications of Artificial Intelligence, pages 564--569. AAAI Press / The MIT Press.
#'
#' @export

bivar <- function(y, ...)
	UseMethod("bivar")
	


## todo:
# methods for other input structures for y:
# 1. mlr results / "long format", i.e. a data.frame with all predictions and ids of test observations
# 2. list of data.frames as long as number of training data sets / resampling iterations containing response, truth and ids of test observations
# noise estimation (via knn or other methods)
# further decomposition for local?
# extensibility for other loss functions
# 3. table method?

	
# bivar.ResamplePrediction <- function(y, ...) {
	
	
# }	
	
	
	
#' @method bivar data.frame
#' @S3method bivar data.frame
#'
#' @rdname bivar

bivar.data.frame <- function(y, ...) {
	lev <- levels(y[,1])
	y <- as.data.frame(t(y))
	y <- lapply(y, function(x) factor(x, levels = lev))
	res <- bivar.default(y, ...)
	return(res)
}



# @method bivar list
# @S3method bivar list
#
# @rdname bivar

# bivar.list <- function(y, ...) {
	# y <- as.data.frame(t(sapply(y, table)))										# distribution of y
	# res <- bivar.default(y, ...)
	# return(res)	
# }


#' @method bivar default
#' @S3method bivar default
#'
#' @rdname bivar

bivar.default <- function(y, grouping, ybayes, posterior, ybest = NULL, ...) {
    lev <- levels(grouping)
    n <- length(grouping)
    if (length(y) != n)
    	stop("'length(y)' must equal 'length(grouping)'")
    pred <- as.data.frame(t(sapply(y, table)))	# distribution of y
    p <- rowSums(pred)
    k <- length(lev)
#    p <- ncol(y)
#    pred <- as.data.frame(sapply(lev, function(x) rowSums(y == x)))             		# distribution of y
    ymain <- factor(max.col(pred, ties.method = "random"), levels = 1:k, labels = lev) 	# main prediction
#print(ymain)
	#ymain <- factor(lev[max.col(pred)], levels = lev)    
	#ymain <- factor(max.col(pred, ties.method = "random"), labels = names(pred), levels = seq(along = pred))         # main prediction
    #ymain <- factor(ymain, levels = lev) ## wieso in 2 schritten? klappt das? lev[max.col()]
#print(ymain)
    if (any(names(pred) != lev)) 
    	warning("problems with factor levels", "\n", "names(pred): ", names(pred), "\n", "lev: ", lev)#?
	if (missing(posterior)) {		## use empirical distribution of y
        V <- error <- numeric(n)
    	for (i in 1:n) {
            V[i] <- mean(y[[i]] != ymain[i])                                        # variance
            error[i] <- mean(y[[i]] != grouping[i])                             	# expected error
        }
 		if (missing(ybayes)) {	## assume noise = 0
            #error <- 1 - pred[cbind(1:n,grouping)]/p                               # expected error
            SE <- B <- as.numeric(grouping != ymain)                                # bias under the assumption that noise = 0, systematic effect
            Vu <- (1-B) * V
            Vb <- B * V
            Vn <- Vu - Vb
            VE <- error - SE                                                        # variance effect (decomposition of James), = Vn?
            # res <- data.frame(main = ymain, error = error, bias = B, variance = V, unbiased.variance = Vu,
                # biased.variance = Vb, net.variance = Vn, systematic.effect = SE, variance.effect = VE, size = p)
            if (!is.null(ybest)) {
				if (length(ybest) != n)
					stop("'length(ybest)' must equal 'length(grouping)")
	   			MB <- SEM <- as.numeric(grouping != ybest)							# model bias under the assumption noise = 0, systematic model effect
   				EB <- as.numeric(ybest != ymain)									# estimation bias
				SEE <- B - MB
	            res <- data.frame(main = ymain, error = error, bias = B, model.bias = MB, estimation.bias = EB, variance = V, unbiased.variance = Vu,
    	            biased.variance = Vb, net.variance = Vn, systematic.effect = SE, systematic.model.effect = SEM, systematic.estimation.effect = SEE, 
    	            variance.effect = VE, size = p)
			} else {
	            res <- data.frame(main = ymain, error = error, bias = B, variance = V, unbiased.variance = Vu,
    	            biased.variance = Vb, net.variance = Vn, systematic.effect = SE, variance.effect = VE, size = p)
			}		
		} else {					## estimate noise
			if (length(ybayes) != n)
				stop("'length(ybayes)' must equal 'length(grouping)")
            N <- as.numeric(grouping != ybayes)                               		# noise
            #error <- 1 - pred[cbind(1:n,noise$ybayes)]/p                           # expected error
            B <- as.numeric(ybayes != ymain)                                  		# bias
            Vu <- (1-B) * V
            Vb <- B * V
            Vn <- Vu - Vb
            h2 <- as.numeric(grouping != ymain)
            SE <- h2 - N
            VE <- error - h2                                                       	# variance effect
            #VE <- error - N - SE                                                    # variance effect
            if (!is.null(ybest)) {
				if (length(ybest) != n)
					stop("'length(ybest)' must equal 'length(grouping)")
	   			MB <- as.numeric(ybayes != ybest)									# model bias
   				EB <- as.numeric(ybest != ymain)									# estimation bias
            	SEM <- as.numeric(grouping != ybest) - N
				SEE <- SE - SEM
	            res <- data.frame(ymain = ymain, error = error, bias = B, model.bias = MB, estimation.bias = EB, variance = V, unbiased.variance = Vu,
    	            biased.variance = Vb, net.variance = Vn, systematic.effect = SE, systematic.model.effect = SEM, systematic.estimation.effect = SEE, 
    	            variance.effect = VE, noise = N, ybayes = ybayes, size = p)
            } else {
	            res <- data.frame(ymain = ymain, error = error, bias = B, variance = V, unbiased.variance = Vu,
    	            biased.variance = Vb, net.variance = Vn, systematic.effect = SE, variance.effect = VE,
        	        noise = N, ybayes = ybayes, size = p)
			}
		}
	} else {						## use posteriors
		if (nrow(posterior) != n)
			stop("'nrow(posterior)' must equal 'length(grouping)'")
		if (ncol(posterior) != length(lev))
			stop("'ncol(posterior)' must equal 'length(lev)'")
		if (!missing(ybayes))
			warning("argument 'ybayes' is ignored")
    	V <- numeric(n)
    	for (i in 1:n) {
        	V[i] <- mean(y[[i]] != ymain[i])                                     	# variance
    	}
    	ybayes <- factor(max.col(posterior, ties.method = "random"), levels = 1:k, labels = lev)  # Bayes prediction lev[max.col()] # muss nach faktorleveln geordnet sein!!!
    	h1 <- posterior[cbind(1:n,ybayes)]											# P(Y = ybayes | x)
    	N <- 1 - h1 
	    error <- rowSums(posterior * (1 - pred/p))                                 	# expected error
 	   	B <- as.numeric(ybayes != ymain)                                            # bias
	    Vu <- (1-B) * V
    	Vb <- B * V
    	Vn <- Vu - Vb
    	h2 <- posterior[cbind(1:n,ymain)]											# P(Y = ymain | x)
	    SE <- h1 - h2                                                              	# systematic effect
	    VE <- error - 1 + h2                                                     	# variance effect
       	if (!is.null(ybest)) {
			if (length(ybest) != n)
				stop("'length(ybest)' must equal 'length(grouping)")
   			MB <- as.numeric(ybayes != ybest)										# model bias
   			EB <- as.numeric(ybest != ymain)										# estimation bias
			h3 <- numeric(n)
    		for (i in 1:n) {
        		h3[i] <- mean(y[[i]] != ybest[i])                                  	
    		}
    		h4 <- posterior[cbind(1:n, ybest)]
			SEM <- h1 - h4
			SEE <- h4 - h2
	    	res <- data.frame(ymain = ymain, error = error, bias = B, model.bias = MB, estimation.bias = EB, variance = V, unbiased.variance = Vu,
    	    	biased.variance = Vb, net.variance = Vn, systematic.effect = SE, systematic.model.effect = SEM, systematic.estimation.effect = SEE, 
    	    	variance.effect = VE, noise = N, ybayes = ybayes, size = p)
       	} else {
    		res <- data.frame(ymain = ymain, error = error, bias = B, variance = V, unbiased.variance = Vu,
        		biased.variance = Vb, net.variance = Vn, systematic.effect = SE, variance.effect = VE,
        		noise = N, ybayes = ybayes, size = p)
       	}
	}
	return(res)
}
