# Copyright (C) 2011-2012 Julia Schiffner
# Copyright (C) 2004-2011 Friedrich Leisch and Bettina Gruen
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



#' @rdname FLXMCL
#' @aliases FLXMCLsvm-class
#'
#' @import flexmix
#' @export

setClass("FLXMCLsvm", contains = "FLXMCL")



#' This is a model driver for \code{\link[flexmix]{flexmix}} implementing mixtures of Support Vector Machines.
#'
#' @title Mixtures of Support Vector Machines
#' @param formula A formula which is interpreted relative to the formula specified in the call to \code{\link[flexmix]{flexmix}} using \code{\link[stats]{update.formula}}. 
#'   Only the left-hand side (response) of the formula is used. Default is to use the original \code{\link[flexmix]{flexmix}} model formula.
#' @param \dots Further arguments to and from other methods.
#'
#' @return Returns an object of class \code{FLXMCLsvm} inheriting from \code{FLXMCL}.
#'
#' @rdname FLXMCLsvm
#' @aliases FLXMCLsvm
#'
#' @import flexmix
#' @export
#'
#' @examples
#' library(locClassData)
#' data <- flashData(1000)
#' grid <- expand.grid(x.1=seq(-6,6,0.2), x.2=seq(-4,4,0.2))
#' 
#' cluster <- kmeans(data$x, center = 2)$cluster
#' model <- FLXMCLsvm(kernel = "linear")
#' fit <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = model, cluster = cluster)
#' 
#' ## prediction for single component models without aggregation
#' pred.grid <- predict(fit, newdata = grid)
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]]["decision",][[1]][,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]]["decision",][[1]][,1], length(seq(-6,6,0.2))), add = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]]["decision",][[1]][,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]]["decision",][[1]][,1], length(seq(-6,6,0.2))), add = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' ## prediction with aggregation depending on membership in mixture components
#' pred.grid <- mypredict(fit, newdata = grid, aggregate = TRUE)
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]]$decision[,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]]$decision[,1], length(seq(-6,6,0.2))), add  = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]]$posterior[,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]]$posterior[,1], length(seq(-6,6,0.2))), add  = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' ## local memberhsip
#' loc.grid <- prior(fit, newdata = grid)
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(loc.grid[,1], length(seq(-6,6,0.2))), add  = TRUE)

# library(mlr)
# task <- makeClassifTask(data = as.data.frame(data), target = "y")
# lrn <- makeLearner("classif.FLXMCLsvm", kernel = "linear", centers = 2)
# tr <- train(lrn, task = task)
# pr <- predict(tr, newdata = grid)


FLXMCLsvm <- function(formula = . ~ ., ...) {
	z <- new("FLXMCLsvm", weighted = TRUE, formula = formula,
		name = "Mixture of SVM models")
	z@defineComponent <- expression({
		predict <- function(x, ...) {
			pred <- getS3method("predict", "wsvm")(fit, newdata = x, probability = TRUE, decision.values = TRUE, ...)
			probs <- attr(pred, "probabilities")
        	decs <- attr(pred, "decision.values")
			lev <- levels(pred)
			ng <- length(lev)
			nl <- length(fit$labels)
# cat("nl\n")
# print(nl)
# cat("diff(fit$labels)\n")
# print(diff(fit$labels))
			if (any(diff(fit$labels) < 0)) { 	# order of levels and labels different
				# binary classification problems
				problems <- cbind(rep(fit$labels, nl:1-1), rep(fit$labels, 1:nl-1))
# cat("problems\n")
# print(problems)
				# binary problems where first label is larger than second label
				col.index <- problems[,1] > problems[,2]
# cat("col.index\n")
# print(col.index)
# print(head(decs))
				# change sign for these binary problems and adjust the colnames
				decs[,col.index] <- decs[,col.index] * (-1)
				colnames(decs)[col.index] <- apply(problems[col.index,,drop = FALSE], 1, function(l) paste(lev[l[2]], lev[l[1]], sep = "/"))
# print(head(decs))
			}
			if (ng > nl) {
				# add columns for missing classes in posterior probability and decision value matrices
	        	posterior <- matrix(0, nrow(probs), ng)
	        	rownames(posterior) <- rownames(probs)
	        	colnames(posterior) <- lev
	        	posterior[,colnames(probs)] <- probs
	        	decision <- matrix(0, nrow(decs), ng * (ng - 1) / 2)
	  ## Was ist, wenn in Komp. 1 Klasse 1 vorhanden und in Komp. 2 Klassen 1 und 2?
	  ## Welche Werte sollte decision in Komp. 1 haben? 0 eigentlich nicht sinnvoll, weil Klasse 2 nie vorhergesagt werden kann...
	  ## sollte hohen Wert für Klasse 1 haben...
	        	colnames(decision) <- paste(rep(lev, ng:1-1), rep(lev, 1:ng-1), sep = "/")
	        	decision[,colnames(decs)] <- decs
			} else {
				# sort columns of posterior probability and decision value matrices
				cnames <- colnames(probs)
				posterior <- probs[,sort(cnames)]
				decision <- decs[,paste(rep(lev, ng:1-1), rep(lev, 1:ng-1), sep = "/"), drop = FALSE]
        	}
# print(head(decision))
        	return(list(posterior = posterior, decision = decision))
		}
		logLik <- function(x, y, ...) {
			# #dec <- attr(getS3method("predict", "wsvm")(fit, newdata = x, decision.values = TRUE, ...), "decision.values")
			# dec <- fit$decision.values
# #print(dec)			
			# #bin.problems <- colnames(dec)
			# # 2 classes
			# labels <- fit$labels  ## if dec positive decision for first of the two class labels
			# correct <- (as.character(y) == labels[1]) * 2 - 1
			# ll <- exp(correct * dec)
# #			ll1 <- ll/max(ll)
# #print(ll1)
# return(ll)		
# # > 2 classes
    		post <- attr(getS3method("predict", "wsvm")(fit, newdata = x, probability = TRUE, ...), "probabilities")
     		ng <- length(attr(y, "lev"))
# print(head(post))
# print(head(y))
    		if (ng > ncol(post)) {
    			ll <- rep(0, nrow(post))
    			col.index <- match(y, colnames(post), 0)
    			row.index <- which(col.index > 0)
    			ll[row.index] <- post[cbind(row.index, col.index[row.index])]
    		} else {
	    		ll <- post[cbind(rownames(post), as.character(y))]
	    	}
# print(head(ll))
	    	return(ll)
		}
		new("FLXcomponent", parameters = list(coefs = fit$coefs), 
			logLik = logLik, predict = predict, df = fit$df)
	})
	z@preproc.y <- function(y) {
		if (is.factor(y)) {
			lev <- levels(y)
			y <- as.matrix(y)
			attr(y, "lev") <- lev
			return(y)
		} else
			return(as.matrix(y))
	}
	z@fit <- function(x, y, w) {
		lev <- attr(y, "lev")
		w <- w/sum(w) * nrow(x)
		if (is.null(lev))
			fit <- wsvm(x, as.vector(y), case.weights = w, probability = TRUE, ...)
		else
			fit <- wsvm(x, factor(y, levels = lev), case.weights = w, probability = TRUE, ...)
# print(str(fit))
		fit$df <- sum(fit$nSV)
		with(fit, eval(z@defineComponent))
	}
	z
}



#' @rdname FLXMCLsvm
#' @aliases FLXgetModelmatrix,FLXMCLsvm-method
#'
#' @import flexmix
#' @export
#'
#' @docType methods

setMethod("FLXgetModelmatrix", signature(model = "FLXMCLsvm"), 
	function (model, data, formula, lhs = TRUE, ...) {
    formula <- flexmix:::RemoveGrouping(formula)
    if (length(grep("\\|", deparse(model@formula)))) 
        stop("no grouping variable allowed in the model")
    if (is.null(model@formula)) 
        model@formula = formula
    model@fullformula = update(terms(formula, data = data), 
        model@formula)
    if (lhs) {
        mf <- if (is.null(model@terms)) 
            model.frame(model@fullformula, data = data, na.action = NULL)
        else model.frame(model@terms, data = data, na.action = NULL)
        model@terms <- attr(mf, "terms")
        modely <- model.response(mf)
        model@y <- model@preproc.y(modely)
    }
    else {
        mt1 <- if (is.null(model@terms)) 
            terms(model@fullformula, data = data)
        else model@terms
        mf <- model.frame(delete.response(mt1), data = data, 
            na.action = NULL)
        model@terms <- attr(mf, "terms")    
    }
    attr(model@terms, "intercept") <- 0 ## intercept removed
    X <- model.matrix(model@terms, data = mf)
    model@contrasts <- attr(X, "contrasts")
    model@x <- X
    model@x <- model@preproc.x(model@x)
    model@xlevels <- .getXlevels(model@terms, mf)
    model
})
