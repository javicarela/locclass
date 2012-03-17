## FLXPmultinom

#' Predict local membership, predict method for FLXPmultinom
#'
#' @param object An object of class "FLXPmultinom".
#' @param newdata Dataframe containing new data.
#' @param \dots ...
#'
#' @import flexmix
#' @export
#' 
#' @rdname FLXPmultinom-methods
#' @aliases predict,FLXPmultinom-method
#'
#' @docType methods

setMethod("predict", signature(object = "FLXPmultinom"), function(object, newdata, ...) {
	# model <- FLXgetModelmatrix(object, newdata, ...)
	mf <- model.frame(object@formula, data = newdata, na.action = NULL)
	terms <- attr(mf, "terms")
	x <- model.matrix(terms, data = mf)
	coefx <- apply(object@coef, 2, function(z) x %*% z)
	probs <- exp(coefx)/rowSums(exp(coefx))
	probs
})



#' Get local membership, fitted method for FLXPmultinom
#'
# @param object An object of class "FLXPmultinom".
# @param \dots ...
#'
#' @import flexmix
#' @export
#' 
#' @rdname FLXPmultinom-methods
#' @aliases fitted,FLXPmultinom-method
#'
#' @docType methods

setMethod("fitted", signature(object = "FLXPmultinom"), function(object, ...) {
	coefx <- apply(object@coef, 2, function(z) object@x %*% z)
	probs <- exp(coefx)/rowSums(exp(coefx))
	probs
})


# pr <- predict(res@concomitant, newdata = as.data.frame(d))
# plot(d$x, col = d$y, cex = pr[,1])
# plot(d$x, col = d$y, cex = pr[,2])


#================================================================
## FLXPwlda

#' @rdname FLXPwlda
#' @aliases FLXPwlda-class
#'
#' @import flexmix
#' @export

setClass("FLXPwlda", contains = "FLXP")



#' Creator function for the concomitant variable model. Priors are modeled by Linear Discriminant Analysis.
#'
#' @title Creator Function for the Concomitant Variable Model based on Linear Discriminant Analysis
#' @param formula A formula for determining the model matrix of the concomitant variables.
#'
#' @return Object of class \code{FLXPwlda} which extends class \code{FLXP} directly and is used for method dispatching.
#'
#' @import flexmix
#' @export
#'
#' @rdname FLXPwlda
#' @aliases FLXPwlda
#'
#' @examples
#' library(locClassData)
#' data <- flashData(1000)
#' grid <- expand.grid(x.1=seq(-6,6,0.2), x.2=seq(-4,4,0.2))
#' 
## svm label switching!!!
## was ist beim zusamensetzen der posteriors mit fehlenden klassen???
#' cluster <- kmeans(data$x, center = 2)$cluster
#' model <- FLXMCLlda()
#' fit <- flexmix(y ~ x.1 + x.2, data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = model, cluster = cluster) 
#'
#' ## prediction for single component models without aggregation
#' pred.grid <- predict(fit, newdata = grid)
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add = TRUE)
#' points(data$x, pch = as.character(data$y))
#loc <- predict(fit@concomitant, newdata = data)
#points(data$x, pch = as.character(data$y), cex = loc[,1])
#' 
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))), add = TRUE)
#' points(data$x, pch = as.character(data$y))
#points(data$x, col = data$y, pch = 19, cex = loc[,2])
#' 
#' ## prediction with aggregation depending on membership in mixture components
#' pred.grid <- predict(fit, newdata = grid, local.aggregate = TRUE)
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add  = TRUE)
#' points(data$x, pch = as.character(data$y))
#' 
#' ## local membership
#' loc.grid <- predict(fit@@concomitant, newdata = grid)
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(loc.grid[,1], length(seq(-6,6,0.2))), add  = TRUE)

FLXPwlda <- function (formula = ~.) {
    z <- new("FLXPwlda", name = "FLXPwlda", formula = formula)
	z@fit <- function(x, y, w, ...) {
       if (missing(w) || is.null(w)) 
            w <- rep(1, nrow(y))
        nc <- ncol(y)
 		y <- factor(max.col(y), levels = seq_len(nc))
        fit <- wlda(x, y, w, ...)
        pred <- predict(fit)
        if (nc > ncol(pred$posterior)) {
        	posterior <- matrix(0, nrow(pred$posterior), nc)
        	rownames(posterior) <- rownames(pred$posterior)
        	colnames(posterior) <- seq_len(nc)
        	posterior[,colnames(pred$posterior)] <- pred$posterior
        } else
        	posterior <- pred$posterior
        return(posterior)
	}
    z@refit <- function(x, y, w, ...) {
        if (missing(w) || is.null(w)) 
            w <- rep(1, nrow(y))
 		y <- factor(max.col(y))
        fit <- wlda(x, y, w, ...)
        #fit$coefnames <- colnames(x)
        fit$weights <- w
        #fit$vcoefnames <- fit$coefnames[seq_len(ncol(x))]
        #fit$lab <- seq_len(ncol(y))
        #class(fit) <- c("multinom", "nnet")
        #fit$Hessian <- nnet:::multinomHess(fit, x)
        #Xr <- qr(x)$rank
        #edf <- (ncol(y) - 1) * Xr
        #fit$df.residual <- sum(w) - edf
        class(fit) <- NULL
        as.matrix(fit)
    }
    z
}



# Get modelmatrix
#
#' @param model An object of class "FLXPwlda".
#' @param data ...
#' @param groups ...
#' @param lhs ...
# @param \dots ...
#'
#' @import flexmix
#' @export
#' 
#' @rdname FLXPwlda
#' @aliases FLXgetModelmatrix,FLXPwlda-method
#'
#' @docType methods

setMethod("FLXgetModelmatrix", signature(model = "FLXPwlda"), function (model, data, groups, lhs, ...) {
    mt <- terms(model@formula, data = data)
	attr(mt, "intercept") <- 0	# important: delete intercept
    mf <- model.frame(delete.response(mt), data = data, na.action = NULL)
    X <- model.matrix(mt, data = mf)
    if (nrow(X)) {
    	if (!flexmix:::checkGroup(X, groups$group)) 
            stop("model variables have to be constant for grouping variable")
        model@x <- X[groups$groupfirst, , drop = FALSE]
    } else {
        model@x <- matrix(1, nrow = sum(groups$groupfirst))
    }
    model
})



# Predict local membership, predict method for FLXPwlda
#
#' @param object An object of class "FLXPwlda".
#' @param newdata Dataframe containing new data.
# @param \dots ...
#'
#' @import flexmix
#' @export
#' 
#' @rdname FLXPwlda
#' @aliases predict,FLXPwlda-method
#'
#' @docType methods

setMethod("predict", signature(object = "FLXPwlda"), function(object, newdata, ...) {
	# model <- FLXgetModelmatrix(object, newdata, ...)
	mf <- model.frame(object@formula, data = newdata, na.action = NULL)
	terms <- attr(mf, "terms")
	attr(terms, "intercept") <- 0	# important: delete intercept
	x <- model.matrix(terms, data = mf)
	fit <- object@coef
	listnames <- rownames(fit)
#print(attributes(fit))	
	attr(fit, "dim") <- NULL	
#print(attributes(fit))
	names(fit) <- listnames
	class(fit) <- "wlda"
#print(class(fit))
	probs <- getS3method("predict", "wlda")(fit, newdata = x)$posterior
	probs
})



# Get local membership, predict method for FLXPwla
#
# @param object An object of class "FLXPwlda".
# @param \dots ...
#'
#' @import flexmix
#' @export
#' 
#' @rdname FLXPwlda
#' @aliases fitted,FLXPwlda-method
#'
#' @docType methods

setMethod("fitted", signature(object = "FLXPwlda"), function(object, ...) {
	fit <- object@coef
	listnames <- rownames(fit)
#print(attributes(fit))	
	attr(fit, "dim") <- NULL	
#print(attributes(fit))
	names(fit) <- listnames
	class(fit) <- "wlda"
#print(class(fit))
	probs <- getS3method("predict", "wlda")(fit, newdata = object@x)$posterior
	probs
})
