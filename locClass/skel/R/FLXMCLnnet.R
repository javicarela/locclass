#' @rdname FLXMCLnnet
#' @aliases FLXMCLnnet-class
#'
#' @import flexmix
#' @export

setClass("FLXMCLnnet", contains = "FLXMCL")



#' This is a model driver for \code{\link[flexmix]{flexmix}} implementing mixtures of Neural Netowrks.
#'
#' @title Mixtures of Neural Networks
#' @param formula A formula which is interpreted relative to the formula specified in the call to \code{\link[flexmix]{flexmix}} using \code{\link[stats]{update.formula}}.
#'   Only the left-hand side (response) of the formula is used. Default is to use the original \code{\link[flexmix]{flexmix}} model formula.
#' @param \dots Further arguments to and from other methods.
#'
#' @return Returns an object of class \code{FLXMCLnnet} inheriting from \code{FLXMCL}.
#'
#' @rdname FLXMCLnnet
# @aliases FLXMCLnnet
#'
#' @import flexmix nnet
#' @export
#'
#' @examples
#' library(locClassData)
#' data <- flashData(1000)
#' grid <- expand.grid(x.1=seq(-6,6,0.2), x.2=seq(-4,4,0.2))
#' 
#' cluster <- kmeans(data$x, center = 2)$cluster
#' model <- FLXMCLnnet(size = 1, trace = FALSE)
#' fit <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = model, cluster = cluster)
#' 
#' ## prediction for single component models without aggregation
#' pred.grid <- predict(fit, newdata = grid)
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))), add = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' ## prediction with aggregation depending on membership in mixture components
#' pred.grid <- predict(fit, newdata = grid, local.aggregate = TRUE)
#' image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add  = TRUE)
#' points(data$x, pch = as.character(data$y))
#'
#' ## local memberhsip
#' loc.grid <- predict(fit@@concomitant, newdata = grid)
#' contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(loc.grid[,1], length(seq(-6,6,0.2))), add  = TRUE)

FLXMCLnnet <- function(formula = . ~ ., ...) {
	z <- new("FLXMCLnnet", weighted = TRUE, formula = formula,
		name = "Mixture of nnet models")
	z@defineComponent <- expression({
		predict <- function(x, ...) {
			post <- getS3method("predict", "nnet")(fit, newdata = x, type = "raw", ...)
			if (ncol(post) == 1) {
				post <- cbind(1-post, post)
				colnames(post) <- fit$lev
			}
			return(post)
		}
		logLik <- function(x, y, ...) {
    		# post <- getS3method("predict", "nnet")(fit, newdata = x, type = "raw", ...)
    		post <- fitted(fit)
# print(head(post))
# print(head(y))
			n <- nrow(post)
			if (ncol(post) == 1) {
				post <- cbind(1-post, post)
    			ll <- post[cbind(1:n, y + 1)] # y in {0,1}; y == 1 iff second level, 0 otherwise
			} else {
    			ll <- t(post)[as.logical(t(y))]
			}
			return(ll)
		}
		new("FLXcomponent", parameters = list(wts = fit$wts), 
			logLik = logLik, predict = predict, df = fit$df)
	})
	z@preproc.y <- function(y) { # y results from model response
    	class.ind <- function(cl) {
        	n <- length(cl)
        	x <- matrix(0, n, length(levels(cl)))
        	x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
        	dimnames(x) <- list(names(cl), levels(cl))
        	x
    	}
    	if (is.factor(y)) {
        	lev <- levels(y)
        	counts <- table(y)
        	if (any(counts == 0L)) {
            	empty <- lev[counts == 0L]
            	warning(sprintf(ngettext(length(empty), "group %s is empty", 
                	"groups %s are empty"), paste(empty, collapse = " ")), 
                	domain = NA)
            	y <- factor(y, levels = lev[counts > 0L])
        	}
        	if (length(lev) == 2L) { ### wirklich lev oder lev[counts > 0]
            	y <- as.matrix(unclass(y)) - 1 ## original: as.vector
            	attr(y, "lev") <- lev
            	attr(y, "entropy") <- TRUE
            	attr(y, "softmax") <- FALSE
        	}
        	else {
            	y <- class.ind(y)
            	attr(y, "lev") <- lev
            	attr(y, "entropy") <- FALSE
            	attr(y, "softmax") <- TRUE
        	}
    	}
    	return(y)
	}
	z@fit <- function(x, y, w) {
		lev <- attr(y, "lev")
#print(lev)
		if (is.null(lev)) {
			fit <- nnet(x, y, weights = w, ...)
		} else {
			fit <- nnet(x, y, weights = w, entropy = attr(y, "entropy"), softmax = attr(y, "softmax"), ...)
			fit$lev <- lev
		}			
		fit$df <- length(fit$wts)
		with(fit, eval(z@defineComponent))
	}
	z
}



#' @rdname FLXMCLnnet
#' @aliases FLXgetModelmatrix,FLXMCLnnet-method
#'
#' @import flexmix
#' @export
#'
#' @docType methods

setMethod("FLXgetModelmatrix", signature(model = "FLXMCLnnet"), 
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
