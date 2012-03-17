#' @rdname FLXMCLmultinom
#' @aliases FLXMCLmultinom-class
#'
#' @import flexmix
#' @export

setClass("FLXMCLmultinom", contains = "FLXMCL")



#' This is a model driver for \code{\link[flexmix]{flexmix}} implementing mixtures of Multinomial Regression Models.
#'
#' @title Mixtures of Multinomial Regression Models
#' @param formula A formula which is interpreted relative to the formula specified in the call to \code{\link[flexmix]{flexmix}} using \code{\link[stats]{update.formula}}. 
#'   Only the left-hand side (response) of the formula is used. Default is to use the original \code{\link[flexmix]{flexmix}} model formula.
#' @param \dots Further arguments to and from other methods.
#'
#' @return Returns an object of class \code{FLXMCLmultinom} inheriting from \code{FLXMCL}.
#'
#' @rdname FLXMCLmultinom
# @aliases FLXMCLmultinom
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
#' model <- FLXMCLmultinom(trace = FALSE)
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

FLXMCLmultinom <- function(formula = . ~ ., ...) {
	z <- new("FLXMCLmultinom", weighted = TRUE, formula = formula,
		name = "Mixture of multinom models")
	z@defineComponent <- expression({
		predict <- function(x, ...) {
			post <- getS3method("predict", "nnet")(fit, newdata = x)
# cat("post1\n")			
# print(post)			
			if (ncol(post) == 1 && length(fit$lev) == 2) {  # fit$lev = NULL?
# print("no matrix")
    			post <- cbind(1-post, post)
    			colnames(post) <- fit$lev
    		}			
# cat("post2\n")			
# print(post)
			return(post)
		}
		logLik <- function(x, y, ...) {
# cat("y\n", y, "\n")
			post <- fitted(fit)
			n <- nrow(post)
# print(head(post))
# print(head(y))
			if (ncol(post) == 1) {
    			post <- cbind(1-post, post)	# post second level
    			ll <- post[cbind(1:n, y + 1)] # y in {0,1}; y == 1 iff second level, 0 otherwise
			} else {
    			ll <- t(post)[as.logical(t(y))]
			}
# print(head(ll))
			return(ll)
  		}
		new("FLXcomponent", parameters = list(wts = fit$wts), 
			logLik = logLik, predict = predict, df = fit$df)
	})
    z@preproc.y <- function(Y){ # Y results from model.response
    	class.ind <- function(cl) {
        	n <- length(cl)
        	x <- matrix(0, n, length(levels(cl)))
        	x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
        	dimnames(x) <- list(names(cl), levels(cl))
        	x
    	}
    	if (!is.matrix(Y)) 
        	Y <- as.factor(Y)
    	lev <- levels(Y)
    	if (is.factor(Y)) {
        	counts <- table(Y)
        	if (any(counts == 0L)) {
            	empty <- lev[counts == 0L]
            	warning(sprintf(ngettext(length(empty), "group %s is empty", 
               		"groups %s are empty"), paste(sQuote(empty), 
                	collapse = " ")), domain = NA)
            	Y <- factor(Y, levels = lev[counts > 0L])
            	lev <- lev[counts > 0L]
        	}
        	if (length(lev) < 2L) 
            	stop("need two or more classes to fit a multinom model")
        	if (length(lev) == 2L) 
            	Y <- as.vector(unclass(Y)) - 1
        	else Y <- class.ind(Y)
        	attr(Y, "lev") <- lev
    	}	
    	if (is.matrix(Y)) {
        	p <- ncol(Y)
        	sY <- Y %*% rep(1, p)
        	if (any(sY == 0)) 
            	stop("some case has no observations")
 		}
# print(attr(Y, "lev"))
	 	return(Y)
    }
    z@fit <- function(x, y, w, ...) {
        require("nnet")
# cat("y\n")
# print(y)
# cat("attr(y, lev)\n")
# print(lev)
# cat("mask\n")
# print(attr(x, "mask"))
# cat("softmax\n")
# print(attr(x, "softmax"))
# cat("entropy\n")
# print(attr(x, "entropy"))
        fit <- getS3method("nnet", "default")(x, y, w, mask = attr(x, "mask"), size = 0, skip = TRUE, 
            softmax = attr(x, "softmax"), entropy = attr(x, "entropy"), 
            rang = 0, trace = FALSE, ...)
		lev <- attr(y, "lev")
#print(lev)
		if (!is.null(lev))
			fit$lev <- lev
		fit$df = length(fit$wts)
		class(fit) <- c("multinom", "nnet")
# print(str(fit))
		with(fit, eval(z@defineComponent))
    }
    z
}


#' @rdname FLXMCLmultinom
#' @aliases FLXgetModelmatrix,FLXMCLmultinom-method
#'
#' @import flexmix
#' @export
#'
#' @docType methods

# offset?
setMethod("FLXgetModelmatrix", signature(model = "FLXMCLmultinom"), 
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
        modely <- model@preproc.y(modely)
# cat("attributes(modely)\n")
# print(attributes(modely))
        a <- is.matrix(modely)
        model@y <- as.matrix(modely)
# cat("attributes(model@y)\n")
# print(attributes(model@y))
        attr(model@y, "lev") <- attr(modely, "lev")
        attr(model@y, "is.matrix") <- a
# cat("attributes(model@y)\n")
# print(attributes(model@y))
    }
    else {
        mt1 <- if (is.null(model@terms)) 
            terms(model@fullformula, data = data)
        else model@terms
        mf <- model.frame(delete.response(mt1), data = data, 
            na.action = NULL)
        model@terms <- attr(mf, "terms")  
        offset <- model.offset(mf)
    }
    X <- model.matrix(model@terms, data = mf)
	r <- ncol(X)
    if (attr(model@y, "is.matrix")) {
		p <- ncol(model@y)
        # sY <- Y %*% rep(1, p)
        # if (any(sY == 0)) 
            # stop("some case has no observations")
        # if (!censored) {
            # Y <- Y/matrix(sY, nrow(Y), p)
            # w <- w * sY
        # }
        if (length(offset) > 1L) {
            if (ncol(offset) != p) 
                stop("ncol(offset) is wrong")
            mask <- c(rep(FALSE, r + 1L + p), rep(c(FALSE, rep(TRUE, 
                r), rep(FALSE, p)), p - 1L))
            X <- cbind(X, offset)
            # Wts <- as.vector(rbind(matrix(0, r + 1L, p), diag(p)))
            # fit <- nnet.default(X, Y, w, Wts = Wts, mask = mask, 
                # size = 0, skip = TRUE, softmax = TRUE, censored = censored, 
                # rang = 0, ...)
        }
        else {
            mask <- c(rep(FALSE, r + 1L), rep(c(FALSE, rep(TRUE, 
                r)), p - 1L))
            # fit <- nnet.default(X, Y, w, mask = mask, size = 0, 
                # skip = TRUE, softmax = TRUE, censored = censored, 
                # rang = 0, ...)
        }
        attr(X, "softmax") <- TRUE
        attr(X, "entropy") <- FALSE
    } else {
        if (length(offset) <= 1L) {
            mask <- c(FALSE, rep(TRUE, r))
            # fit <- nnet.default(X, Y, w, mask = mask, size = 0, 
                # skip = TRUE, entropy = TRUE, rang = 0, ...)
        }
        else {
            mask <- c(FALSE, rep(TRUE, r), FALSE)
            # Wts <- c(rep(0, r + 1L), 1)
            X <- cbind(X, offset)
            # fit <- nnet.default(X, Y, w, Wts = Wts, mask = mask, 
                # size = 0, skip = TRUE, entropy = TRUE, rang = 0, 
                # ...)
        }
        attr(X, "softmax") <- FALSE
        attr(X, "entropy") <- TRUE
    }
    attr(X, "mask") <- mask
    model@contrasts <- attr(X, "contrasts")
    model@x <- X
    model@x <- model@preproc.x(model@x)
    model@xlevels <- .getXlevels(model@terms, mf)
    model
})
