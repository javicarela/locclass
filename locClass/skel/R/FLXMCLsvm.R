#' @rdname FLXMCLsvm
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
# @aliases FLXMCLsvm
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

FLXMCLsvm <- function(formula = . ~ ., ...) {
	z <- new("FLXMCLsvm", weighted = TRUE, formula = formula,
		name = "Mixture of wsvm models")
	z@defineComponent <- expression({
		predict <- function(x, ...) {
			# pred <- getS3method("predict", "wsvm")(fit, newdata = x, probability = TRUE, ...)
			pred <- getS3method("predict", "wsvm")(fit, newdata = x, probability = TRUE, decision.values = TRUE, ...)
			probs <- attr(pred, "probabilities")
			cnames <- colnames(probs)
			probs[,sort(cnames)]
			attr(probs, "dec") <- attr(pred, "decision.values")
			probs
		}
		logLik <- function(x, y, ...) { # positive, mit gewichten???? aufsummieren???
    		post <- attr(getS3method("predict", "wsvm")(fit, newdata = x, probability = TRUE, ...), "probabilities")
    		n <- nrow(post)
    		return(post[cbind(1:n, as.character(y))])
		}
		new("FLXcomponent", parameters = list(coefs = fit$coefs), 
			logLik = logLik, predict = predict, df = fit$df)
	})
	z@preproc.y <- function(y) { # y model.response (factor or numeric vector)
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
#print(lev)
		n <- nrow(x)
		#w <- w/sum(w) * n
		if (is.null(lev))
			fit <- wsvm(x, as.vector(y), case.weights = w, probability = TRUE, ...)
		else
			fit <- wsvm(x, factor(y, levels = lev), case.weights = w, probability = TRUE, ...)
#print(w)
#print(str(fit))
#d[[i]] <<- fit$decision.values
#i <<- i+1
		fit$df <- 10 #??? 
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
