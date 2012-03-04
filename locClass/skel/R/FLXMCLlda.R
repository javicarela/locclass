#' @rdname FLXMCLlda
#' @aliases FLXMCLlda-class
#'
#' @import flexmix
#' @export

setClass("FLXMCLlda", contains = "FLXMCL")



#' This is a model driver for \code{\link[flexmix]{flexmix}} from package \pkg{flexmix} implementing mixtures of Linear Discriminant Analysis Models.
#'
#' @title Mixtures of Linear Discriminant Analysis Models
#'
#' @param formula A formula which is interpreted relative to the formula specified in the call to \code{\link[flexmix]{flexmix}} using \code{\link[stats]{update.formula}}. 
#'   Only the left-hand side (response) of the formula is used. Default is to use the original \code{\link[flexmix]{flexmix}} model formula.
#' @param method Method for scaling the pooled weighted covariance matrix, either \code{"unbiased"} or maximum-likelihood (\code{"ML"}). 
#'   Defaults to \code{"unbiased"}.
#' @param \dots Further arguments to and from other methods.
#'
#' @return Returns an object of class \code{FLXMCLlda} inheriting from \code{FLXMCL}.
#'
#' @rdname FLXMCLlda
# @aliases FLXMCLlda
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
#' model <- FLXMCLlda()
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

FLXMCLlda <- function(formula = . ~ ., method = c("unbiased", "ML"), ...) {
	method <- match.arg(method)
	z <- new("FLXMCLlda", weighted = TRUE, formula = formula,
		name = "Mixture of LDA models")
	z@defineComponent <- expression({
		predict <- function(x, ...) {
			getS3method("predict", "wlda")(fit, newdata = x, ...)$posterior
		}
		logLik <- function(x, y, ...) {
    		post <- getS3method("predict", "wlda")(fit, newdata = x, ...)$posterior
    		n <- nrow(post)
    		return(post[cbind(1:n, as.character(y))])
		}
		new("FLXcomponent", parameters = list(prior = fit$prior, means = fit$means, cov = fit$cov), 
			logLik = logLik, predict = predict, df = fit$df)
	})
	z@preproc.y <- function(grouping) {
    	if (!is.factor(grouping)) 
        	warning("'grouping' was coerced to a factor")
	    g <- as.factor(grouping)
	    lev <- levels(grouping)
    	g <- as.matrix(g)
    	attr(g, "lev") <- lev
		g
	}
	z@fit <- function(x, y, w) {
#print(attr(y, "lev"))
		fit <- wlda(x, factor(y, levels = attr(y, "lev")), weights = w, method = method)
#print(fit$method)
		K <- nrow(fit$means)
		d <- ncol(fit$means)
		fit$df <- K*d + d*(d-1)/2
		with(fit, eval(z@defineComponent))
	}
	z
}


#' @rdname FLXMCLlda
#' @aliases FLXgetModelmatrix,FLXMCLlda-method
#'
#' @import flexmix
#' @export
#'
#' @docType methods

setMethod("FLXgetModelmatrix", signature(model = "FLXMCLlda"), 
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
    attr(model@terms, "intercept") <- 0  ## intercept removed
    X <- model.matrix(model@terms, data = mf)
    model@contrasts <- attr(X, "contrasts")
    model@x <- X
    model@x <- model@preproc.x(model@x)
    model@xlevels <- .getXlevels(model@terms, mf)
    model
})
