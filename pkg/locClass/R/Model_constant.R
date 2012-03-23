#' Combine Model-Based Recursive Partitioning with a Constant Classifier.
#'
#' This page lists all ingredients to combine a Constant Classifier with Model-Based Recursive Partitioning
#' (\code{\link[party]{mob}} from package \pkg{party}). See the example for how to do that.
#'
#' \code{constantModel} is an object of class \code{\link[modeltools]{StatModel-class}} implemented in package \pkg{modeltools} that
#' provides an infra-structure for an unfitted \code{\link{constant}} model.
#'
#' Moreover, methods for \code{\link{constant}} and \code{constantModel} objects for the generic functions
#' \code{\link[party]{reweight}}, \code{\link[stats]{deviance}}, \code{\link[sandwich]{estfun}}, and
#' \code{\link[stats]{predict}} are provided.
#'
#' @title Combine Model-based Recursive Partitioning with a Constant Classifier
#'
#' @param object An object of class "constantModel" and "constant", respectively.
#' @param x An object of class "constant".
#' @param weights A vector of observation weights.
#' @param out Should class labels or posterior probabilities be returned?
#' @param \dots Further arguments.
#'
#' @return 
#' \code{reweight}: The re-weighted fitted "constant Model" object. \cr
#' \code{deviance}: The value of the deviance for the Constant Classifier extracted from \code{object}, i.e. the log-likelihood. \cr
#' \code{estfun}: The empirical estimating (or score) function for the Constant Classifier, i.e. the derivatives of the log-likelihood with respect
#'   to the parameters, evaluated at the training data. \cr
#' \code{predict}: Either a vector of predicted class labels or a matrix of class posterior probabilities.
#'
#' @seealso \code{\link[party]{reweight}}, \code{\link[stats]{deviance}}, \code{\link[sandwich]{estfun}}, \code{\link[stats]{predict}}.
#'
#' @references 
#' Zeileis, A., Hothorn, T. and Kornik, K. (2008), Model-based recursive partitioning. 
#' \emph{Journal of Computational and Graphical Statistics}, \bold{17(2)} 492--514.
#'
#' @examples
#' library(locClassData)
#' library(party)
#'
#' data <- vData(500)
#' x <- seq(0,1,0.05)
#' grid <- expand.grid(x.1 = x, x.2 = x)
#' 
#' fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = constantModel,
#' control = mob_control(objfun = deviance, minsplit = 20))
#'
#' ## predict posterior probabilities
#' pred <- predict(fit, newdata = grid, out = "posterior")
#' post <- matrix(0, length(pred), 2)
#' colnames(post) = 1:2
#' for (i in seq_along(pred))
#'     post[i, colnames(pred[[i]])] = pred[[i]]
#' 
#' image(x, x, matrix(as.numeric(post[,1]), length(x)), xlab = "x.1", ylab = "x.2")
#' contour(x, x, matrix(as.numeric(post[,1]), length(x)), levels = 0.5, add = TRUE)
#' points(data$x, pch = as.character(data$y))
#' 
#' ## predict node membership
#' splits <- predict(fit, newdata = grid, type = "node")
#' contour(x, x, matrix(splits, length(x)), levels = min(splits):max(splits), add = TRUE, lty = 2)
#'
#' @rdname constantModel 
#'
#' @import party
#' @importFrom sandwich estfun
#' @export

constantModel <- new("StatModel",
	name = "constant classifier",
	dpp = function(formula, data = list(), subset = NULL, na.action = NULL, 
			frame = NULL, enclos = sys.frame(sys.nframe()), other = list(), 
    		designMatrix = TRUE, responseMatrix = TRUE, setHook = NULL, ...) {
    		mf <- match.call(expand.dots = FALSE)
    		m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
		    mf <- mf[c(1, m)]
    		mf[[1]] <- as.name("model.frame")
    		mf$na.action <- stats::na.pass
    		MEF <- new("ModelEnvFormula")
    		MEF@formula <- c(modeltools:::ParseFormula(formula, data = data)@formula, 
        		other)
    		MEF@hooks$set <- setHook
    		if (is.null(frame)) 
        		frame <- parent.frame()
    		mf$subset <- try(subset)
    		if (inherits(mf$subset, "try-error")) 
        		mf$subset <- NULL
    		MEF@get <- function(which, data = NULL, frame = parent.frame(), 
        		envir = MEF@env) {
        		if (is.null(data)) 
          	  		RET <- get(which, envir = envir, inherits = FALSE)
        		else {
            		oldData <- get(which, envir = envir, inherits = FALSE)
            		if (!use.subset) 
                		mf$subset <- NULL
            		mf$data <- data
            		mf$formula <- MEF@formula[[which]]
            		RET <- eval(mf, frame, enclos = enclos)
            		modeltools:::checkData(oldData, RET)
        		}
        		return(RET)
    		}
    		MEF@set <- function(which = NULL, data = NULL, frame = parent.frame(), 
        		envir = MEF@env) {
        		if (is.null(which)) 
            		which <- names(MEF@formula)
        		if (any(duplicated(which))) 
            		stop("Some model terms used more than once")
        		for (name in which) {
            		if (length(MEF@formula[[name]]) != 2) 
                		stop("Invalid formula for ", sQuote(name))
            		mf$data <- data
            		mf$formula <- MEF@formula[[name]]
            		if (!use.subset) 
                		mf$subset <- NULL
            		MF <- eval(mf, frame, enclos = enclos)
            		if (exists(name, envir = envir, inherits = FALSE)) 
                		modeltools:::checkData(get(name, envir = envir, inherits = FALSE), 
                  		MF)
            		assign(name, MF, envir = envir)
            		mt <- attr(MF, "terms")
            		if (name == "input" && designMatrix) {
                		attr(mt, "intercept") <- 0
                		assign("designMatrix", model.matrix(mt, data = MF, 
                  		...), envir = envir)
            		}
            		if (name == "response" && responseMatrix) {
                		assign("responseMatrix", MF[,1], envir = envir)
            		}
        		}
        		MEapply(MEF, MEF@hooks$set, clone = FALSE)
    		}
    		use.subset <- TRUE
    		MEF@set(which = NULL, data = data, frame = frame)
    		use.subset <- FALSE
    		if (!is.null(na.action)) 
        		MEF <- na.action(MEF)
	       	MEF
		},
	fit = function (object, weights = NULL, ...) {
    		if (is.null(weights)) {
       			z <- constant(object@get("designMatrix"), object@get("responseMatrix"), ...)
    		} else {
        		z <- constant(object@get("designMatrix"), object@get("responseMatrix"),
            		weights = weights, ...)
    		}
    		class(z) <- c("constantModel", "constant")
    		z$terms <- attr(object@get("input"), "terms")
    		z$contrasts <- attr(object@get("designMatrix"), "contrasts")
    		z$xlevels <- attr(object@get("designMatrix"), "xlevels")
    		z$predict_response <- function(newdata = NULL) {#### prior as argument for predict?
        		if (!is.null(newdata)) {
            		penv <- new.env()
            		object@set("input", data = newdata, env = penv)
            		dm <- get("designMatrix", envir = penv, inherits = FALSE)
        		} else {
            		dm <- object@get("designMatrix")
        		}
    			lev1 <- names(z$prior)
    			ng <- length(lev1)
				posterior <- matrix(z$prior, nrow = nrow(dm), ncol = ng, byrow = TRUE)
				dimnames(posterior) <- list(rownames(dm), lev1)
    			gr <- factor(lev1[max.col(posterior)], levels = z$lev)
			    names(gr) <- rownames(dm)
        		return(gr)
    		}
    		z$addargs <- list(...)
    		z$ModelEnv <- object
    		z$statmodel <- constantModel
   		 	z
		},
	predict = function (object, newdata = NULL, ...) {
			object$predict_response(newdata = newdata)
		},
	capabilities = new("StatModelCapabilities",
		weights = TRUE,
		subset = TRUE
	)
)
	


#' @rdname constantModel
#'
#' @method reweight constantModel
#' @S3method reweight constantModel
	
reweight.constantModel <- function (object, weights, ...) {
    fit <- constantModel@fit
    do.call("fit", c(list(object = object$ModelEnv, weights = weights), object$addargs))
}



#' @noRd
#'
#' @method model.matrix constantModel
#' @S3method model.matrix constantModel

model.matrix.constantModel <- function (object, ...) 
	object$ModelEnv@get("designMatrix")



#' @noRd

model.response.constantModel <- function (object, ...)
	object$ModelEnv@get("responseMatrix")



#' @rdname constant Model
#'
#' @method deviance constant
#' @S3method deviance constant

## negative log-likelihood for constant
## wts is 0 or 1
## if classes are missing in the training data their weights are 0
## instead of calculating the quantities for all observations and then multipliying by 0 or 1 before summing them up
## calculate them only for those observations with weights 1
deviance.constant <- function (object, ...) {
    wts <- weights(object)
    if (is.null(wts)) 
        wts <- 1
	#xmat <- model.matrix(object, ...)[wts == 1,]    
    gr <- model.response.constantModel(object, ...)[wts > 0]
# print(gr)
# print(object$prior)
    pr <- object$prior[as.character(gr)]
# print(pr)
    return(-sum(log(pr)))
    # z <- xmat - object$means[as.character(gr),]
    # return(-sum(log(pr) - 0.5 * determinant(object$cov)$modulus - 0.5 * mahalanobis(z, 0, object$cov)))
}



#' @rdname constantModel
#'
#' @method estfun constant
#' @S3method estfun constant

estfun.constant <- function(x, ...) {
    wts <- weights(x)
    if (is.null(wts)) 
        wts <- 1
    gr <- model.response.constantModel(x, ...)
    K <- length(x$prior)
    if (K > 1) {
	    pr <- x$prior[1:(K-1)]
    	lev <- names(x$prior)
    	d <- matrix(0, length(gr), K-1)
		colnames(d) <- lev[1:(K-1)]
    	d[wts > 0, ] <- -1
		row.index <- wts > 0 & gr != lev[K]
		gr <- gr[row.index]	
		col.index <- match(as.character(gr), lev[1:(K-1)], nomatch = 0)
		d[cbind(which(row.index),col.index)] <- 1/x$prior[as.character(gr)] - 1
	} else if (K == 1) {  ## simplify
	    pr <- x$prior
    	lev <- names(x$prior)
    	d <- matrix(0, length(gr), K)
		colnames(d) <- lev
    	d[wts > 0, ] <- -1
		row.index <- wts > 0
		gr <- gr[row.index]	
		col.index <- match(as.character(gr), lev, nomatch = 0)  ## todo: simplify
		d[cbind(which(row.index),col.index)] <- 1/x$prior[as.character(gr)] - 1
	}
	# print(d)
	# print(colSums(d))
    return(d)
}



#' @rdname constantModel
#'
#' @method predict constantModel
#' @S3method predict constantModel

predict.constantModel <- function(object, out = c("class", "posterior"), ...) {
	pred <- NextMethod(object, ...)
	out <- match.arg(out)
	pred <- switch(out,
		class = pred$class,
		posterior = {
			post <- pred$posterior
			lapply(seq_len(nrow(post)), function(i) post[i,, drop = FALSE])
		})
	return(pred)
}
