#' Combine Model-based Recursive Partitioning with Support Vector Machines.
#'
#' This page lists all ingredients to combine Support Vector Machines with Model-Based Recursive Partitioning
#' (\code{\link[party]{mob}} from package \pkg{party}). See the example for how to do that.
#'
#' \code{svmModel} is an object of class \code{\link[modeltools]{StatModel-class}} implemented in package \pkg{modeltools} that
#' provides an infra-structure for an unfitted \code{\link{wsda}} model.
#'
#' Moreover, methods for \code{\link{wsvm}} and \code{svmModel} objects for the generic functions
#' \code{\link[party]{reweight}}, \code{\link[stats]{deviance}}, \code{\link[sandwich]{estfun}}, and
#' \code{\link[stats]{predict}} are provided.
#'
#' @title Combine Model-based Recursive Partitioning with Support Vector Machines
#'
#' @param object An object of class "svmModel" and "wsvm", respectively.
#' @param x An object of class "wsvm".
#' @param weights A vector of observation weights.
#' @param out Should class labels or posterior probabilities be returned?
#' @param \dots Further arguments.
#'
#' @return 
#' \code{reweight}: The re-weighted fitted "svmModel" object. \cr
#' \code{deviance}: The value of the objective function extracted from \code{object}. \cr
#' \code{estfun}: The empirical estimating (or score) function, i.e. the derivatives of the objective function with respect
#'   to the parameters, evaluated at the training data. \cr
#' \code{predict}: Either a vector of predicted class labels, a matrix of decision values or a matrix of class posterior probabilities.
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
#' fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = svmModel, kernel = "linear",
#' control = mob_control(objfun = deviance, minsplit = 200))
#'
#' ## predict decision values
#' dec <- predict(fit, newdata = grid, out = "decision")
#' 
#' image(x, x, matrix(dec, length(x)), xlab = "x.1", ylab = "x.2")
#' contour(x, x, matrix(dec, length(x)), levels = 0, add = TRUE)
#' points(data$x, pch = as.character(data$y))
#' 
#' ## predict node membership
#' splits <- predict(fit, newdata = grid, type = "node")
#' contour(x, x, matrix(splits, length(x)), levels = min(splits):max(splits), add = TRUE, lty = 2)
#'
#' @rdname svmModel 
#'
#' @import party
#' @importFrom sandwich estfun
#' @export

svmModel <- new("StatModel",
	name = "support vector machine",
	dpp = function(formula, data = list(), subset = NULL, na.action = NULL, 
			frame = NULL, enclos = sys.frame(sys.nframe()), other = list(), 
    		designMatrix = TRUE, responseMatrix = TRUE, setHook = NULL, ...) {
    		mf <- match.call(expand.dots = FALSE)
    		m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
		    mf <- mf[c(1, m)]
    		mf[[1]] <- as.name("model.frame")
    		mf$na.action <- stats::na.pass
#cat("mf\n")
#print(mf)
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
#cat("MF[,1]")
#print(MF[,1])
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
#cat("MEF\n")
#print(str(MEF))
       	MEF
		},
	fit = function (object, weights = NULL, ..., scale = TRUE) {
# function (formula, data = NULL, case.weights = rep(1, nrow(data)), 
    # ..., subset, na.action = na.omit, scale = TRUE) 		    
    		# call <- match.call()
    		# if (!inherits(formula, "formula")) 
        		# stop("method is only for formula objects")
    		# m <- match.call(expand.dots = FALSE)
    		# if (identical(class(eval.parent(m$data)), "matrix")) 
        		# m$data <- as.data.frame(eval.parent(m$data))
    		# m$... <- NULL
    		# m$scale <- NULL
    		# m$cw <- case.weights
    		# m[[1]] <- as.name("model.frame")
    		# m$na.action <- na.action
    		# m <- eval(m, parent.frame())
    		Terms <- attr(object@get("input"), "terms")
#print(1)
    		# cw <- m[, "(cw)"]
    		# attr(Terms, "intercept") <- 0
    		x <- object@get("designMatrix")
    		y <- object@get("responseMatrix")
#print(2)
    		# attr(x, "na.action") <- attr(y, "na.action") <- attr(weights, 
        		# "na.action") <- attr(m, "na.action")
    		if (length(scale) == 1) 
        		scale <- rep(scale, ncol(x))
#print(3)
    		if (any(scale)) {
        		remove <- unique(c(which(labels(Terms) %in% names(attr(x, 
            		"contrasts"))), which(!scale)))
        		scale <- !attr(x, "assign") %in% remove
    		}
#print(4)
			if (is.null(weights)) {
				z <- wsvm(x, y, scale = scale, ...)#, na.action = na.action)
			} else {
				z <- wsvm(x, y, scale = scale, case.weights = weights, 
        		...)#, na.action = na.action)
			}
    		# ret <- wsvm.default(x, y, scale = scale, case.weights = cw, 
        		# ..., na.action = na.action)
    		z$case.weights <- weights
    		# ret$call <- call
    		# ret$call[[1]] <- as.name("wsvm")
    		z$terms <- Terms
    		# if (!is.null(attr(m, "na.action"))) 
        		# ret$na.action <- attr(m, "na.action")
    		# class(ret) <- c("wsvm.formula", class(ret))
    		# return(ret)
    		class(z) <- c("svmModel", "wsvm", "svm")
    		# z$contrasts <- attr(x, "contrasts")
    		# z$xlevels <- attr(x, "xlevels")
    		z$predict_response <- function(newdata = NULL) {#### probability, decision.values als Argument für predict?
        		if (!is.null(newdata)) {
            		penv <- new.env()
            		object@set("input", data = newdata, env = penv)
            		dm <- get("designMatrix", envir = penv, inherits = FALSE)
        		} else {
            		dm <- object@get("designMatrix")
        		}
    			# lev1 <- names(z$prior)
    			# ng <- length(lev1)
    			# posterior <- matrix(0, ncol = ng, nrow = nrow(dm), dimnames = list(rownames(dm), lev1))
    			# posterior[, lev1] <- sapply(lev1, function(y) log(z$prior[y]) - 
        			# 0.5 * mahalanobis(dm, center = z$means[y, ], cov = z$cov))
    			# gr <- factor(lev1[max.col(posterior)], levels = z$lev)
			    # names(gr) <- rownames(dm)
# stop("predict_response aufgerufen")
        		# return(gr)
    		}
    		z$addargs <- list(...)
    		z$ModelEnv <- object
    		z$statmodel <- svmModel
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
	

	
#' @rdname svmModel
#'
#' @method reweight svmModel
#' @S3method reweight svmModel
	
reweight.svmModel <- function (object, weights, ...) {
    fit <- svmModel@fit
    do.call("fit", c(list(object = object$ModelEnv, weights = weights), object$addargs))
}



#' @noRd
#'
#' @method model.matrix svmModel
#' @S3method model.matrix svmModel

model.matrix.svmModel <- function (object, ...) 
	object$ModelEnv@get("designMatrix")



#' @noRd

model.response.svmModel <- function (object, ...)
	object$ModelEnv@get("responseMatrix")



#' @rdname svmModel
#'
#' @method deviance wsvm
#' @S3method deviance wsvm

## dual objective function for wsvm (to minimize)
deviance.wsvm <- function (object, ...) {
	return(sum(object$obj))
}



#' @rdname svmModel
#'
#' @method estfun wsvm
#' @S3method estfun wsvm

estfun.wsvm <- function(x, ...) {
    wts <- weights(x)
    if (is.null(wts)) 
        wts <- 1
	## resort coefs
	n <- length(wts)
	K <- x$nclasses  # 2 for regression and one-class
#cat("K", K, "\n")
	levels <- x$levels
	labels <- x$labels
	d1 <- matrix(0, n, K - 1)
	d1[x$index,] <- x$coefs
	if (K > 2) {
		y <- model.response.svmModel(x, ...)
		coefs <- matrix(0, n, K*(K-1)/2)
		colns <- character(K*(K-1)/2)
		for (i in 1:(K - 1)) {
    		for (j in (i + 1):K) {
                colns[(i - 1)*K - i*(i-1)/2 + j - i] <- 
                           paste(levels[labels[i]],
                                 "/", levels[labels[j]],
                                 sep = "")
#print((i - 1)*K - i*(i-1)/2 + j - i)
    			idx <- y == levels[labels[i]]
    			coefs[idx,(i-1)*K - i*(i-1)/2 + j-i] <- d1[idx,j-1]
    			idx <- y == levels[labels[j]]
				coefs[idx,(i-1)*K - i*(i-1)/2 + j-i] <- d1[idx,i]    				
    		}
    	}
		colnames(coefs) <- colns
    	d1 <- coefs
    } 
	## d2
	#idx <- abs(d1) < x$cost & abs(d1) > 0
	#predict.wsvm(newdata = xmat[idx[,1],], decision.values = TRUE)
	# calculate mean of margin vectors for all two-class problems
	#xmat <- ...
	#x$cost

	# calculate kernel values for all training observations
	
	# calculate score function for all two-class problems
	#d2[,] <- x$rho[] - d1[] * kernel[]        
#cat("d\n")
#print(d1)
#print(wts * d1)
#print(colSums(wts*d1))
    return(wts * d1)
}



#' @rdname svmModel
#'
#' @method predict svmModel
#' @S3method predict svmModel

## todo: class labels can be interchanged: correct sign of decision.values
predict.svmModel <- function(object, out = c("class", "posterior", "decision"), ...) {
	pred <- NextMethod(object, ...)
# m <- match.call(expand.dots = FALSE)
# print(m$...)
	out <- match.arg(out)
	pred <- switch(out,
		class = NextMethod(object, ...),
		posterior = {
			pred <- NextMethod(object, probability = TRUE, ...)
#print(pred)
			post <- attr(pred, "probabilities")
			lapply(seq_len(nrow(post)), function(i) post[i,])
		},
		decision = {
			pred <- NextMethod(object, decision.values = TRUE, ...)			
			decision <- attr(pred, "decision.values")
			lapply(seq_len(nrow(decision)), function(i) decision[i,])
		})
	return(pred)
}
