#' Combine Model-based Recursive Partitioning with Neural Networks.
#'
#' This page lists all ingredients to combine Neural Networks with Model-Based Recursive Partitioning
#' (\code{\link[party]{mob}} from package \pkg{party}). See the example for how to do that.
#'
#' \code{nnetModel} is an object of class \code{\link[modeltools]{StatModel-class}} implemented in package \pkg{modeltools} that
#' provides an infra-structure for an unfitted \code{\link[nnet]{nnet}} model.
#'
#' Moreover, methods for \code{\link[nnet]{nnet}} and \code{nnetModel} objects for the generic functions
#' \code{\link[party]{reweight}}, \code{\link[stats]{deviance}}, \code{\link[sandwich]{estfun}}, and
#' \code{\link[stats]{predict}} are provided.
#'
#' @title Combine Model-based Recursive Partitioning with Neural Networks
#' 
#' @param object An object of class "nnetModel" and "nnet", respectively.
#' @param x An object of class "nnet".
#' @param weights A vector of observation weights.
#' @param out Should class labels or posterior probabilities be returned?
#' @param \dots Further arguments.
#'
#' @return 
#' \code{reweight}: The re-weighted fitted "nnetModel" object. \cr
#' \code{deviance}: The value of the objective function extracted from \code{object}. \cr
#' \code{estfun}: The empirical estimating (or score) function, i.e. the derivatives of the objective function with respect
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
#' fit <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = data, model = nnetModel, size = 1, trace = FALSE,
#' control = mob_control(objfun = deviance, minsplit = 200))
#'
#' ## predict posterior probabilities
#' pred <- predict(fit, newdata = grid, out = "posterior")
#' post <- do.call("rbind", pred)
#' 
#' image(x, x, matrix(as.numeric(post[,1]), length(x)), xlab = "x.1", ylab = "x.2")
#' contour(x, x, matrix(as.numeric(post[,1]), length(x)), levels = 0.5, add = TRUE)
#' points(data$x, pch = as.character(data$y))
#' 
#' ## predict node membership
#' splits <- predict(fit, newdata = grid, type = "node")
#' contour(x, x, matrix(splits, length(x)), levels = min(splits):max(splits), add = TRUE, lty = 2)
#'
#' @rdname nnetModel
#'
#' @import party
#' @export

nnetModel <- new("StatModel",
	name = "neural networks",
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
            			assign("responseMatrix", MF[,1:ncol(MF)], envir = envir)
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
    		class.ind <- function(cl) {
        		n <- length(cl)
        		x <- matrix(0, n, length(levels(cl)))
        		x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
        		dimnames(x) <- list(names(cl), levels(cl))
       	 		x
    		}
			y <- object@get("responseMatrix")
#cat("y----\n")
#print(str(y))
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
        		if (length(lev) == 2L) {
#       print("2 classes")
            		y <- as.vector(unclass(y)) - 1
            		if (is.null(weights)) {
            			z <- mynnet.default(object@get("designMatrix"), y, entropy = TRUE, ...)
				    } else {
            			z <- mynnet.default(object@get("designMatrix"), y, weights = weights, entropy = TRUE, ...)
				    }
				    z$lev <- lev
				} else {
#		print("more than 2 classes")
            		y <- class.ind(y)
            		if (is.null(weights)) {
            			z <- mynnet.default(object@get("designMatrix"), y, softmax = TRUE, ...)
				    } else {
            			z <- mynnet.default(object@get("designMatrix"), y, weights = weights, softmax = TRUE, ...)
				    }
				    z$lev <- lev
       			 }
			} else {
#    	print("y not a factor")
	    		if (is.null(weights)) {
    	   			z <- mynnet.default(object@get("designMatrix"), y, ...)
    			} else {
        			z <- mynnet.default(object@get("designMatrix"), y, weights = weights, ...) 
    			}
    		}
    		class(z) <- c("nnetModel", "nnet")
    		z$terms <- attr(object@get("input"), "terms")
    		z$coefnames <- colnames(object@get("designMatrix"))
    		z$contrasts <- attr(object@get("designMatrix"), "contrasts")
    		z$xlevels <- attr(object@get("designMatrix"), "xlevels")
    		z$predict_response <- function(newdata = NULL) {#### prior als Argument für predict?
        		if (!is.null(newdata)) {
            		penv <- new.env()
            		object@set("input", data = newdata, env = penv)
            		dm <- get("designMatrix", envir = penv, inherits = FALSE)
        		} else {
            		dm <- object@get("designMatrix")
        		}
    		}
    		z$addargs <- list(...)
    		z$ModelEnv <- object
    		z$statmodel <- nnetModel
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

	
	
#' @rdname nnetModel
#'
#' @method reweight nnetModel
#' @S3method reweight nnetModel
#' @import party
	
reweight.nnetModel <- function (object, weights, ...) {
    fit <- nnetModel@fit
    do.call("fit", c(list(object = object$ModelEnv, weights = weights), object$addargs))
}



#' @rdname nnetModel
#'
#' @method deviance nnet
#' @S3method deviance nnet
#' @importFrom stats deviance

deviance.nnet <- function (object, ...) {
	return(object$value)
}



#' @rdname nnetModel
#'
#' @method estfun nnet
#' @S3method estfun nnet
#' @importFrom sandwich estfun

estfun.nnet <- function(x, ...) {
	# print(rowSums(x$gradient[,29:33]))
	# print(x$gradient)
	# print(colSums(x$gradient))
	# print(str(x))
	# print(x$weights)
	# n <- sum(x$weights)
	# print(n)
	# print(cp <- crossprod((x$gradient[x$weights > 0,])/sqrt(n)))
	# print(solve(cp))
	# e <- eigen(crossprod((x$gradient[x$weights > 0,])/sqrt(n)), only.values = TRUE)$values
	# print(e)
	# print(eigen(cov(x$gradient[x$weights > 0,]), only.values = TRUE))
	return(x$gradient)
}



#' @rdname nnetModel
#'
#' @method predict nnetModel
#' @S3method predict nnetModel

predict.nnetModel <- function(object, out = c("class", "posterior"), ...) {
	out <- match.arg(out)
	pred <- switch(out,
		class = {
			cl <- NextMethod(object, type = "class", ...)
			factor(cl, levels = object$lev)	
		},
		posterior = {
			post <- NextMethod(object, type = "raw", ...)
			if (ncol(post) == 1)
				post = cbind(1 - post, post)
			#print(str(post))
			#print(object$lev)
			colnames(post) <- object$lev
			lapply(seq_len(nrow(post)), function(i) post[i,, drop = FALSE])
		})
	return(pred)
}



# mynnet <- function (x, ...) 
	# UseMethod("mynnet")



# mynnet.formula <- function (formula, data, weights, ..., subset, na.action, contrasts = NULL) {
    # m <- match.call(expand.dots = FALSE)
    # if (is.matrix(eval.parent(m$data))) 
        # m$data <- as.data.frame(data)
    # m$... <- m$contrasts <- NULL
    # m[[1L]] <- as.name("model.frame")
    # m <- eval.parent(m)
    # Terms <- attr(m, "terms")
    # x <- model.matrix(Terms, m, contrasts)
    # cons <- attr(x, "contrast")
    # xint <- match("(Intercept)", colnames(x), nomatch = 0L)
    # if (xint > 0L) 
        # x <- x[, -xint, drop = FALSE]
    # w <- model.weights(m)
    # if (length(w) == 0L) 
        # w <- rep(1, nrow(x))
    # y <- model.response(m)
    # if (is.factor(y)) {
        # lev <- levels(y)
        # counts <- table(y)
        # if (any(counts == 0L)) {
            # empty <- lev[counts == 0L]
            # warning(sprintf(ngettext(length(empty), "group %s is empty", 
                # "groups %s are empty"), paste(empty, collapse = " ")), 
                # domain = NA)
            # y <- factor(y, levels = lev[counts > 0L])
        # }
        # if (length(lev) == 2L) {
            # y <- as.vector(unclass(y)) - 1
            # res <- mynnet.default(x, y, w, entropy = TRUE, ...)
            # res$lev <- lev
        # }
        # else {
            # y <- class.ind(y)
            # res <- mynnet.default(x, y, w, softmax = TRUE, ...)
            # res$lev <- lev
        # }
    # }
    # else res <- mynnet.default(x, y, w, ...)
    # res$terms <- Terms
    # res$coefnames <- colnames(x)
    # res$call <- match.call()
    # res$na.action <- attr(m, "na.action")
    # res$contrasts <- cons
    # res$xlevels <- .getXlevels(Terms, m)
    # class(res) <- c("nnet.formula", "nnet")
    # res
# }



#' @noRd

mynnet.default <- function (x, y, weights, size, Wts, mask = rep(TRUE, length(wts)), 
    linout = FALSE, entropy = FALSE, softmax = FALSE, censored = FALSE, 
    skip = FALSE, rang = 0.7, decay = 0, maxit = 100, Hess = FALSE, 
    trace = TRUE, MaxNWts = 1000, abstol = 1e-04, reltol = 1e-08, 
    ...) {
    net <- NULL
    x <- as.matrix(x)
    y <- as.matrix(y)
    if (any(is.na(x))) 
        stop("missing values in 'x'")
    if (any(is.na(y))) 
        stop("missing values in 'y'")
    if (dim(x)[1L] != dim(y)[1L]) 
        stop("nrows of 'x' and 'y' must match")
    if (linout && entropy) 
        stop("entropy fit only for logistic units")
    if (softmax) {
        linout <- TRUE
        entropy <- FALSE
    }
    if (censored) {
        linout <- TRUE
        entropy <- FALSE
        softmax <- TRUE
    }
    net$n <- c(dim(x)[2L], size, dim(y)[2L])
    net$nunits <- as.integer(1L + sum(net$n))
    net$nconn <- rep(0, net$nunits + 1L)
    net$conn <- numeric(0L)
    net <- norm.net(net)
    if (skip) 
        net <- add.net(net, seq(1L, net$n[1L]), seq(1L + net$n[1L] + 
            net$n[2L], net$nunits - 1L))
    if ((nwts <- length(net$conn)) == 0) 
        stop("no weights to fit")
    if (nwts > MaxNWts) 
        stop(gettextf("too many (%d) weights", nwts), domain = NA)
    nsunits <- net$nunits
    if (linout) 
        nsunits <- net$nunits - net$n[3L]
    net$nsunits <- nsunits
    net$decay <- decay
    net$entropy <- entropy
    if (softmax && NCOL(y) < 2L) 
        stop("'softmax = TRUE' requires at least two response categories")
    net$softmax <- softmax
    net$censored <- censored
    if (missing(Wts)) 
        if (rang > 0) 
            wts <- runif(nwts, -rang, rang)
        else wts <- rep(0, nwts)
    else wts <- Wts
    if (length(wts) != nwts) 
        stop("weights vector of incorrect length")
    if (length(mask) != length(wts)) 
        stop("incorrect length of 'mask'")
    if (trace) {
        cat("# weights: ", length(wts))
        nw <- sum(mask != 0)
        if (nw < length(wts)) 
            cat(" (", nw, " variable)\n", sep = "")
        else cat("\n")
        flush.console()
    }
    if (length(decay) == 1L) 
        decay <- rep(decay, length(wts))
    .C("VR_set_net", as.integer(net$n), as.integer(net$nconn), 
        as.integer(net$conn), as.double(decay), as.integer(nsunits), 
        as.integer(entropy), as.integer(softmax), as.integer(censored))
    ntr <- dim(x)[1L]
    nout <- dim(y)[2L]
    if (missing(weights)) 
        weights <- rep(1, ntr)
    if (length(weights) != ntr || any(weights < 0)) 
        stop("invalid weights vector")
    Z <- as.double(cbind(x, y))
    storage.mode(weights) <- "double"
    tmp <- .C("VR_dovm", as.integer(ntr), Z, weights, as.integer(length(wts)), 
        wts = as.double(wts), val = double(1), as.integer(maxit), 
        as.logical(trace), as.integer(mask), as.double(abstol), 
        as.double(reltol), ifail = integer(1L))
    net$value <- tmp$val
    net$wts <- tmp$wts
    net$convergence <- tmp$ifail
    tmp <- matrix(.C("VR_nntest", as.integer(ntr), Z, tclass = double(ntr * 
        nout), as.double(net$wts))$tclass, ntr, nout)
    dimnames(tmp) <- list(rownames(x), colnames(y))
    net$fitted.values <- tmp
    tmp <- y - tmp
    dimnames(tmp) <- list(rownames(x), colnames(y))
    net$residuals <- tmp
    z <- .C("VR_dfunc2", as.double(net$wts), dfn = double(length(net$wts) * ntr))
	net$gradient <- matrix(z$dfn, nrow = ntr, byrow = TRUE)[,mask]
    #z <- .C("VR_dfunc", as.double(net$wts), df = double(length(net$wts)), fp = as.double(1))
	#net$gr <- z$df
    .C("VR_unset_net")
    if (entropy) 
        net$lev <- c("0", "1")
    if (softmax) 
        net$lev <- colnames(y)
    net$call <- match.call()
    if (Hess) 
        net$Hessian <- nnetHess(net, x, y, weights)
    net$weights <- weights
    class(net) <- "nnet"
    net
}


#' @import nnet

norm.net <- nnet:::norm.net



# root.matrix <- function (X) {
    # if ((ncol(X) == 1) && (nrow(X) == 1)) 
        # return(sqrt(X))
    # else {
        # X.eigen <- eigen(X, symmetric = TRUE)
		# zero.values <- sapply(X.eigen$values, all.equal, current = 0)
		# X.eigen$values[zero.values == "TRUE" & X.eigen$values < 0] <- 0
        # if (any(X.eigen$values < 0)) 
            # stop("matrix is not positive semidefinite")
        # sqomega <- sqrt(diag(X.eigen$values))
        # V <- X.eigen$vectors
        # return(V %*% sqomega %*% t(V))
    # }
# }
