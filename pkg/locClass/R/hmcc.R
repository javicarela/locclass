#  Copyright (C) 2011 J. Schiffner
#  Copyrights... mda, MASS
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

#' Fit the hierarchical mixture model or the common components model for Gaussian mixture-based classification.  
#'
#' This function fits the hierarchical mixture model (Titsias and Likas, 2002) or the common components model
#' (Titsias and Likas, 2001) for Gaussian mixture-based classification, that is the class conditional distributions 
#' are modeled as Gaussian mixtures.
#'
#' If \code{method = "cc"} the common components model is fitted. It is given as 
#' \deqn{f(x\,|\,\theta) = \sum_{k=1}^K p_k \sum_{j=1}^J \pi_{jk} \phi(x\,|\,\mu_j, \Sigma_j)}{%
#'       f(x | theta) = sum_{k=1}^K P.k sum_{j=1}^J Pi.jk  phi(x | mu.j, Sigma.j)}
#' where      
#' \describe{
#' \item{\eqn{K}:}{the number of classes,}
#' \item{\eqn{J:}}{the number of mixture components,}
#' \item{\eqn{p_k:}{P.k}}{the class priors,}
#' \item{\eqn{\pi_{jk}}{Pi.jk}:}{the mixture weights.}  
#' }
#' The maximum likelihood parameter estimates are computed by means of the EM algorithm (Dempster et al., 1977).
#'
#' If \code{method = "hm1"} or \code{"hm2"} the hierarchical mixture model
#' \deqn{f(x\,|\,\theta) = \sum_{j=1}^J \pi_j \sum_{k=1}^K p_{kj} \phi(x\,|\,\mu_{kj}, \Sigma_{kj})}{%
#'       f(x | theta) = sum_{j=1}^J Pi.j sum_{k=1}^K P.kj  phi(x | mu.kj, Sigma.kj)}
#' is fitted where 
#' \describe{
#' \item{\eqn{J}:}{the number of mixture components,}  
#' \item{\eqn{K}:}{the number of classes,}
#' \item{\eqn{\pi_j}{Pi.j}:}{the mixture weights,}  
#' \item{\eqn{p_{kj}}{P.kj}:}{the conditional class priors.}
#' }
#' The hierarchical structure of the model is reflected when calculating the parameter estimates. 
#' In a first step, a model-based clustering is carried out. For this purpose two different models can be applied:
#' \describe{
#' \item{\code{"hm1"}}{A simple Gaussian mixture model:
#'	 \deqn{f(x\,|\,\psi) = \sum_{j=1}^J \pi_j \phi(x\,|\,\mu_j, \Sigma_j).}{%
#'          f(x | psi) = sum_{j=1}^J Pi.j phi(x | mu.j, Sigma.j).}}
#'  \item{\code{"hm2"}}{The common components model:
#'    \deqn{f(x\,|\,\psi) = \sum_{k=1}^K p_k \sum_{j=1}^J \pi_{jk} \phi(x\,|\,\mu_j, \Sigma_j).}{%
#'          f(x | psi) = sum_{k=1}^K P.k sum_{j=1}^J Pi.jk  phi(x | mu.j, Sigma.j).}}
#' }
#' Based on the estimated cluster membership probabilities obtained in the first step
#' maximum likelihood estimates of \eqn{\pi_j}{Pi.j}, \eqn{p_{kj}}{P.kj}, \eqn{\mu_{kj}}{mu.kj}, and \eqn{\Sigma_{kj}}{Sigma.kj} 
#' are calculated in a second step.
#'  
#' If the estimate of \eqn{p_{kj}}{P.kj} is close to zero it is likely that the \eqn{j}-th mixture component 
#' does not represent data of the \eqn{k}-th class. Therefore, if the estimate of \eqn{p_{kj}}{P.kj} is below 
#' the chosen \code{thr} it is set to zero, i.e. the \eqn{j}-th mixture component of the \eqn{k}-th 
#' class is pruned.
#' By default \code{thr} is set to 
#'   \deqn{\frac{\textrm{the number of predictors} + 1}{\textrm{the number of training observations}}.}{%
#'   (the number of predictors + 1)/(the number of training observations).
#'   }
#'
#' stopping criterion, \code{eps} ???
#'
#' @title Hierarchical Mixture and Common Components Classifier
#'
#' @param formula A \code{formula} of the form \code{groups ~ x1 + x2 + ...}, that is, the response is the grouping \code{factor} 
#' and the right hand side specifies the (non-\code{factor}) discriminators.
#' @param data A \code{data.frame} from which variables specified in \code{formula} are to be taken.
#' @param x (Required if no \code{formula} is given as principal argument.) A \code{matrix} or \code{data.frame} or \code{Matrix} containing the explanatory variables.
#' @param grouping (Required if no \code{formula} is given as principal argument.) A \code{factor} specifying the class membership for each observation.
#' @param J The number of mixture components. See Details below.
#' @param method \code{"hm1"} or \code{"hm2"} for the hierarchical mixture classifier, or \code{"cc"} for the common components classifier. See Details below.
#' @param \dots Further arguments.
#' @param tries The number of random starts. See \code{\link{hmccStart}}.
#' @param iter A limit on the total number of iterations in the EM algorithm.
#' @param eps Stop criterion for the EM algorithm. See Details.
#' @param thr (Required for \code{method}s \code{"hm1"} and \code{"hm2"}.) Threshold for pruning of mixture components. See Details below.
#' @param subset An index vector specifying the cases to be used in the training sample. (NOTE: If given, this argument must be named.) 
#' @param na.action A function to specify the action to be taken if \code{NA}s are found.
#'
#' @return  An object of class \code{"hmcc"} containing the following components:
#' \item{K}{The number of classes.}
#' \item{J}{The number of mixture components.}
#' \item{P.k}{(Only for \code{method} \code{"cc"}.) A vector of class priors.}
#' \item{Pi.jk}{(Only for \code{method} \code{"cc"}.) A matrix of conditional mixture weights.}
#' \item{Pi.j}{(Only for \code{method}s \code{"hm1"} and \code{hm2}.) A vector of mixture weights.}
#' \item{P.kj}{(Only for \code{method}s \code{"hm1"} and \code{hm2}.) A matrix of conditional class priors.}
#' \item{mu.kj}{(Only for \code{method}s \code{"hm1"} and \code{hm2}.) Array of mean vectors. If a mixture component was pruned the correponding entries in \code{mu.kj} are set to \code{NA}.}
#' \item{Sigma.kj}{(Only for \code{method}s \code{"hm1"} and \code{hm2}.) Array of covariance matrices. If a mixture component was pruned the correponding entries in \code{Sigma.kj} are set to \code{NA}.}
#' \item{mu.j}{Array of mean vectors either of the simple mixture model (for \code{method} \code{"hm1"}) or of the common components model (for \code{method}s \code{"cc"} and \code{hm2}).}
#' \item{Sigma.j}{Array of covariance matrices either of the simple mixture model (for \code{method} \code{"hm1"}) or of the common components model (for \code{method}s \code{"cc"} and \code{"hm2"}).}
#' \item{ll}{The maximized log-likelihood.}
#' \item{method}{The \code{method} used (\code{"cc"}, \code{"hm1"}, or \code{"hm2"})}
#' \item{call}{The (matched) function call.}
#'
#' @references 
#' Dempster, A. P., Laird, N. M., Rubin, D. B. (1977), Maximum likelihood from incomplete
#'    data via the EM algorithm. \emph{Journal of the Royal Statistical Society B}, \bold{39(1)}, 
#'    1--38.
#'
#' Titsias, M. K., Likas, A. C. (2001), Shared kernel models for class conditional density estimation.
#'    \emph{IEEE Transactions on Neural Networks}, \bold{12(5)}, 987--997.
#'  
#' Titsias, M. K., Likas, A. C. (2002), Mixture of experts classification using a hierarchical mixture model.
#'    \emph{Neural Computation}, \bold{14}, 2221--2244.
#'
#' @seealso \code{\link{hmccStart}}, \code{\link{predict.hmcc}}. See also \pkg{\link[mda]{mda}} for Gaussian mixture-based classification.
#'
#' @examples
#' library(mlbench)
#' data.train <- as.data.frame(mlbench.waveform(300))
#' data.test <- as.data.frame(mlbench.waveform(200))
#' 
#' ## "cc"
#' fit <- hmcc(classes ~ ., data = data.train, J = 5, method = "cc")
#' pred <- predict(fit, data.test)
#' mean(pred$class != data.test$classes)
#' 
#' ## "hm1"
#' fit <- hmcc(classes ~ ., data = data.train, J = 3, method = "hm1")
#' pred <- predict(fit, data.test)
#' mean(pred$class != data.test$classes)
#' 
#' ## "hm2"
#' fit <- hmcc(classes ~ ., data = data.train, J = 3, method = "hm2")
#' pred <- predict(fit, data.test)
#' mean(pred$class != data.test$classes)
#' 
#' @keywords classif cluster multivariate
#'
#' @aliases hmcc hmcc.data.frame hmcc.default hmcc.formula hmcc.matrix
#'
#' @export

hmcc <- function (x, ...)
    UseMethod("hmcc")



#' @rdname hmcc
#' @method hmcc formula
#'
#' @S3method hmcc formula

hmcc.formula <- function (formula, data, ..., subset, na.action) {
    m <- match.call(expand.dots = FALSE)
    m$... <- NULL
    m[[1L]] <- as.name("model.frame")
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    grouping <- model.response(m)
    x <- model.matrix(Terms, m)
    xint <- match("(Intercept)", colnames(x), nomatch = 0)
    if (xint > 0) 
        x <- x[, -xint, drop = FALSE]
    res <- hmcc.default(x, grouping, ...)
    res$terms <- Terms
    cl <- match.call()
    cl[[1L]] <- as.name("hmcc")
    res$call <- cl
    res$contrasts <- attr(x, "contrasts")
    res$xlevels <- .getXlevels(Terms, m)
    res$na.action <- attr(m, "na.action")
    res
}



#' @rdname hmcc
#' @method hmcc data.frame
#'
#' @S3method hmcc data.frame

hmcc.data.frame <- function (x, ...) {
    res <- hmcc(structure(data.matrix(x, rownames.force = TRUE), class = "matrix"), ...)
    cl <- match.call()
    cl[[1L]] <- as.name("hmcc")
    res$call <- cl
    res
}



#' @rdname hmcc
#' @method hmcc matrix
#'
#' @S3method hmcc matrix

hmcc.matrix <- function (x, grouping, ..., subset, na.action = na.fail) {
    if (!missing(subset)) {
        x <- x[subset, , drop = FALSE]
        grouping <- grouping[subset]
    }
    if (missing(na.action)) {
        if (!is.null(naa <- getOption("na.action"))) {    # if options(na.action = NULL) the default na.fail comes into play
            if(!is.function(naa)) {
                na.action <- get(naa, mode = "function")
            } else {
                na.action <- naa
            }
		}
    } 
    dfr <- na.action(structure(list(g = grouping, x = x), class = "data.frame",
    	 row.names = rownames(x)))
    grouping <- dfr$g
    x <- dfr$x
    res <- hmcc.default(x, grouping, ...)
    cl <- match.call()
    cl[[1L]] <- as.name("hmcc")
    res$call <- cl
 	#res$na.action <- na.action # write na.action in res?
    res
}



#' @rdname hmcc
#' @method hmcc default
#'
#' @S3method hmcc default

hmcc.default <- function (x, grouping, J, method = c("hm1", "hm2", "cc"), ..., tries = 10, iter = 15, eps = 10^(-5), thr) {
    if (is.null(dim(x)))
        stop("'x' is not a matrix")
    x <- as.matrix(x)
    if (any(!is.finite(x)))
        stop("infinite, NA or NaN values in 'x'")
    n <- nrow(x)
	p <- ncol(x)        
    if (n != length(grouping)) 
        stop("'nrow(x)' and 'length(grouping)' are different")
    if (!is.factor(grouping))
        warning("'grouping' was coerced to a factor")
    g <- as.factor(grouping)
    lev <- lev1 <- levels(g)
    counts <- as.vector(table(g))
    if (any(counts == 0)) {
        empty <- lev[counts == 0]
        warning(sprintf(ngettext(length(empty), "class %s is empty", 
            "classes %s are empty"), paste(empty, collapse = ", ")), 
            domain = NA)
        lev1 <- lev[counts > 0]
        g <- factor(g, levels = lev1)
        counts <- as.vector(table(g))
    }
    ng <- length(counts)
    method <- match.arg(method)
    if (missing(thr)) {
        thr <- (p + 1)/n
    } else {
        if (method == "cc")
            warning("'thr' is not required for 'method' \"cc\" and its value will be ignored")
        if (thr < 0 | thr > 1)
            stop("thr is not in [0,1]")    
    }
    
   # object <- list(x = x, grouping = g, J = J, method = method, n = n, d = p, K = ng, n.k = counts, lev1 = lev1, lev = lev, iter = iter, eps = eps, thr = thr)
    
    hmcc <- hmccStart(x, g, J, method, n, counts, p, ng, lev1, lev, tries, iter, eps, thr)
    #hmcc <- hmccFit(compp, x, g, J, method, n, counts, p, ng, lev1, lev, iter, eps, thr)
    # lev dranbasteln???
    cl <- match.call()
    cl[[1]] <- as.name("hmcc")
    return(structure(c(hmcc, call = cl), class = "hmcc"))
}



#' @nord
#'
#' @import mvtnorm

hmccFit <- function (compp, x, grouping, J, method, n, n.k, d, K, lev1, lev, iter, eps, thr) {

    hm1.em <- function (compp, x, J, n, d, iter, eps, ...) { 
        l.alt <- -1/.Machine$double.eps
        count <- 1
        mu.j <- array(0, dim = c(1, d, J), dimnames = list(NULL, colnames(x), 1:J))
        Sigma.j <- array(0, dim = c(d, d, J), dimnames = list(colnames(x), colnames(x), 1:J))
        mu.Sigma.N <- colSums(compp)
        mu.j[,,1:J] <- t(t(compp) %*% x/mu.Sigma.N)
        Sigma.j[,,1:J] <- sapply(1:J, function(y) t(compp[,y] * t(t(x) - mu.j[,,y])) %*% t(t(x) - mu.j[,,y])/mu.Sigma.N[y])
        Pi.j <- mu.Sigma.N/n
        repeat {
            if (count > iter) 
            	break
            compp.Z <- sapply(1:J, function(y) Pi.j[y] * dmvnorm(x, mean = mu.j[,,y], sigma = Sigma.j[,,y]))   
            compp.N <- rowSums(compp.Z)
            compp <- compp.Z/compp.N
            mu.Sigma.N <- colSums(compp)
            mu.j[,,1:J] <- t(t(compp) %*% x/mu.Sigma.N)
            Sigma.j[,,1:J] <- sapply(1:J, function(y) t(compp[,y] * t(t(x) - mu.j[,,y])) %*% t(t(x) - mu.j[,,y])/mu.Sigma.N[y])
            Pi.j <- mu.Sigma.N/n
            l <- sum(log(compp.N))
            if (abs(l - l.alt) < eps) 
            	break     
            l.alt <- l
            count <- count + 1
        }
        return(list(compp = compp, mu.j = mu.j, Sigma.j = Sigma.j, Pi.j = Pi.j, l = l))
    }

    hm2.em <- cc.em <- function (compp, x, grouping, J, n.k, d, K, lev1, iter, eps, ...) {
        l.alt <- -1/.Machine$double.eps
        count <- 1
        mu.j <- array(0, dim = c(1, d, J), dimnames = list(NULL, colnames(x), 1:J))
        Sigma.j <- array(0, dim = c(d, d, J), dimnames = list(colnames(x), colnames(x), 1:J))
        mu.Sigma.N <- colSums(compp)
        mu.j[,,1:J] <- t(t(compp) %*% x/mu.Sigma.N)
        Sigma.j[,,1:J] <- sapply(1:J, function(y) t(compp[, y] * t(t(x) - mu.j[,,y])) %*% t(t(x) - mu.j[,,y])/mu.Sigma.N[y])
        Pi.jk <- tapply(compp, list(rep(grouping, J), col(compp)), sum)
        Pi.jk <- t(Pi.jk/n.k)
        repeat {
            if (count > iter) 
            	break
            compp.Z <- compp
            for(k in 1:K) {
                compp.Z[grouping == lev1[k],] <- sapply(1:J, function(y) Pi.jk[y,k] * dmvnorm(x[grouping == lev1[k],], mean = mu.j[,,y], sigma = Sigma.j[,,y]))
            }  
            compp.N <- rowSums(compp.Z)
            compp <- compp.Z/compp.N
            mu.Sigma.N <- colSums(compp)
            mu.j[,,1:J] <- t(t(compp) %*% x/mu.Sigma.N)
            Sigma.j[,,1:J] <- sapply(1:J, function(y) t(compp[, y] * t(t(x) - mu.j[,,y])) %*% t(t(x) - mu.j[,,y])/mu.Sigma.N[y])
            Pi.jk <- tapply(compp, list(rep(grouping, J), col(compp)), sum)
            Pi.jk <- t(Pi.jk/n.k)
            l <- sum(log(compp.N))
            if (abs(l - l.alt) < eps) 
            	break
            l.alt <- l
            count <- count + 1
        }
        return(list(compp = compp, mu.j = mu.j, Sigma.j = Sigma.j, Pi.jk = Pi.jk, l = l))
    }
       
    hm.fit <- function (compp, x, grouping, J, n, d, K, lev1, thr, ...) {
        P.Z <- tapply(compp, list(rep(grouping, J), col(compp)), sum)
        P.N <- colSums(P.Z)
        Pi.j <- P.N/n
        P.kj <- t(t(P.Z)/P.N)
        prune.index <- P.Z < thr * n
        P.Z[prune.index] <- NA
        P.kj[prune.index] <- 0
        mu.kj <- array(0, dim = c(K, d, J), dimnames = list(lev1, colnames(x), 1:J))
        for(k in 1:K) 
        	mu.kj[k,,1:J] <- t(t(compp[grouping == lev1[k], ]) %*% x[grouping == lev1[k],]/P.Z[k,])
        Sigma.kj <- array(0, dim = c(d, d, K*J))
        dimnames(Sigma.kj) <- list(colnames(x), colnames(x), paste(rep(1:J,K), rep(lev1, each = J), sep = "."))
        for(k in 1:K) 
        	Sigma.kj[,,(1:J) + (k-1)*J] <- sapply(1:J, function(y) t(compp[grouping == lev1[k], y] * t(t(x[grouping == lev1[k],]) - mu.kj[k,,y])) %*% t(t(x[grouping == lev1[k],]) - mu.kj[k,,y])/P.Z[k,y])
        return(list(Pi.j = Pi.j, P.kj = P.kj, mu.kj = mu.kj, Sigma.kj = Sigma.kj, P.Z = P.Z))
    }
 
    cc.fit <- function (Pi.jk, n, n.k, ...) {
        P.k <- n.k/n
        names(P.k) <- lev1
        P.Z <- t(Pi.jk) * n.k
        return(list(P.k = P.k, P.Z = P.Z))
    }                                                                                   
  
    em <- get(paste(method, "em", sep = "."), mode = "function")
    em.res <- em(compp = compp, x = x, grouping = grouping, J = J, n = n, n.k = n.k, d = d, K = K, lev1 = lev1, iter = iter, eps = eps)            
    fit.res <- switch(method, 
        hm1 = hm.fit(compp = em.res$compp, x, grouping, J, n, d, K, lev1, thr),
        hm2 = hm.fit(compp = em.res$compp, x, grouping, J, n, d, K, lev1, thr),
        cc = cc.fit(Pi.jk = em.res$Pi.jk, n, n.k))  
    if (method %in% c("hm1", "hm2"))
        return(structure(list(K = K, J = J, Pi.j = fit.res$Pi.j, P.kj = fit.res$P.kj, mu.kj = fit.res$mu.kj, Sigma.kj = fit.res$Sigma.kj, mu.j = em.res$mu.j, Sigma.j = em.res$Sigma.j, ll = em.res$l, method = method, P.Z = fit.res$P.Z, lev1 = lev1, lev = lev), class = "hmcc"))
    else
        return(structure(list(K = K, J = J, P.k = fit.res$P.k, Pi.jk = em.res$Pi.jk, mu.j = em.res$mu.j, Sigma.j = em.res$Sigma.j, ll = em.res$l, method = method, P.Z = fit.res$P.Z, lev1 = lev1, lev = lev), class = "hmcc"))  
}



#' Calculate initial cluster membership probabilities for the function \code{\link{hmcc}} that fits the hierarchical mixture model 
#' or the common components model.
#'
#' The EM algorithm requires starting values for the cluster membership probabilities.
#' These are obtained by applying the K-means algorithm with \eqn{J} centers.
#' The K-means algorithm again requires starting centers, which are \eqn{J} randomly selected 
#' training observations.
#' 
#' In order to avoid the whole procedure suffering from bad choices of starting 
#' values the common components and hierarchical mixture models are fitted multiple
#' times (specified by \code{tries}) with different sets of random starting values. The
#' class posteriors of the training observations are calculated
#' and the fit with the smallest posterior deviance
#' \deqn{-2 \cdot \sum_{i=1}^n \ln P(y_i\,|\,x_i)}{%
#'       -2 * \sum_{i=1}^n ln P(y_i | x_i)}
#' is chosen. Here, \eqn{n} is the number of training observations and \eqn{y_i} denotes
#' the class label of \eqn{x_i}.
#'
#' @title Initialization of the Hierarchical Mixture Classifier and the Common Components Classifier
#'
#' @param x A \code{matrix} or \code{data.frame} or \code{Matrix} containing the explanatory variables.
#' @param grouping A \code{factor} specifying the class membership for each observation.
#' @param J The number of mixture components. See \code{\link{hmcc}}.
#' @param method \code{"hm1"} or \code{"hm2"} for the hierarchical mixture classifier, or \code{"cc"} for the common components classifier. See \code{\link{hmcc}}.

#' @param n The number of training observations.
#' @param n.k The number of training observations in the \eqn{k}-th class (\eqn{k = 1,\ldots,K}{k = 1,...,K}).
#' @param d The number of explanatory variables.
#' @param K The number of classes.
#' @param lev1 The class labels.
#' @param lev The class labels.
#' @param tries The number of random starts. See Details below.
#' @param iter A limit on the total number of iterations in the EM algorithm.
#' @param eps Stop criterion for the EM algorithm. See \code{\link{hmcc}}.
#' @param thr (Required for \code{method} \code{"hm1"} and \code{"hm2"}.) Threshold for pruning of mixture components. See \code{\link{hmcc}}.
#'
#' @return A matrix containing estimated cluster membership probabilities \eqn{P(j\,|\,x_i)}{P(j|x_i)} of the training observations.
#'
#' @references
#' Dempster, A. P., Laird, N. M., Rubin, D. B. (1977), Maximum likelihood from incomplete
#' data via the EM algorithm. \emph{Journal of the Royal Statistical Society B}, \bold{39(1)}, 
#' 1--38.
#' 
#' Titsias, M. K., Likas, A. C. (2001), Shared kernel models for class conditional density estimation.
#' \emph{IEEE Transactions on Neural Networks}, \bold{12(5)}, 987--997.
#' 
#' Titsias, M. K., Likas, A. C. (2002), Mixture of experts classification using a hierarchical mixture model.
#' \emph{Neural Computation}, \bold{14}, 2221--2244.
#'
#' @seealso \code{\link{hmcc}}, \code{\link{predict.hmcc}}.
#'
#' @keywords classif cluster multivariate
#'
#' @export

hmccStart <- function (x, grouping, J, method, n, n.k, d, K, lev1, lev, tries, iter, eps, thr) {
    best <- function (compp, x, grouping, J, method, n, n.k, d, K, lev1, lev, iter, eps, thr) {
        object <- hmccFit(compp, x, grouping, J, method, n, n.k, d, K, lev1, lev, iter, eps, thr)
        classp <- predict.hmcc(object, x)$posterior
        index <- cbind(seq(along = grouping), grouping)
        classp <- classp[index]
        ll <- -2 * sum(log(classp[classp > .Machine$double.eps]))
       	return(list(ll = ll, object = object))
    }
    best.ll <- 1/.Machine$double.eps
    for (tr in seq_len(tries)) {
        km <- kmeans(x, J)
        compp <- diag(J)[km$cluster, ]
        res <- try(best(compp, x, grouping, J, method, n, n.k, d, K, lev1, lev, iter, eps, thr))
        if (res$ll < best.ll) {
            #keep.compp <- compp
            best.ll <- res$ll
            keep.object <- res$object
        }  
    }
    return(keep.object)
#    return(keep.compp)
}



#' @param x A \code{hmcc} object.
#' @param ... Further arguments to \code{\link{print}}.
#'
#' @method print hmcc
#' @nord
#'
#' @S3method print hmcc

print.hmcc <- function (x, ...) {
    if (!is.null(cl <- x$call)) {
        names(cl)[2] <- ""
        cat("Call:\n")
        dput(cl, control = NULL)
    }
    cat("\nNumber of classes:\n")
    print(x$K, ...)
    cat("\nNumber of mixture components:\n")
    print(x$J, ...)
    if (x$method %in% c("hm1","hm2")) {
        cat("\nParameter estimates for the hierarchical mixture model:\n")    
        cat("\nMixture weights:\n")
        print(x$Pi.j, ...)
        cat("\nConditional class prior probabilities:\n")
        print(x$P.kj, ...)
        cat("\nMeans:\n")
        print(x$mu.kj, ...)
        if (x$method == "hm1") {
            cat("\nParameter estimates for the simple mixture model:\n")    
            cat("\nMeans:\n")
            print(x$mu.j, ...)
        }
        if (x$method == "hm2") {
            cat("\nParameter estimates for the common components model:\n")    
            cat("\nMeans:\n")
            print(x$mu.j, ...)
        }
    }
    if(x$method == "cc"){
        cat("\nParameter estimates for the common components model:\n")    
        cat("\nClass prior probabilities:\n")
        print(x$P.k, ...)
        cat("\nConditional mixture weights:\n")
        print(x$Pi.jk, ...)
        cat("\nMeans:\n")
        print(x$mu.j, ...)
    }
    invisible(x)
}



#' Classify multivariate observations in conjunction with the hierarchical mixture model or the common components model.
#'
#' This function is a method for the generic function \code{predict()} for class \code{"hmcc"}. 
#' It can be invoked by calling \code{predict(x)} for an object \code{x} of the appropriate class, or directly by calling 
#' \code{predict.hmcc(x)} regardless of the class of the object.
#' 
#' ?Missing values in \code{newdata} are handled by returning \code{NA} if the linear discriminants cannot be evaluated. 
#' ?If \code{newdata} is omitted and the \code{na.action} of the fit omitted cases, these will be omitted on the prediction. 
#'
#' @title Classify Multivariate Observations Based on the Hierarchical Mixture Model or the Common Components Model
#'
#' @param object An object of class \code{"hmcc"}.
#' @param newdata A \code{data.frame} of cases to be classified or, if \code{object} has a \code{formula}, a \code{data.frame} with columns 
#' of the same names as the variables used. A vector will be interpreted as a row vector. If \code{newdata} is missing, 
#' an attempt will be made to retrieve the data used to fit the \code{hmcc} object.
#' @param \dots Further arguments.
#'
#' @return A \code{list} with components:
#' \item{class}{The predicted class labels (a \code{factor}).}
#' \item{posterior}{Matrix of class posterior probabilities.}
#'
#' @references
#'  Titsias, M. K., Likas, A. C. (2001), Shared kernel models for class conditional density estimation.
#'    \emph{IEEE Transactions on Neural Networks}, \bold{12(5)}, 987--997.
#'  
#'  Titsias, M. K., Likas, A. C. (2002), Mixture of experts classification using a hierarchical mixture model.
#'    \emph{Neural Computation}, \bold{14}, 2221--2244.
#'
#' @seealso \code{\link{hmcc}}, \code{\link{hmccStart}}.
#'
#' @examples
#' library(mlbench)
#' data.train <- as.data.frame(mlbench.waveform(300))
#' data.test <- as.data.frame(mlbench.waveform(200))
#' 
#' ## "cc"
#' fit <- hmcc(classes ~ ., data = data.train, J = 5, method = "cc")
#' pred <- predict(fit, data.test)
#' mean(pred$class != data.test$classes)
#' 
#' ## "hm1"
#' fit <- hmcc(classes ~ ., data = data.train, J = 3, method = "hm1")
#' pred <- predict(fit, data.test)
#' mean(pred$class != data.test$classes)
#' 
#' ## "hm2"
#' fit <- hmcc(classes ~ ., data = data.train, J = 3, method = "hm2")
#' pred <- predict(fit, data.test)
#' mean(pred$class != data.test$classes)
#' 
#' @keywords classif cluster multivariate
#'
#' @method predict hmcc
#' @rdname predict.hmcc
#'
#' @S3method predict hmcc
#' @import mvtnorm

predict.hmcc <- function(object, newdata, ...) {
    if(!inherits(object, "hmcc")) {
        stop("object not of class \"hmcc\"")
    }
    if (!is.null(Terms <- object$terms)) {
        Terms <- delete.response(Terms)
        if (missing(newdata))
            newdata <- model.frame(object)
        else {
            newdata <- model.frame(Terms, newdata, na.action = na.pass,
                xlev = object$xlevels)
            if (!is.null(cl <- attr(Terms, "dataClasses")))
                .checkMFClasses(cl, newdata)
        }
        x <- model.matrix(Terms, newdata, contrasts = object$contrasts)
        xint <- match("(Intercept)", colnames(x), nomatch = 0)
        if (xint > 0)
            x <- x[, -xint, drop = FALSE]
    } else {
        if (missing(newdata)) {
            if (!is.null(sub <- object$call$subset))
                newdata <- eval.parent(parse(text = paste(deparse(object$call$x,
                    backtick = TRUE), "[", deparse(sub, backtick = TRUE),
                    ",]")))
            else newdata <- eval.parent(object$call$x)
            if (!is.null(nas <- object$call$na.action))
                newdata <- eval(call(nas, newdata))
        }
        if (is.null(dim(newdata)))
            dim(newdata) <- c(1, length(newdata))
        x <- as.matrix(newdata)
    }

    hm1.predict <- hm2.predict <- function (object, x, ...) {
        n <- nrow(x)
        with(object, {
        	lev1 <- colnames(P.kj)
    		if (ncol(x) != ncol(object$mu.kj))
        		stop("wrong number of variables")
    		if (length(colnames(x)) > 0L && any(colnames(x) != dimnames(object$mu.kj)[[2L]]))
        		warning("variable names in 'newdata' do not match those in 'object'")
            c.p <- matrix(0, n, K * J)
            classp.Z <- matrix(0, n, K)
            for (k in 1:K) {
                c.p[,(1:J) + (k-1)*J] <- sapply(1:J, function(y) if(is.na(P.Z[k,y])) c.p.x <- rep(0, n) else c.p.x <- P.Z[k,y]/n * dmvnorm(x, mean = mu.kj[k,,y], sigma = Sigma.kj[,,y + J*(k-1)]))
                classp.Z[,k] <- rowSums(c.p[,(1:J) + J*(k-1), drop = FALSE])
            }
            classp.N <- rowSums(classp.Z)
            classp <- classp.Z/classp.N
            dimnames(classp) <- list(rownames(x), lev1)
            posterior <- matrix(0, n, length(lev))
            dimnames(posterior) <- list(rownames(x), lev)
            posterior[,lev1] <- classp
            cl <- factor(lev1[max.col(classp.Z)], levels = lev)
            names(cl) <- rownames(x)
            list(class = cl, posterior = posterior)
        })
    }

    cc.predict <- function (object, x, ...) {
        n <- nrow(x)
        with(object, {
        	lev1 <- names(P.k)
    		if (ncol(x) != ncol(object$mu.j))
        		stop("wrong number of variables")
    		if (length(colnames(x)) > 0L && any(colnames(x) != dimnames(object$mu.j)[[2L]]))
        		warning("variable names in 'newdata' do not match those in 'object'")
            c.p <- matrix(0, n, K * J)
            classp.Z <- matrix(0, n, K)
            for (k in 1:K) {
                c.p[,(1:J) + (k-1)*J] <- sapply(1:J, function(y) P.Z[k,y] * dmvnorm(x, mean = mu.j[,,y], sigma = Sigma.j[,,y])) 
                classp.Z[,k] <- rowSums(c.p[,(1:J) + J*(k-1), drop = FALSE])
            }
            classp.N <- rowSums(classp.Z)
            classp <- classp.Z/classp.N
            dimnames(classp) <- list(rownames(x), lev1)
            posterior <- matrix(0, n, length(lev))
            dimnames(posterior) <- list(rownames(x), lev)
            posterior[,lev1] <- classp
            cl <- factor(lev1[max.col(classp.Z)], levels = lev)
            names(cl) <- rownames(x)
            return(list(class = cl, posterior = posterior))
        })
    }
    
    hmcc.predict <- get(paste(object$method, "predict", sep = "."), mode = "function")
    return(hmcc.predict(object, x))
}
