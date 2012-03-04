#### flexmix
### FLXdist
## FLXM (component specific models)
## FLXcomponent (functions to determine component specific log-liklihoods and predictions, component-specific parameters)
## FLXP concomitant variable model
### FLXcontrol





#' @rdname FLXMCL
#' @aliases FLXMCL-class
#' 
#' @import flexmix
#' @export
#'
#' @title Local Mixtures of Classifiers

setClass("FLXMCL", contains = "FLXM")



# Fitted
#
# @param object An object of class "flexmix" ...
# @param drop ...
# @param local.aggregate Logical. ...
# @param \dots ...
#'
#' @import flexmix
#' @export
#'
#' @rdname FLXMCL
#' @aliases FLXMCL fitted,flexmix-method
#'
#' @docType methods

setMethod("fitted", signature(object = "flexmix"), function (object, drop = TRUE, local.aggregate = FALSE, ...){
        x <- list()
        for (m in seq_along(object@model)) {
            comp <- lapply(object@components, "[[", m)
            x[[m]] <- fitted(object@model[[m]], comp, ...)
        }
        #print(x)
        if (local.aggregate) {
        	post <- fitted(object@concomitant, ...)
            z <- lapply(x, function(z) matrix(rowSums(matrix(sapply(seq_len(object@k), 
                function(K) z[[K]] * post[,K]), ncol = object@k)), 
                nrow = nrow(z[[1]])))
            print(str(z))
         	for (m in seq_along(object@model))
         		colnames(z[[m]]) <- colnames(x[[m]][[1]])
            # z <- lapply(x, function(z) matrix(rowSums(matrix(sapply(seq_len(object@k), 
                # function(K) z[[K]] * object@prior[K]), ncol = object@k)), 
                # nrow = nrow(z[[1]])))
            if (drop && all(lapply(z, ncol) == 1)) {
                z <- sapply(z, unlist)
            }
        }
        else {
            z <- list()
            for (k in seq_len(object@k)) {
                z[[k]] <- do.call("cbind", lapply(x, "[[", k))
            }
            names(z) <- paste("Comp", seq_len(object@k), sep = ".")
            if (drop && all(lapply(z, ncol) == 1)) {
                z <- sapply(z, unlist)
            }
        }
        z
})



# Predict
# 
#' @param object An object of class "flexmix" ...
#' @param newdata Dataframe containing new data
#' @param local.aggregate Logical. 
#' @param \dots ...
#'
#' @import flexmix
#' @export
#'
#' @rdname FLXMCL
#' @aliases FLXMCL predict,flexmix-method
#'
#' @docType methods

setMethod("predict", signature(object = "flexmix"), function (object, newdata = list(), local.aggregate = FALSE, ...){
	    if (missing(newdata)) 
            return(fitted(object, local.aggregate = local.aggregate, drop = FALSE))
        x = list()
        for (m in seq_along(object@model)) {
            comp <- lapply(object@components, "[[", m)
            x[[m]] <- predict(object@model[[m]], newdata, comp, ##slot model not found
                ...)
        }
        if (local.aggregate) {
        	post <- predict(object@concomitant, newdata = newdata, ...)
            z <- lapply(x, function(z) matrix(rowSums(matrix(sapply(seq_len(object@k), 
                function(K) {print(head(z[[K]])); z[[K]] * post[,K]}), ncol = object@k)), 
                nrow = nrow(z[[1]])))
         	for (m in seq_along(object@model))
         		colnames(z[[m]]) <- colnames(x[[m]][[1]])
        }
        else {
            z <- list()
            for (k in seq_len(object@k)) {
                z[[k]] <- do.call("cbind", lapply(x, "[[", k))
            }
            names(z) <- paste("Comp", seq_len(object@k), sep = ".")
        }
        z
})
