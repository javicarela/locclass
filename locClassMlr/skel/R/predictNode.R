#' Predict the ID of the associated terminal node for \code{\link[party]{mob}} trees.
#'
#' @title Predict the ID of the Associated Terminal Node for \code{\link[party]{mob}} Trees
#'
#' @param object [\code{\link[mlr]{WrappedModel}}] \cr
#'   Object of class \code{\link[mlr]{WrappedModel}} where \code{object$learner.model} is of class \code{\link[party]{mob}}.
#' @param newdata [\code{data.frame}]\cr
#'   New data to predict. Defaults to \code{NULL}.
#' @param \dots Further arguments to \code{\link[party]{predict.mob}}.
#'
#' @return A vector of IDs of terminal nodes.
#' @export
#'
#' @examples
#' task <- makeClassifTask(data = iris, target = "Species") # create classification task
#' lrn <- makeLearner("classif.mobConstantModel")           # create learner
#' mod <- train(lrn, task)                                  # train learner
#' predictNode(mod)                                         # terminal nodes
#'
predictNode <- function(object, newdata = NULL, ...) {
	if (!inherits(object$learner.model, "mob"))
		stop("Learner model is not of class 'mob'")
	predict(object$learner.model, newdata = newdata, type = "node", ...)
}
