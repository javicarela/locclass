#' Predict the ID of the associated terminal node.
#'
#' @title Predict the ID of the Associated Terminal Node
#'
#' @param .object Object of class \code{"mob"}.
#' @param .newdata New data to predict.
#' @param \dots Further arguments to \code{\link[party]{predict.mob}}.
#'
#' @return A vector of IDs of terminal nodes.
#' @export

predictNode <- function(.object, .newdata = NULL, ...) {
	if (!inherits(.object$learner.model, "mob"))
		stop("Learner model is not of class 'mob'")
	predict(.object$learner.model, newdata = .newdata, type = "node", ...)
}
