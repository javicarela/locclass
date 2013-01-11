#' Methods for creating \code{\link[mlr]{Learner}} objects.
#'
#' @title Methods for Creating \code{\link[mlr]{Learner}} Objects.
#'
#' @aliases makeRLearner makeRLearner.classif.constant
#'
#' @name makeRLearner
#'
#' @rdname makeRLearner
#' @method makeRLearner classif.constant
#' @S3method makeRLearner classif.constant
#' @importFrom mlr makeRLearner
makeRLearner.classif.constant = function() {
	makeRLearnerClassif(
		cl = "classif.constant",
		package = "locClass",
		par.set = makeParamSet(
#		makeDiscreteLearnerParam(id = "method", default = "unbiased", values = c("unbiased", "ML"))
		), 
		oneclass = FALSE,
		twoclass = TRUE,
		multiclass = TRUE,
		missings = FALSE,
		numerics = TRUE,
		factors = TRUE,
		prob = TRUE,
		weights = TRUE
	)
}



#' Methods for training a \code{\link[mlr]{Learner}} object.
#'
#' @title Methods for Training a \code{\link[mlr]{Learner}} Object
#'
#' @param .learner [\code{\link[mlr]{RLearner}}]\cr  
#'   Wrapped learner. 
#' @param .task [\code{\link[mlr]{SupervisedTask}}]\cr
#'   Task to train learner on.
#' @param .subset [\code{integer}]\cr
#'   Subset of cases for training set, index the task with this.
#'   You probably want to use \code{\link[mlr]{getTaskData}} for this purpose. 
#' @param \dots [any]\cr
#'   Additional (hyper)parameters, which need to be passed to the underlying train function.
#' @return [any] Model of the underlying learner.
#'
#'
#' @aliases trainLearner trainLearner.classif.constant
#'
#' @name trainLearner
#'
#' @rdname trainLearner
#' @method trainLearner classif.constant
#' @S3method trainLearner classif.constant
#' @importFrom mlr trainLearner
trainLearner.classif.constant <- function(.learner, .task, .subset,  ...) {
	f = getTaskFormula(.task)
	if (.task$task.desc$has.weights)
    	constant(f, data = getTaskData(.task, .subset), weights = .task$weights[.subset], ...)
	else  
		constant(f, data = getTaskData(.task, .subset), ...)
}



#' Methods for predicting a \code{\link[mlr]{Learner}} object.
#'
#' @title Methods for Predicting a \code{\link[mlr]{Learner}} Object
#'
#' @param .learner [\code{\link[mlr]{RLearner}}]\cr  
#'   Wrapped learner. 
#' @param .model [\code{\link[mlr]{WrappedModel}}]\cr
#'   Model produced by training. 
#' @param .newdata [\code{data.frame}]\cr
#'   New data to predict. Does not include target column.
#' @param \dots [any]\cr
#'   Additional parameters, which need to be passed to the underlying \code{predict} function.
#' @return Either a factor for type \code{"response"} or a matrix for
#'   type \code{"prob"}. In the latter case the columns must be named with the class labels.
#'
#' @aliases predictLearner predictLearner.classif.constant
#'
#' @name predictLearner
#'
#' @rdname predictLearner
#' @method predictLearner classif.constant
#' @S3method predictLearner classif.constant
#' @importFrom mlr predictLearner
predictLearner.classif.constant <- function(.learner, .model, .newdata, ...) {
	p = predict(.model$learner.model, newdata = .newdata, ...)
	if(.learner$predict.type == "response")
		return(p$class)
	else
		return(p$posterior)
}
