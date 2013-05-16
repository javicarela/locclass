#' Methods for creating \pkg{mlr} \code{\link[mlr]{Learner}} objects.
#' The generic function is already documented in \code{\link[mlr]{makeRLearner}}.
#'
#' @title Methods for Creating \pkg{mlr} \code{\link[mlr]{Learner}} Objects
#'
#' @aliases makeRLearner makeRLearner.classif.constant
#'
#' @name makeRLearner
#'
#' @rdname makeRLearner
#' @method makeRLearner classif.constant
#' @S3method makeRLearner classif.constant
#' @importFrom mlr makeRLearner
#'
#' @examples
#' lrn1 <- makeLearner("classif.wlda", predict.type = "prob") # linear discr. analysis
#' lrn2 <- makeLearner("classif.wsvm")                        # support vector machine
#'
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



#' Methods for training a \pkg{mlr} \code{\link[mlr]{Learner}} object.
#' The generic function is already documented in \code{\link[mlr]{trainLearner}}.
#' For convenience part of the documentation is adopted here.
#'
#' @title Methods for Training a \pkg{mlr} \code{\link[mlr]{Learner}} Object
#'
#' @param .learner [\code{\link[mlr]{RLearner}}]\cr  
#'   Wrapped learner. 
#' @param .task [\code{\link[mlr]{SupervisedTask}}]\cr
#'   Task to train learner on.
#' @param .subset [\code{integer}]\cr
#'   An index vector specifying the training cases to be used for fitting the model.
#' @param .weights [\code{numeric}]\cr
#'   Optional, non-negative case weight vector to be used during fitting. If given, must be of same length as \code{.subset} and in corresponding order. 
#'   By default missing which means that no weights are used.
#' @param \dots [any]\cr
#'   Additional (hyper)parameters to be passed to the underlying train function.
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
#'
#' @examples
#' task <- makeClassifTask(data = iris, target = "Species")  # create classification task
#' lrn <- makeLearner("classif.wlda", predict.type = "prob") # linear discr. analysis
#' inds <- sample(nrow(iris), 100)                   # indices of training observations
#' mod <- train(lrn, task, subset = inds)            # train the learner on training data
#'
trainLearner.classif.constant <- function(.learner, .task, .subset, .weights, ...) {
	f = as.formula(getTaskFormulaAsString(.task))
	# if (.task$task.desc$has.weights)
    	# constant(f, data = getTaskData(.task, .subset), weights = .task$weights[.subset], ...)
	# else  
		constant(f, data = getTaskData(.task, .subset), ...)
}



#' Methods for predicting a \pkg{mlr} \code{\link[mlr]{Learner}} object.
#' The generic function is already documented in \code{\link[mlr]{predictLearner}}.
#' For convenience part of the documentation is adopted here.
#'
#' @title Methods for Predicting a \pkg{mlr} \code{\link[mlr]{Learner}} Object
#'
#' @param .learner [\code{\link[mlr]{RLearner}}]\cr  
#'   Wrapped learner. 
#' @param .model [\code{\link[mlr]{WrappedModel}}]\cr
#'   Model produced by training. 
#' @param .newdata [\code{data.frame}]\cr
#'   New data to predict. Does not include target column.
#' @param \dots [any]\cr
#'   Additional parameters to be passed to the underlying \code{predict} function.
#' @return For classification: \cr
#'   Either a factor if \code{predict.type="response"} or a matrix for
#'   if \code{predict.type="prob"} was chosen when calling \code{\link[mlr]{makeLearner}}. 
#'   In the latter case the columns must be named with the class labels. \cr
#'   For regression: \cr
#'   Either a numeric for type \code{predict.type="response"} or a matrix with two columns for \cr \code{predict.type="se"}.
#' 	 In the latter case the first column is the estimated response (mean value) and the second column the estimated standard errors.
#'
#' @aliases predictLearner predictLearner.classif.constant
#'
#' @name predictLearner
#'
#' @rdname predictLearner
#' @method predictLearner classif.constant
#' @S3method predictLearner classif.constant
#' @importFrom mlr predictLearner
#'
#' @examples
#' task <- makeClassifTask(data = iris, target = "Species")  # create classification task
#' lrn <- makeLearner("classif.wlda", predict.type = "prob") # linear discr. analysis
#' inds <- sample(nrow(iris), 100)                   # indices of training observations
#' mod <- train(lrn, task, subset = inds)            # train the learner on training data
#' pred <- predict(mod, newdata = iris[-inds,])      # predict on test data
#' perf <- performance(pred, measure = mmce)         # calculate misclassification rate
#'
predictLearner.classif.constant <- function(.learner, .model, .newdata, ...) {
	p = predict(.model$learner.model, newdata = .newdata, ...)
	if(.learner$predict.type == "response")
		return(p$class)
	else
		return(p$posterior)
}
