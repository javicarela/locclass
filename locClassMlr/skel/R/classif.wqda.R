#' @rdname makeRLearner
#' @method makeRLearner classif.wqda
#' @S3method makeRLearner classif.wqda
makeRLearner.classif.wqda = function() {
	makeRLearnerClassif(
		cl = "classif.wqda",
		package = "locClass",
		par.set = makeParamSet(
			makeDiscreteLearnerParam(id = "method", default = "unbiased", values = c("unbiased", "ML"))
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



#' @rdname trainLearner
#' @method trainLearner classif.wqda
#' @S3method trainLearner classif.wqda
trainLearner.classif.wqda = function(.learner, .task, .subset,  ...) {
	f = getTaskFormula(.task)
	if (.task$task.desc$has.weights)
		wqda(f, data = getTaskData(.task, .subset), weights = .task$weights[.subset], ...)
	else  
		wqda(f, data = getTaskData(.task, .subset), ...)
}



#' @rdname predictLearner
#' @method predictLearner classif.wqda
#' @S3method predictLearner classif.wqda
predictLearner.classif.wqda = function(.learner, .model, .newdata, ...) {
	p = predict(.model$learner.model, newdata = .newdata, ...)
	if (.learner$predict.type == "response")
		return(p$class)
	else
		return(p$posterior)
}
