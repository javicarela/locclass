#' @rdname makeRLearner
#' @method makeRLearner classif.wlda
#' @S3method makeRLearner classif.wlda
makeRLearner.classif.wlda = function() {
	makeRLearnerClassif(
		cl = "classif.wlda",
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
#' @method trainLearner classif.wlda
#' @S3method trainLearner classif.wlda
trainLearner.classif.wlda = function(.learner, .task, .subset, .weights, ...) {
	f = as.formula(getTaskFormulaAsString(.task))
	# if (.task$task.desc$has.weights)
		# wlda(f, data = getTaskData(.task, .subset), weights = .task$weights[.subset], ...)
	# else  
		wlda(f, data = getTaskData(.task, .subset), ...)
}



#' @rdname predictLearner
#' @method predictLearner classif.wlda
#' @S3method predictLearner classif.wlda
predictLearner.classif.wlda = function(.learner, .model, .newdata, ...) {
	p = predict(.model$learner.model, newdata = .newdata, ...)
	if (.learner$predict.type == "response")
		return(p$class)
	else
		return(p$posterior)
}
