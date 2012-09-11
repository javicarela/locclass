#' @rdname makeRLearner
#' @method makeRLearner classif.wsvm
#' @S3method makeRLearner classif.wsvm
makeRLearner.classif.wsvm = function() {
	makeRLearnerClassif(
		cl = "classif.wsvm",
		package = "locClass",
		par.set = makeParamSet(
			# makeLogicalLearnerParam(id = "scale"),
			makeDiscreteLearnerParam(id = "type", default = "C-classification", values = c("C-classification", "nu-classification")),
			makeDiscreteLearnerParam(id = "kernel", default = "radial", values = c("linear", "polynomial", "radial", "sigmoid")),
			makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L, requires = expression(kernel=="polynomial")),
			makeNumericLearnerParam(id = "gamma", lower = 0, requires = expression(kernel != "linear")),
			makeNumericLearnerParam(id = "coef0", default = 0, requires = expression(kernel == "polynomial" || kernel == "sigmoid")),
			makeNumericLearnerParam(id = "cost",  default = 1, lower = 0, requires = expression(type == "C-classification")),
			makeNumericLearnerParam(id = "nu", default = 0.5, requires = expression(type == "nu-classification")),
			# class.weights ?
			makeNumericLearnerParam(id = "cachesize", default = 40L),
			makeNumericLearnerParam(id = "tolerance", default = 0.001, lower = 0),
			# epsilon not needed for classification
			makeLogicalLearnerParam(id = "shrinking", default = TRUE),
			# cross ?
			makeLogicalLearnerParam(id = "fitted", default = TRUE),
			# probability specified through learner
			makeIntegerLearnerParam(id = "seed", default = 1L) 
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
#' @method trainLearner classif.wsvm
#' @S3method trainLearner classif.wsvm
trainLearner.classif.wsvm = function(.learner, .task, .subset,  ...) {
	f = getTaskFormula(.task)
	if (.task$task.desc$has.weights)
		wsvm(f, data = getTaskData(.task, .subset), case.weights = .task$weights[.subset], probability = .learner$predict.type == "prob", ...)
	else  
		wsvm(f, data = getTaskData(.task, .subset), probability = .learner$predict.type == "prob", ...)
}



#' @rdname predictLearner
#' @method predictLearner classif.wsvm
#' @S3method predictLearner classif.wsvm
predictLearner.classif.wsvm = function(.learner, .model, .newdata, ...) {
	if (.learner$predict.type == "response") {
		p = predict(.model$learner.model, newdata = .newdata, ...)
	} else {
		p = predict(.model$learner.model, newdata = .newdata, probability = TRUE, ...)
		p = attr(p, "probabilities")
	}
	return(p)
}
