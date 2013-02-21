#' @rdname makeRLearner
#' @method makeRLearner classif.daqda
#' @S3method makeRLearner classif.daqda
makeRLearner.classif.daqda = function() {
	makeRLearnerClassif(
		cl = "classif.daqda",
		package = "locClass",
		par.set = makeParamSet(
			makeDiscreteLearnerParam(id = "wf", default = "biweight", values = c("biweight", "cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine", "rectangular", "triangular")),
			makeNumericLearnerParam(id = "bw", lower = 0),
			makeIntegerLearnerParam(id = "k", lower = 1),
          	makeLogicalLearnerParam(id = "nn.only", requires = expression(!missing(k))),
			makeIntegerLearnerParam(id = "itr", default = 3, lower = 1),
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
#' @method trainLearner classif.daqda
#' @S3method trainLearner classif.daqda
trainLearner.classif.daqda = function(.learner, .task, .subset, .weights, ...) {
	f = as.formula(getTaskFormulaAsString(.task))
    # if (.task$task.desc$has.weights)
		# daqda(f, data = getTaskData(.task, .subset), weights = .task$weights[.subset], ...)
	# else  
		daqda(f, data = getTaskData(.task, .subset), ...)
}



#' @rdname predictLearner
#' @method predictLearner classif.daqda
#' @S3method predictLearner classif.daqda
predictLearner.classif.daqda = function(.learner, .model, .newdata, ...) {
	p = predict(.model$learner.model, newdata = .newdata, ...)
	if (.learner$predict.type == "response")
		return(p$class)
	else
		return(p$posterior)
}
