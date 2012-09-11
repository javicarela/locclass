#' @rdname makeRLearner
#' @method makeRLearner classif.osqda
#' @S3method makeRLearner classif.osqda
makeRLearner.classif.osqda = function() {
	makeRLearnerClassif(
		cl = "classif.osqda",
		package = "locClass",
		par.set = makeParamSet(
			makeDiscreteLearnerParam(id = "wf", default = "biweight", values = c("biweight", "cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine", "rectangular", "triangular")),
			makeNumericLearnerParam(id = "bw", lower = 0),
			makeIntegerLearnerParam(id = "k", lower = 1),
          	makeLogicalLearnerParam(id = "nn.only", requires = expression(!missing(k))),
			makeDiscreteLearnerParam(id = "method", default = "unbiased", values = c("unbiased", "ML"))
     	),
		oneclass = FALSE,
		twoclass = TRUE,
		multiclass = TRUE,
		missings = FALSE,
		numerics = TRUE,
		factors = TRUE,
		prob = TRUE,
		weights = FALSE
	)
}



#' @rdname trainLearner
#' @method trainLearner classif.osqda
#' @S3method trainLearner classif.osqda
trainLearner.classif.osqda = function(.learner, .task, .subset,  ...) {
	f = getTaskFormula(.task)
	osqda(f, data = getTaskData(.task, .subset), ...)
}



#' @rdname predictLearner
#' @method predictLearner classif.osqda
#' @S3method predictLearner classif.osqda
predictLearner.classif.osqda = function(.learner, .model, .newdata, ...) {
	p = predict(.model$learner.model, newdata = .newdata, ...)
	if (.learner$predict.type == "response")
		return(p$class)
	else
		return(p$posterior)
}
