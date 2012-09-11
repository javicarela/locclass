#' @rdname makeRLearner
#' @method makeRLearner classif.kda
#' @S3method makeRLearner classif.kda
makeRLearner.classif.kda = function() {
	makeRLearnerClassif(
		cl = "classif.kda",
		package = "locClass",
		par.set = makeParamSet(
			makeDiscreteLearnerParam(id = "wf", default = "biweight", values = c("biweight", "cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine", "rectangular", "triangular")),
			makeNumericLearnerParam(id = "bw", lower = 0),
			makeIntegerLearnerParam(id = "k", lower = 1),
          	makeLogicalLearnerParam(id = "nn.only", requires = expression(!missing(k)))
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
#' @method trainLearner classif.kda
#' @S3method trainLearner classif.kda
trainLearner.classif.kda = function(.learner, .task, .subset,  ...) {
	f = getTaskFormula(.task)
	kda(f, data = getTaskData(.task, .subset), ...)
}



#' @rdname predictLearner
#' @method predictLearner classif.kda
#' @S3method predictLearner classif.kda
predictLearner.classif.kda = function(.learner, .model, .newdata, ...) {
	p = predict(.model$learner.model, newdata = .newdata, ...)
	if (.learner$predict.type == "response")
		return(p$class)
	else
		return(p$posterior)
}
