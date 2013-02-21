#' @rdname makeRLearner
#' @method makeRLearner classif.kmc
#' @S3method makeRLearner classif.kmc
makeRLearner.classif.kmc = function() {
	makeRLearnerClassif(
		cl = "classif.kmc",
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
#' @method trainLearner classif.kmc
#' @S3method trainLearner classif.kmc
trainLearner.classif.kmc = function(.learner, .task, .subset, .weights, ...) {
	f = as.formula(getTaskFormulaAsString(.task))
	kmc(f, data = getTaskData(.task, .subset), ...)
}



#' @rdname predictLearner
#' @method predictLearner classif.kmc
#' @S3method predictLearner classif.kmc
predictLearner.classif.kmc = function(.learner, .model, .newdata, ...) {
	p = predict(.model$learner.model, newdata = .newdata, ...)
	if (.learner$predict.type == "response")
		return(p$class)
	else
		return(p$posterior)
}
