makeRLearner.classif.oslda = function() {
	makeRLearnerClassif(
		cl = "classif.oslda",
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



trainLearner.classif.oslda = function(.learner, .task, .subset,  ...) {
	f = getTaskFormula(.task)
	oslda(f, data = getTaskData(.task, .subset), ...)
}



predictLearner.classif.oslda = function(.learner, .model, .newdata, ...) {
	p = predict(.model$learner.model, newdata = .newdata, ...)
	if (.learner$predict.type == "response")
		return(p$class)
	else
		return(p$posterior)
}
