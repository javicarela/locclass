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



trainLearner.classif.kmc = function(.learner, .task, .subset,  ...) {
	f = getTaskFormula(.task)
	kmc(f, data = getTaskData(.task, .subset), ...)
}



predictLearner.classif.kmc = function(.learner, .model, .newdata, ...) {
	p = predict(.model$learner.model, newdata = .newdata, ...)
	if (.learner$predict.type == "response")
		return(p$class)
	else
		return(p$posterior)
}
