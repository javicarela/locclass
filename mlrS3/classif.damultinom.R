makeRLearner.classif.damultinom = function() {
	makeRLearnerClassif(
		cl = "classif.damultinom",
		package = "locClass",
		par.set = makeParamSet(
			## damultinom
			# contrasts ?
        	makeLogicalLearnerParam(id = "Hess", default = FALSE),
			makeLogicalLearnerParam(id = "censored", default = FALSE),
			makeLogicalLearnerParam(id = "model", default = FALSE),
			## dannet
			makeDiscreteLearnerParam(id = "wf", default = "biweight", values = c("biweight", "cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine", "rectangular", "triangular")),
			makeNumericLearnerParam(id = "bw", lower = 0),
			makeIntegerLearnerParam(id = "k", lower = 1),
			makeLogicalLearnerParam(id = "nn.only", requires = expression(!missing(k))),
			makeIntegerLearnerParam(id = "itr", default = 3, lower = 1),
			## nnet
			# size is hard coded
			# Wts is hard coded
			# mask is hard coded
			# linout hard coded
			# entropy hard coded
			# softmax hard coded
			# skip is hard coded
			# rang is hard coded
        	makeNumericLearnerParam(id = "decay", default = 0),
        	makeIntegerLearnerParam(id = "maxit", default = 100L, lower = 1L),
			makeLogicalLearnerParam(id = "trace", default = TRUE),
			makeIntegerLearnerParam(id = "MaxNWts", default = 1000L),
        	makeNumericLearnerParam(id = "abstol", default = 1.0e-4),
        	makeNumericLearnerParam(id = "reltol", default = 1.0e-8)
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



trainLearner.classif.damultinom = function(.learner, .task, .subset,  ...) {
	f = getTaskFormula(.task)
	if (.task$task.desc$has.weights)
		damultinom(f, data = getTaskData(.task, .subset), weights = .task$weights[.subset], ...)
	else
		damultinom(f, data = getTaskData(.task, .subset), ...)
}



predictLearner.classif.damultinom = function(.learner, .model, .newdata, ...) {
	p = predict(.model$learner.model, newdata=.newdata, ...)
	if (.learner$predict.type == "response")
		return(p$class)
	else
		return(p$posterior)
}
