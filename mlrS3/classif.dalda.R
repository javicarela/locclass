makeRLearner.classif.dalda = function() {
	makeRLearnerClassif(
		cl = "classif.dalda",
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



trainLearner.classif.dalda <- function(.learner, .task, .subset,  ...) {
	f = getTaskFormula(.task)
    if (.task$task.desc$has.weights)
        dalda(f, data = getTaskData(.task, .subset), weights = .task$weights[.subset], ...)
	else  
		dalda(f, data = getTaskData(.task, .subset), ...)
}



predictLearner.classif.dalda <- function(.learner, .model, .newdata, ...) {
	p = predict(.model$learner.model, newdata = .newdata, ...)
	if (.learner$predict.type == "response")
		return(p$class)
	else
		return(p$posterior)
}
