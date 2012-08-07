makeRLearner.classif.constant = function() {
	makeRLearnerClassif(
		cl = "classif.constant",
		package = "locClass",
		par.set = makeParamSet(
#		makeDiscreteLearnerParam(id = "method", default = "unbiased", values = c("unbiased", "ML"))
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



trainLearner.classif.constant <- function(.learner, .task, .subset,  ...) {
	f = getTaskFormula(.task)
	if (.task$task.desc$has.weights)
    	constant(f, data = getTaskData(.task, .subset), weights = .task$weights[.subset], ...)
	else  
		constant(f, data = getTaskData(.task, .subset), ...)
}



predictLearner.classif.constant <- function(.learner, .model, .newdata, ...) {
	p = predict(.model$learner.model, newdata = .newdata, ...)
	if(.learner$predict.type == "response")
		return(p$class)
	else
		return(p$posterior)
}
