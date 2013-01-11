#' @rdname makeRLearner
#' @method makeRLearner classif.nnetRep
#' @S3method makeRLearner classif.nnetRep
makeRLearner.classif.nnetRep = function() {
	makeRLearnerClassif(
		cl = "classif.nnetRep",
		package = "locClass",
		par.set = makeParamSet(
			makeIntegerLearnerParam(id="reps", default=1L, lower=1L),		
			makeIntegerLearnerParam(id="size", default=3L, lower=0L, pass.default=TRUE),
			# contrasts?
			makeNumericVectorLearnerParam(id = "Wts"),
			# makeLogicalVectorLearnerParam(id = "mask"),
			# linout
			# entropy
			# softmax
			# censored
			makeLogicalLearnerParam(id="skip", default=FALSE),
			makeNumericLearnerParam(id="rang", default=0.7),
			makeNumericLearnerParam(id="decay", default=0),
			makeIntegerLearnerParam(id="maxit", default=100L, lower=1L),
			makeLogicalLearnerParam(id="trace", default=TRUE),
			makeIntegerLearnerParam(id="MaxNWts", default=1000L),
			makeNumericLearnerParam(id="abstol", default=1.0e-4),
			makeNumericLearnerParam(id="reltol", default=1.0e-8),      
			makeLogicalLearnerParam(id="Hess", default=FALSE)
		), 
		# par.vals = list(size=3L),
		twoclass = TRUE,
		multiclass = TRUE,
		numerics = TRUE,
		factors = TRUE,
		prob = TRUE,
		weights = TRUE
	)
}



#' @rdname trainLearner
#' @method trainLearner classif.nnetRep
#' @S3method trainLearner classif.nnetRep
trainLearner.classif.nnetRep = function(.learner, .task, .subset,  ...) {
	f = getTaskFormula(.task)
	if (.task$task.desc$has.weights)
		nnetRep(formula = f, data = getTaskData(.task, .subset), weights = .task$weights[.subset], ...)
	else  
		nnetRep(formula = f, data = getTaskData(.task, .subset), ...)      
}



#' @rdname predictLearner
#' @method predictLearner classif.nnetRep
#' @S3method predictLearner classif.nnetRep
predictLearner.classif.nnetRep = function(.learner, .model, .newdata, ...) {
	type = switch(.learner$predict.type, response="class", prob="raw")
	p = predict(.model$learner.model, newdata=.newdata, type=type, ...)
	if (type == "class")
		return(as.factor(p))
	else {
		if (length(.model$task.desc$class.levels) == 2) {
		y <- cbind(1-p, p) 
		colnames(y) = .model$task.desc$class.levels
		return(y)
	} else
		return(p) 
	}
}
