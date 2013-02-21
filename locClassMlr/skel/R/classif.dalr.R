#' @rdname makeRLearner
#' @method makeRLearner classif.dalr
#' @S3method makeRLearner classif.dalr
makeRLearner.classif.dalr = function() {
	makeRLearnerClassif(
		cl = "classif.dalr",
		package = "locClass",
		par.set = makeParamSet(
			makeNumericLearnerParam(id = "thr", lower = 0, upper = 1, default = 0.5),					
			makeDiscreteLearnerParam(id = "wf", default = "biweight", values = c("biweight", "cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine", "rectangular", "triangular")),
			makeNumericLearnerParam(id = "bw", lower = 0),
			makeIntegerLearnerParam(id = "k", lower = 1L),
          	makeLogicalLearnerParam(id = "nn.only", requires = expression(!missing(k))),
			makeIntegerLearnerParam(id = "itr", default = 3L, lower = 1L),
          	makeLogicalLearnerParam(id = "intercept", default = TRUE)		     			# add arguments to glm
     		# offset
     		# control
     		# model
     		# x
     		# y
     		# contrasts
     		# start
     		# etastart
     		# mustart
     	),
		oneclass = FALSE,
		twoclass = TRUE,
		multiclass = FALSE,
		missings = FALSE,
		numerics = TRUE,
		factors = TRUE,
		prob = TRUE,
		weights = TRUE
	)
}



#' @rdname trainLearner
#' @method trainLearner classif.dalr
#' @S3method trainLearner classif.dalr
trainLearner.classif.dalr = function(.learner, .task, .subset, .weights, ...) {
	f = as.formula(getTaskFormulaAsString(.task))
	# if (.task$task.desc$has.weights)
		# dalr(f, data = getTaskData(.task, .subset), weights = .task$weights[.subset], ...)
     # else  
		dalr(f, data = getTaskData(.task, .subset), ...)
}



#' @rdname predictLearner
#' @method predictLearner classif.dalr
#' @S3method predictLearner classif.dalr
predictLearner.classif.dalr = function(.learner, .model, .newdata, ...) {
	p = predict(.model$learner.model, newdata = .newdata, ...)
	if (.learner$predict.type == "response")
		return(p$class)
	else
		return(p$posterior)
}
