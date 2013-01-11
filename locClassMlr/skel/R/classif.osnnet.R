#' @rdname makeRLearner
#' @method makeRLearner classif.osnnet
#' @S3method makeRLearner classif.osnnet
makeRLearner.classif.osnnet = function() {
	makeRLearnerClassif(
		cl = "classif.osnnet",
		package = "locClass",
		par.set = makeParamSet(
			makeIntegerLearnerParam(id = "reps", default = 1, lower = 1),
			makeIntegerLearnerParam(id = "size", default = 3L, lower = 0L, pass.default = TRUE),
			# contrasts?
			makeNumericVectorLearnerParam(id = "Wts"),
			# makeLogicalVectorLearnerParam(id = "mask"),
			# linout
			# entropy
			# softmax
			# censored
			makeLogicalLearnerParam(id = "skip", default = FALSE),
			makeNumericLearnerParam(id = "rang", default = 0.7),
			makeNumericLearnerParam(id = "decay", default = 0),
			makeIntegerLearnerParam(id = "maxit", default = 100L, lower = 1L),
			makeLogicalLearnerParam(id = "trace", default = TRUE),
			makeIntegerLearnerParam(id = "MaxNWts", default = 1000L),
			makeNumericLearnerParam(id = "abstol", default = 1.0e-4),
			makeNumericLearnerParam(id = "reltol", default = 1.0e-8),
			makeDiscreteLearnerParam(id = "wf", default = "biweight", values = c("biweight", "cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine", "rectangular", "triangular")),
			makeNumericLearnerParam(id = "bw", lower = 0),
			makeIntegerLearnerParam(id = "k", lower = 1),
			makeLogicalLearnerParam(id = "nn.only", requires = expression(!missing(k)))
			# Hess currently not supported
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
#' @method trainLearner classif.osnnet
#' @S3method trainLearner classif.osnnet
trainLearner.classif.osnnet = function(.learner, .task, .subset,  ...) {
	f = getTaskFormula(.task)
	osnnet(f, data = getTaskData(.task, .subset), ...)
}



#' @rdname predictLearner
#' @method predictLearner classif.osnnet
#' @S3method predictLearner classif.osnnet
predictLearner.classif.osnnet = function(.learner, .model, .newdata, ...) {
	if (.learner$predict.type == "response")
		p = predict(.model$learner.model, newdata = .newdata, type = "class", ...)
	else {
		p = predict(.model$learner.model, newdata = .newdata, type = "raw", ...)
		if (length(.model$task.desc$class.levels) == 2) {
          	p <- cbind(1-p, p) 
			colnames(p) = .model$task.desc$class.levels
		}
	}
	return(p)
}
