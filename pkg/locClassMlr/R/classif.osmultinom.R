#' @rdname makeRLearner
#' @method makeRLearner classif.osmultinom
#' @S3method makeRLearner classif.osmultinom
makeRLearner.classif.osmultinom = function() {
	makeRLearnerClassif(
		cl = "classif.osmultinom",
		package = "locClass",
		par.set = makeParamSet(
			## osmultinom
			# contrasts
			# censored
			makeLogicalLearnerParam(id = "model", default = FALSE),
			## osnnet
			# size is hard coded
			# Wts is hard coded
			# mask is hard coded
			# linout 
			# entropy is hard coded
			# softmax is hard coded
			# skip is hard coded
			# rang is hard coded
			makeNumericLearnerParam(id = "decay", default = 0),
			makeIntegerLearnerParam(id = "maxit", default = 100L, lower = 1L),
			makeLogicalLearnerParam(id = "trace", default = TRUE),
			makeIntegerLearnerParam(id = "MaxNWts", default = 1000L),
			makeNumericLearnerParam(id = "abstol", default=1.0e-4),
			makeNumericLearnerParam(id = "reltol", default=1.0e-8),
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
#' @method trainLearner classif.osmultinom
#' @S3method trainLearner classif.osmultinom
trainLearner.classif.osmultinom = function(.learner, .task, .subset,  ...) {
	f = getTaskFormula(.task)
	osmultinom(f, data = getTaskData(.task, .subset), ...)
}



#' @rdname predictLearner
#' @method predictLearner classif.osmultinom
#' @S3method predictLearner classif.osmultinom
predictLearner.classif.osmultinom = function(.learner, .model, .newdata, ...) {
	if (.learner$predict.type == "response")
		p = predict(.model$learner.model, newdata = .newdata, type = "class", ...)
	else {
		p = predict(.model$learner.model, newdata = .newdata, type = "probs", ...)
		if (length(.model$task.desc$class.levels) == 2) {
			p <- cbind(1-p, p) 
			colnames(p) = .model$task.desc$class.levels
		}
	}
	return(p)
}
