#' @rdname makeRLearner
#' @method makeRLearner classif.dannet
#' @S3method makeRLearner classif.dannet
makeRLearner.classif.dannet = function() {
	makeRLearnerClassif(
		cl = "classif.dannet",
		package = "locClass",
		par.set = makeParamSet(
			## dannet
			makeDiscreteLearnerParam(id = "wf", default = "biweight", values = c("biweight", "cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine", "rectangular", "triangular")),
			makeNumericLearnerParam(id = "bw", lower = 0),
			makeIntegerLearnerParam(id = "k", lower = 1),
          	makeLogicalLearnerParam(id = "nn.only", requires = expression(!missing(k))),
			makeIntegerLearnerParam(id = "itr", default = 3, lower = 1),
			# contrasts?
			## nnet
        	makeIntegerLearnerParam(id = "size", default = 3L, lower=0L, pass.default=TRUE),
        	makeNumericVectorLearnerParam(id = "Wts"),
        	#makeLogicalVectorLearnerParam(id = "mask"),#???
        	# linout hard coded
        	# entropy hard coded
        	# softmax hard coded
        	# censored hard coded       
        	makeLogicalLearnerParam(id="skip", default=FALSE),
        	makeNumericLearnerParam(id="rang", default=0.7),
        	makeNumericLearnerParam(id="decay", default=0),
        	makeIntegerLearnerParam(id="maxit", default=100L, lower=1L),
        	makeLogicalLearnerParam(id="Hess", default=FALSE),
       		makeLogicalLearnerParam(id="trace", default=TRUE),
			makeIntegerLearnerParam(id="MaxNWts", default=1000L),
			makeNumericLearnerParam(id="abstol", default=1.0e-4),
			makeNumericLearnerParam(id="reltol", default=1.0e-8)
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



#' @rdname trainLearner
#' @method trainLearner classif.dannet
#' @S3method trainLearner classif.dannet
trainLearner.classif.dannet = function(.learner, .task, .subset,  ...) {
	f = getTaskFormula(.task)
	if (.task$task.desc$has.weights)
		dannet(f, data = getTaskData(.task, .subset), weights = .task$weights[.subset], ...)
	else
    	dannet(f, data = getTaskData(.task, .subset), ...)
}



#' @rdname predictLearner
#' @method predictLearner classif.dannet
#' @S3method predictLearner classif.dannet
predictLearner.classif.dannet = function(.learner, .model, .newdata, ...) {
	p = predict(.model$learner.model, newdata = .newdata, ...)
	if (.learner$predict.type == "response")
		return(p$class)
	else
		return(p$posterior)
}
