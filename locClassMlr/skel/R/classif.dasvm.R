#' @rdname makeRLearner
#' @method makeRLearner classif.dasvm
#' @S3method makeRLearner classif.dasvm
makeRLearner.classif.dasvm = function() {
	makeRLearnerClassif(
		cl = "classif.dasvm",
		package = "locClass",
		par.set = makeParamSet(
			## dasvm
			makeDiscreteLearnerParam(id = "wf", default = "biweight", values = c("biweight", "cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine", "rectangular", "triangular")),
			makeNumericLearnerParam(id = "bw", lower = 0),
			makeIntegerLearnerParam(id = "k", lower = 1),
          	makeLogicalLearnerParam(id = "nn.only", requires = expression(!missing(k))),
			makeIntegerLearnerParam(id = "itr", default = 3, lower = 1),
			makeDiscreteLearnerParam(id = "method", default = "prob", values = c("prob", "decision")),
          	# makeLogicalLearenParam(id = "scale"),
         	makeDiscreteLearnerParam(id = "type", default = "C-classification", values = c("C-classification", "nu-classification")),
			## wsvm
         	makeDiscreteLearnerParam(id = "kernel", default = "radial", values = c("linear", "polynomial", "radial", "sigmoid")),
         	makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L, requires = expression(kernel == "polynomial")),
          	makeNumericLearnerParam(id = "gamma", lower = 0, requires = expression(kernel!="linear")),
          	makeNumericLearnerParam(id = "coef0", default = 0, requires = expression(kernel=="polynomial" || kernel=="sigmoid")),
          	makeNumericLearnerParam(id = "cost",  default = 1, lower = 0, requires = expression(type=="C-classification")),
          	makeNumericLearnerParam(id = "nu", default = 0.5, requires = expression(type=="nu-classification")),
          	# class.weights
          	makeNumericLearnerParam(id = "cachesize", default = 40L),
          	makeNumericLearnerParam(id = "tolerance", default = 0.001, lower = 0),
          	# epsilon not needed for classification
          	makeLogicalLearnerParam(id = "shrinking", default = TRUE),
          	# cross ?
          	makeLogicalLearnerParam(id = "fitted", default = TRUE),
          	# probability not needed since specified through learner
          	makeIntegerLearnerParam(id = "seed", default = 1L)
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
#' @method trainLearner classif.dasvm
#' @S3method trainLearner classif.dasvm
trainLearner.classif.dasvm = function(.learner, .task, .subset, .weights, ...) {
	f = as.formula(getTaskFormulaAsString(.task))
	# if (.task$task.desc$has.weights)
		# dasvm(f, data = getTaskData(.task, .subset), probability = .learner$predict.type == "prob", case.weights = .task$weights[.subset], ...)
	# else  
		dasvm(f, data = getTaskData(.task, .subset), probability = .learner$predict.type == "prob", ...)
}



#' @rdname predictLearner
#' @method predictLearner classif.dasvm
#' @S3method predictLearner classif.dasvm
predictLearner.classif.dasvm = function(.learner, .model, .newdata, ...) {
	if (.learner$predict.type == "response") {
		p = predict(.model$learner.model, newdata = .newdata, ...)
	} else {
		p = predict(.model$learner.model, newdata = .newdata, probability = TRUE, ...)
		p = attr(p, "probabilities")
	}
}
