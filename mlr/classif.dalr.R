setClass(
		"classif.dalr",
		contains = c("rlearner.classif")
)



setMethod(
		f = "initialize",
		signature = signature("classif.dalr"),
		def = function(.Object) {
			par.set = makeParamSet(
					makeNumericLearnerParam(id = "thr", lower = 0, upper = 1, default = 0.5),					
					makeDiscreteLearnerParam(id = "wf", default = "biweight", values = c("biweight", 
						"cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine", 
						"rectangular", "triangular")),
					makeNumericLearnerParam(id = "bw", lower = 0),
					makeIntegerLearnerParam(id = "k", lower = 1L),
          			makeLogicalLearnerParam(id = "nn.only", requires = expression(!missing(k))),
					makeIntegerLearnerParam(id = "itr", default = 3L, lower = 1L),
          			makeLogicalLearnerParam(id = "intercept", default = TRUE)				
     				# add arguments to glm
     				# offset
     				# control
     				# model
     				# x
     				# y
     				# contrasts
     				# start
     				# etastart
     				# mustart
     		)
			.Object = callNextMethod(.Object, pack  = "locClass", par.set = par.set)
			setProperties(.Object,
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
)



setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.dalr",
				.task="ClassifTask",
				.subset="integer"
		),
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
      		if (.task@desc@has.weights)
        		dalr(f, data = getData(.task, .subset), weights = .task@weights[.subset], ...)
      		else  
				dalr(f, data = getData(.task, .subset), ...)
		}
)



setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.dalr",
				.model = "WrappedModel",
				.newdata = "data.frame"
		),
		def = function(.learner, .model, .newdata, ...) {
			p = predict(.model@learner.model, newdata = .newdata, ...)
			if(.learner@predict.type == "response")
				return(p$class)
			else
				return(p$posterior)
		}
)
