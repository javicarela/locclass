setClass(
		"classif.oslda",
		contains = c("rlearner.classif")
)



setMethod(
		f = "initialize",
		signature = signature("classif.oslda"),
		def = function(.Object) {
		  	par.set = makeParamSet(
					makeDiscreteLearnerParam(id = "wf", default = "biweight", values = c("biweight", 
						"cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine", 
						"rectangular", "triangular")),
					makeNumericLearnerParam(id = "bw", lower = 0),
					makeIntegerLearnerParam(id = "k", lower = 1),
          			makeLogicalLearnerParam(id = "nn.only", requires = expression(!missing(k))),
					makeDiscreteLearnerParam(id = "method", default = "unbiased", values = c("unbiased", "ML"))
     		 )
      		.Object = callNextMethod(.Object, pack="locClass", par.set = par.set)
			setProperties(.Object,
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
)



setMethod(
		f = "trainLearner",
		signature = signature(
				.learner = "classif.oslda",
				.task = "ClassifTask", 
				.subset = "integer"
		),
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
			oslda(f, data = getData(.task, .subset), ...)
		}
)



setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.oslda",
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
