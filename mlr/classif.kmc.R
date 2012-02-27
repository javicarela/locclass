setClass(
		"classif.kmc",
		contains = c("rlearner.classif")
)

setMethod(
		f = "initialize",
		signature = signature("classif.kmc"),
		def = function(.Object) {
		  	par.set = makeParamSet(
					makeDiscreteLearnerParam(id = "wf", default = "biweight", values = c("biweight", 
						"cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine", 
						"rectangular", "triangular")),
					makeNumericLearnerParam(id = "bw", lower = 0),
					makeIntegerLearnerParam(id = "k", lower = 1),
          			makeLogicalLearnerParam(id = "nn.only", requires = expression(!missing(k)))
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
				.learner = "classif.kmc",
				.task = "ClassifTask", 
				.subset = "integer"
		),

		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
			kmc(f, data=getData(.task, .subset), ...)
		}
)



setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.kmc",
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
