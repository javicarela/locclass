setClass(
		"classif.ossvm",
		contains = c("rlearner.classif")
)



setMethod(
		f = "initialize",
		signature = signature("classif.ossvm"),
		def = function(.Object) {
			par.set = makeParamSet(
					# scale
					makeDiscreteLearnerParam(id = "type", default = "C-classification", values = c("C-classification", "nu-classification")),
					makeDiscreteLearnerParam(id = "kernel", default = "radial", values = c("linear", "polynomial", "radial", "sigmoid")),
					makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L, requires = expression(kernel == "polynomial")),
					makeNumericLearnerParam(id = "gamma", lower = 0, requires = expression(kernel != "linear")),
					makeNumericLearnerParam(id = "coef0", default = 0, requires = expression(kernel == "polynomial" || kernel == "sigmoid")),
					makeNumericLearnerParam(id = "cost",  default = 1, lower = 0, requires = expression(type == "C-classification")),
					makeNumericLearnerParam(id = "nu", default = 0.5, requires = expression(type == "nu-classification")),
					# class.weights?
					makeNumericLearnerParam(id = "cachesize", default = 40L),
					makeNumericLearnerParam(id = "tolerance", default = 0.001, lower = 0),
					# epsilon not needed for classification			
					makeLogicalLearnerParam(id = "shrinking", default = TRUE),
					makeIntegerLearnerParam(id = "seed", default = 1L),
					makeDiscreteLearnerParam(id = "wf", default = "biweight", values = c("biweight",
						"cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine",
						"rectangular", "triangular")),
					makeNumericLearnerParam(id = "bw", lower = 0),
					makeIntegerLearnerParam(id = "k", lower = 1),
          			makeLogicalLearnerParam(id = "nn.only", requires = expression(!missing(k)))
     		 )
			.Object = callNextMethod(.Object, pack = "locClass", par.set = par.set)
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
				.learner = "classif.ossvm",
				.task = "ClassifTask",
				.subset = "integer"
		),
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
			ossvm(f, data = getData(.task, .subset), ...)
		}
)



setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.ossvm",
				.model = "WrappedModel",
				.newdata = "data.frame"
		),
		def = function(.learner, .model, .newdata, ...) {
			p = predict(.model@learner.model, newdata = .newdata, ...)
			if(.learner@predict.type == "response") {
				p = predict(.model@learner.model, newdata = .newdata, ...)
			} else {
				p = predict(.model@learner.model, newdata = .newdata, probability = TRUE, ...)
				p = attr(p, "probabilities")
			}
		}
)
