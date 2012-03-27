setClass(
		"classif.osnnet",
		contains = c("rlearner.classif")
)



setMethod(
		f = "initialize",
		signature = signature("classif.osnnet"),
		def = function(.Object) {
			par.set = makeParamSet(
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
					makeDiscreteLearnerParam(id = "wf", default = "biweight", values = c("biweight",
						"cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine",
						"rectangular", "triangular")),
					makeNumericLearnerParam(id = "bw", lower = 0),
					makeIntegerLearnerParam(id = "k", lower = 1),
          			makeLogicalLearnerParam(id = "nn.only", requires = expression(!missing(k)))
          			# Hess currently not supported
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
				.learner = "classif.osnnet",
				.task = "ClassifTask",
				.subset = "integer"
		),
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
			osnnet(f, data = getData(.task, .subset), ...)
		}
)



setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.osnnet",
				.model = "WrappedModel",
				.newdata = "data.frame"
		),
		def = function(.learner, .model, .newdata, ...) {
			if(.learner@predict.type == "response")
				p = predict(.model@learner.model, newdata = .newdata, type = "class", ...)
			else {
				p = predict(.model@learner.model, newdata = .newdata, type = "raw", ...)
				if (length(.model@task.desc@class.levels) == 2) {
          			p <- cbind(1-p, p) 
					colnames(p) = .model@task.desc@class.levels
				}
			}
			return(p)
		}
)
