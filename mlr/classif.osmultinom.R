setClass(
		"classif.osmultinom",
		contains = c("rlearner.classif")
)



setMethod(
		f = "initialize",
		signature = signature("classif.osmultinom"),
		def = function(.Object) {
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
					makeDiscreteLearnerParam(id = "wf", default = "biweight", values = c("biweight",
						"cauchy", "cosine", "epanechnikov", "exponential", "gaussian", "optcosine",
						"rectangular", "triangular")),
					makeNumericLearnerParam(id = "bw", lower = 0),
					makeIntegerLearnerParam(id = "k", lower = 1),
          			makeLogicalLearnerParam(id = "nn.only", requires = expression(!missing(k)))
        			# Hess currently not supported
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
				.learner = "classif.osmultinom",
				.task = "ClassifTask",
				.subset = "integer"
		),
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
			osmultinom(f, data = getData(.task, .subset), ...)
		}
)



setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.osmultinom",
				.model = "WrappedModel",
				.newdata = "data.frame"
		),
		def = function(.learner, .model, .newdata, ...) {
			if (.learner@predict.type == "response")
				p = predict(.model@learner.model, newdata = .newdata, type = "class", ...)
			else
				p = predict(.model@learner.model, newdata = .newdata, type = "probs", ...)
			return(p)
		}
)
