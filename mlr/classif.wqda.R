setClass(
		"classif.wqda",
		contains = c("rlearner.classif")
)



setMethod(
		f = "initialize",
		signature = signature("classif.wqda"),
		def = function(.Object) {
			par.set = makeParamSet(
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
					weights = TRUE
			)
		}
)



setMethod(
		f = "trainLearner",
		signature = signature(
				.learner = "classif.wqda",
				.task = "ClassifTask",
				.subset = "integer"
		),
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
			if (.task@desc@has.weights)
				wqda(f, data = getData(.task, .subset), weights = .task@weights[.subset], ...)
      		else  
        		wqda(f, data = getData(.task, .subset), ...)
		}
)



setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.wqda",
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
