#' @rdname makeRLearner
#' @method makeRLearner classif.mobMultinomModel
#' @S3method makeRLearner classif.mobMultinomModel
makeRLearner.classif.mobMultinomModel = function() {
	makeRLearnerClassif(
		cl = "classif.mobMultinomModel",
		package = c("locClass", "party"),
		par.set = makeParamSet(
			## mob parameters
			makeNumericLearnerParam(id = "alpha", lower = 0, upper = 1, default = 0.05),
			makeLogicalLearnerParam(id = "bonferroni", default = TRUE),
			makeIntegerLearnerParam(id = "minsplit", lower = 1, default = 20),
			makeNumericLearnerParam(id = "trim", lower = 0, default = 0.1),
			makeFunctionLearnerParam(id = "objfun", default = deviance),
			makeLogicalLearnerParam(id = "breakties", default = FALSE),
			# parm?
			# verbose?
			## multinom parameters
			makeLogicalLearnerParam(id = "Hess", default = FALSE),
			makeDiscreteLearnerParam(id = "summ", default = 0L, values = 0:3),
			makeLogicalLearnerParam(id = "model", default = FALSE),
			## nnet parameters
			# size is hard coded
			# contrasts ?
			# Wts is hard coded
			# mask is hard coded
			# linout hard coded
			# entropy hard coded
			# softmax hard coded
			makeLogicalLearnerParam(id = "censored", default = FALSE),
			# skip is hard coded
			# rang is hard coded
			makeNumericLearnerParam(id = "decay", default = 0),
			makeIntegerLearnerParam(id = "maxit", default = 100L, lower = 1L),
			makeLogicalLearnerParam(id = "trace", default = TRUE),
			makeIntegerLearnerParam(id = "MaxNWts", default = 1000L),
			makeNumericLearnerParam(id = "abstol", default = 1.0e-4),
			makeNumericLearnerParam(id = "reltol", default = 1.0e-8)
      	),
		twoclass = TRUE,
		multiclass = TRUE,
		numerics = TRUE,
		factors = TRUE,
		prob = TRUE,
		weights = TRUE
	)
}



#' @rdname trainLearner
#' @method trainLearner classif.mobMultinomModel
#' @S3method trainLearner classif.mobMultinomModel
trainLearner.classif.mobMultinomModel = function(.learner, .task, .subset,  ...) {
	f = as.formula(paste(.task$task.desc$target, "~", paste(getTaskFeatureNames(.task), collapse = "+"), "|", paste(getTaskFeatureNames(.task), collapse = "+")))
	mf = match.call()
	m = match(c("alpha", "bonferroni", "minsplit", "trim", "objfun", "breakties", "parm", "verbose"), names(mf), 0)
	mf = mf[c(1, m)]
	mf[[1]] = as.name("mob_control")
	if (.task$task.desc$has.weights)
		mob(f, data = getTaskData(.task, .subset), weights = .task$weights[.subset], model = multinomModel, control = eval(mf), ...)
	else  
		mob(f, data = getTaskData(.task, .subset), model = multinomModel, control = eval(mf), ...)			
}



#' @rdname predictLearner
#' @method predictLearner classif.mobMultinomModel
#' @S3method predictLearner classif.mobMultinomModel
predictLearner.classif.mobMultinomModel = function(.learner, .model, .newdata, ...) {
	lev = levels(.model$learner.model@responses@variables[[1]])
	if (.learner$predict.type == "response") {
		p = predict(.model$learner.model, newdata = .newdata, out = "class", ...)
		p = factor(p, labels = lev, levels = seq_along(lev))
	} else {
		pred = predict(.model$learner.model, newdata = .newdata, out = "posterior", ...)
		p = matrix(0, length(pred), length(lev))
		colnames(p) = lev
		rownames(p) = rownames(.newdata)
		for (i in seq_along(pred)) {
			p[i, colnames(pred[[i]])] = pred[[i]]
		}
	}
	return(p)
}
