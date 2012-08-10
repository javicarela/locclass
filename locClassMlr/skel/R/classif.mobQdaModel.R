#' @rdname makeRLearner
#' @method makeRLearner classif.mobQdaModel
#' @S3method makeRLearner classif.mobQdaModel
makeRLearner.classif.mobQdaModel = function() {
	makeRLearnerClassif(
		cl = "classif.mobQdaModel",
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
			## wqda parameters
			makeDiscreteLearnerParam(id = "method", default = "unbiased", values = c("unbiased", "ML"))
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
#' @method trainLearner classif.mobQdaModel
#' @S3method trainLearner classif.mobQdaModel
trainLearner.classif.mobQdaModel = function(.learner, .task, .subset,  ...) {
	f = as.formula(paste(.task$task.desc$target, "~", paste(getTaskFeatureNames(.task), collapse = "+"), "|", paste(getTaskFeatureNames(.task), collapse = "+")))
	mf = match.call()
    m = match(c("alpha", "bonferroni", "minsplit", "trim", "objfun", "breakties", "parm", "verbose"), names(mf), 0)
	mf = mf[c(1, m)]
    mf[[1]] = as.name("mob_control")
	if (.task$task.desc$has.weights)
		mob(f, data = getTaskData(.task, .subset), weights=.task$weights[.subset], model = qdaModel, control = eval(mf), ...)
	else  
		mob(f, data = getTaskData(.task, .subset), model = qdaModel, control = eval(mf), ...)
}



#' @rdname predictLearner
#' @method predictLearner classif.mobQdaModel
#' @S3method predictLearner classif.mobQdaModel
predictLearner.classif.mobQdaModel = function(.learner, .model, .newdata, ...) {
	lev = levels(.model$learner.model@responses@variables[[1]])
	if (.learner$predict.type == "response") {
		p = predict(.model$learner.model, newdata=.newdata, out = "class", ...)
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
