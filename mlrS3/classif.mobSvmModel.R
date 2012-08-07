makeRLearner.classif.mobSvmModel = function() {
	makeRLearnerClassif(
		cl = "classif.mobSvmModel",
		package = "locClass",
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
			## wsvm parameters
			# makeLogicalLearnerParam(id = "scale"),
			makeDiscreteLearnerParam(id = "type", default = "C-classification", values = c("C-classification", "nu-classification")),
			makeDiscreteLearnerParam(id = "kernel", default = "radial", values = c("linear", "polynomial", "radial", "sigmoid")),
			makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L, requires = expression(kernel=="polynomial")),
			makeNumericLearnerParam(id = "gamma", lower = 0, requires = expression(kernel != "linear")),
			makeNumericLearnerParam(id = "coef0", default = 0, requires = expression(kernel == "polynomial" || kernel == "sigmoid")),
			makeNumericLearnerParam(id = "cost",  default = 1, lower = 0, requires = expression(type == "C-classification")),
			makeNumericLearnerParam(id = "nu", default = 0.5, requires = expression(type == "nu-classification")),
			# class.weights ?
			makeNumericLearnerParam(id = "cachesize", default = 40L),
			makeNumericLearnerParam(id = "tolerance", default = 0.001, lower = 0),
			# epsilon not needed for classification
          	makeLogicalLearnerParam(id = "shrinking", default = TRUE),
			# cross ?
			makeLogicalLearnerParam(id = "fitted", default = TRUE),
			# probability specified through learner
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



trainLearner.classif.mobSvmModel = function(.learner, .task, .subset,  ...) {
	f = as.formula(paste(.tasktask.desc$target, "~", paste(getTaskFeatureNames(.task), collapse = "+"), "|", paste(getTaskFeatureNames(.task), collapse = "+")))
	mf = match.call()
    m = match(c("alpha", "bonferroni", "minsplit", "trim", "objfun", "breakties", "parm", "verbose"), names(mf), 0)
	mf = mf[c(1, m)]
    mf[[1]] = as.name("mob_control")
	if (.tasktask.desc$has.weights)
		mob(f, data = getTaskData(.task, .subset), weights = .task$weights[.subset], probability = .learner$predict.type == "prob", model = svmModel, control = eval(mf), ...)
	else  
		mob(f, data = getTaskData(.task, .subset), probability = .learner$predict.type == "prob", model = svmModel, control = eval(mf), ...)
}



predictLearner.classif.mobSvmModel = function(.learner, .model, .newdata, ...) {
	lev = levels(.model$learner.model$responses$variables[[1]])
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
