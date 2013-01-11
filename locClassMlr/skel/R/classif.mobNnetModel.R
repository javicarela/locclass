#' @rdname makeRLearner
#' @method makeRLearner classif.mobNnetModel
#' @S3method makeRLearner classif.mobNnetModel
makeRLearner.classif.mobNnetModel = function() {
	makeRLearnerClassif(
		cl = "classif.mobNnetModel",
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
			## nnet parameters
			makeIntegerLearnerParam(id = "reps", default = 1, lower = 1),
   		    makeIntegerLearnerParam(id = "size", default = 3L, lower=0L),
			# contrasts?
       	 	makeNumericVectorLearnerParam(id = "Wts"),
       		# makeLogicalVectorLearnerParam(id = "mask"),#???
        	# linout hard coded
        	# entropy hard coded
        	# softmax hard coded
        	# censored hard coded       
        	makeLogicalLearnerParam(id = "skip", default = FALSE),
        	makeNumericLearnerParam(id = "rang", default = 0.7),
        	makeNumericLearnerParam(id = "decay", default = 0),
        	makeIntegerLearnerParam(id = "maxit", default = 100L, lower = 1L),
        	makeLogicalLearnerParam(id = "Hess", default = FALSE),
        	makeLogicalLearnerParam(id = "trace", default = TRUE),
        	makeIntegerLearnerParam(id = "MaxNWts", default = 1000L),
        	makeNumericLearnerParam(id = "abstol", default = 1.0e-4),
        	makeNumericLearnerParam(id = "reltol", default = 1.0e-8)
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
#' @method trainLearner classif.mobNnetModel
#' @S3method trainLearner classif.mobNnetModel
trainLearner.classif.mobNnetModel = function(.learner, .task, .subset,  ...) {
	f = as.formula(paste(.task$task.desc$target, "~", paste(getTaskFeatureNames(.task), collapse = "+"), "|", paste(getTaskFeatureNames(.task), collapse = "+")))
	mf = match.call()
    m = match(c("alpha", "bonferroni", "minsplit", "trim", "objfun", "breakties", "parm", "verbose"), names(mf), 0)
	mf = mf[c(1, m)]
    mf[[1]] <- as.name("mob_control")
	if (.task$task.desc$has.weights)
		mob(f, data = getTaskData(.task, .subset), weights = .task$weights[.subset], model = nnetModel, control = eval(mf), ...)
	else
		mob(f, data = getTaskData(.task, .subset), model = nnetModel, control = eval(mf), ...)
}



#' @rdname predictLearner
#' @method predictLearner classif.mobNnetModel
#' @S3method predictLearner classif.mobNnetModel
predictLearner.classif.mobNnetModel = function(.learner, .model, .newdata, ...) {
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
