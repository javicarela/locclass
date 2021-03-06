makeRLearner.classif.FLXMCLmultinom = function() {
	makeRLearnerClassif(
		cl = "classif.FLXMCLmultinom",
		package = "locClass",
		par.set = makeParamSet(
			## kmeans parameters
			makeIntegerLearnerParam(id = "centers", lower = 1),
			## flexmix parameters
			# makeIntegerLearnerParam(id = "k", lower = 1, requires = expression(missing(cluster))),
			# makeIntegerVectorLearnerParam(id = "cluster", requires = expression(missing(k))),
			## control
			makeIntegerLearnerParam(id = "iter.max", lower = 1L, default = 200L),						
			makeNumericLearnerParam(id = "minprior", lower = 0, upper = 1, default = 0.05),
			makeNumericLearnerParam(id = "tolerance", lower = 0, default = 1e-06),	
			makeIntegerLearnerParam(id = "verbose", lower = 0L, default = 0L),						
			makeDiscreteLearnerParam(id = "classify", values = c("auto", "weighted", "hard", "CEM", "random", "SEM"), default = "auto"),
			makeIntegerLearnerParam(id = "nrep", lower = 1L, default = 1L),
			## multinom parameters
			## todo: some may not be supported
			# contrasts ?
    	    makeLogicalLearnerParam(id = "Hess", default = FALSE),
			makeLogicalLearnerParam(id = "censored", default = FALSE),
			makeLogicalLearnerParam(id = "model", default = FALSE),
			## nnet
			# size is hard coded
			# Wts is hard coded
			# mask is hard coded
			# linout hard coded
			# entropy hard coded
			# softmax hard coded
			# skip is hard coded
        	# rang is hard coded
        	makeNumericLearnerParam(id = "decay", default = 0),
        	makeIntegerLearnerParam(id = "maxit", default = 100L, lower = 1L),
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



trainLearner.classif.FLXMCLmultinom = function(.learner, .task, .subset,  ...) {
	f1 = getTaskFormula(.task)
	f2 = as.formula(paste("~ ", paste(getTaskFeatureNames(.task), collapse = "+")))
	model = FLXMCLmultinom(...)
	mf = match.call()
	mcontrol = match(c("iter.max", "minprior", "tolerance", "verbose", "classify", "nrep"), names(mf), 0)
	mfcontrol = mf[c(1, mcontrol)]
	mfcontrol[[1]] = as.name("list")
	control = eval(mfcontrol)
	mkmeans = match("centers", names(mf), 0)
	mfkmeans = mf[c(1, mkmeans)]
	mfkmeans$x = getTaskData(.task, .subset, target.extra = TRUE)$data
	mfkmeans[[1]] = as.name("kmeans")
	cluster = eval(mfkmeans)$cluster
	if (.task$task.desc$has.weights)
		flexmix(f1, data = getTaskData(.task, .subset), weights = .task$weights[.subset], concomitant = FLXPwlda(f2), model = model, control = control, cluster = cluster)
		# k = eval(mf$k), cluster = eval(mf$cluster))
	else
		flexmix(f1, data = getTaskData(.task, .subset), concomitant = FLXPwlda(f2), model = model, control = control, cluster = cluster)
		# k = eval(mf$k), cluster = eval(mf$cluster))
}



predictLearner.classif.FLXMCLmultinom = function(.learner, .model, .newdata, ...) {
	lev = attr(.model$learner.model$model[[1]]@y, "lev")
	p = mypredict(.model$learner.model, newdata = .newdata, aggregate = TRUE, ...)[[1]]
	if (.learner$predict.type == "response") {
		p = factor(colnames(p)[max.col(p)], levels = lev) ## does this always work?
	}
	return(p)			
}
