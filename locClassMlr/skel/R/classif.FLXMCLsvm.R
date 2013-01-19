#' @rdname makeRLearner
#' @method makeRLearner classif.FLXMCLsvm
#' @S3method makeRLearner classif.FLXMCLsvm
makeRLearner.classif.FLXMCLsvm = function() {
	makeRLearnerClassif(
		cl = "classif.FLXMCLsvm",
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
			makeNumericLearnerParam(id = "toleranceMix", lower = 0, default = 1e-06),	
			makeIntegerLearnerParam(id = "verbose", lower = 0L, default = 0L),						
			makeDiscreteLearnerParam(id = "classify", values = c("auto", "weighted", "hard", "CEM", "random", "SEM"), default = "auto"),
			makeIntegerLearnerParam(id = "nrep", lower = 1L, default = 1L),
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
			makeNumericLearnerParam(id = "tolerance", default = 0.001, lower = 0), ## same name as flexmix control parameter
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



#' @rdname trainLearner
#' @method trainLearner classif.FLXMCLsvm
#' @S3method trainLearner classif.FLXMCLsvm
trainLearner.classif.FLXMCLsvm = function(.learner, .task, .subset,  ...) {
	f1 = getTaskFormula(.task)
	f2 = as.formula(paste("~ ", paste(getTaskFeatureNames(.task), collapse = "+")))
	model = FLXMCLsvm(...)
	mf = match.call()
	mcontrol = match(c("iter.max", "minprior", "toleranceMix", "verbose", "classify", "nrep"), names(mf), 0)
	mfcontrol = mf[c(1, mcontrol)]
	ind <- which(names(mfcontrol) == "toleranceMix")
	names(mfcontrol)[ind] <- "tolerance"
	mfcontrol[[1]] = as.name("list")
	control = eval(mfcontrol)
	mfcontrol$tolerance = 10^(-2)	# FIXME: should not be hard-coded
	controlInit = eval(mfcontrol)
	mkmeans = match("centers", names(mf), 0)
	mfkmeans = mf[c(1, mkmeans)]
	mfkmeans$x = getTaskData(.task, .subset, target.extra = TRUE)$data
	mfkmeans[[1]] = as.name("kmeans")
	cluster = replicate(5, eval(mfkmeans)$cluster)	# FIXME: 5 should not be hard-coded
	if (.task$task.desc$has.weights) {
		fit <- myStepFlexmix(f1, data = getTaskData(.task, .subset), weights = .task$weights[.subset], concomitant = FLXPmultinom(f2), model = model, control = controlInit, cluster = cluster)
		flexmix(f1, data = getTaskData(.task, .subset), weights = .task$weights[.subset], concomitant = FLXPmultinom(f2), model = model, control = control, cluster = posterior(fit))
		# flexmix(f1, data = getTaskData(.task, .subset), weights = .task$weights[.subset], concomitant = FLXPwlda(f2), model = model, control = control, cluster = cluster)
		# k = eval(mf$k), cluster = eval(mf$cluster))
	} else {
		fit <- myStepFlexmix(f1, data = getTaskData(.task, .subset), concomitant = FLXPmultinom(f2), model = model, control = controlInit, cluster = cluster)
		flexmix(f1, data = getTaskData(.task, .subset), concomitant = FLXPmultinom(f2), model = model, control = control, cluster = posterior(fit))
		# flexmix(f1, data = getTaskData(.task, .subset), concomitant = FLXPwlda(f2), model = model, control = control, cluster = cluster)
		# k = eval(mf$k), cluster = eval(mf$cluster))
	}
}



#' @rdname predictLearner
#' @method predictLearner classif.FLXMCLsvm
#' @S3method predictLearner classif.FLXMCLsvm
predictLearner.classif.FLXMCLsvm = function(.learner, .model, .newdata, ...) {
	lev = attr(.model$learner.model@model[[1]]@y, "lev")
	p = mypredict(.model$learner.model, newdata = .newdata, aggregate = TRUE, ...)[[1]]
	if (.learner$predict.type == "response") {
		p = factor(colnames(p)[max.col(p)], levels = lev)
	} else {
		p = p/rowSums(p)
	}	
	return(p)
}

## old version
# predictLearner.classif.FLXMCLsvm = function(.learner, .model, .newdata, ...) {
	# lev = attr(.model$learner.model@model[[1]]@y, "lev")
	# pred = mypredict(.model$learner.model, newdata = .newdata, aggregate = TRUE, ...)[[1]]
 # print(str(pred))
# # print(str(pred$decision))
	# if (.learner$predict.type == "prob") {
		# p = pred$posterior
# # print(head(p))
		# attr(p, "decision") <- pred$decision
	# } else {
		# ## are columns of decision == zero? NAs
		# ## does voting work?
		# ng <- length(lev)
		# problems <- rbind(rep(lev, ng:1-1), unlist(sapply(2:ng, function(x) lev[x:ng])))
# # print(problems)
		# votes <- (pred$decision < 0) + 1
		# candidates <- sapply(1:length(lev), function(z) return(problems[votes[,z],z]))				
# # print(head(pred$decision,20))
# # print(head(votes,20))
# # print(head(candidates,20))
		# if (ng == 2) {
			# p = factor(candidates, levels = lev)
		# } else {
			# p = sapply(lev, function(x) rowSums(candidates == x))
			# p = factor(lev[max.col(p)], levels = lev)
		# }
	# }
	# return(p)			
# }
