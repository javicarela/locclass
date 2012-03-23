setClass(
		"classif.FLXMCLconstant",
		contains = c("rlearner.classif")
)



setMethod(
		f = "initialize",
		signature = signature("classif.FLXMCLconstant"),
		def = function(.Object) {
				par.set = makeParamSet(
						## kmeans parameters
						makeIntegerLearnerParam(id = "centers", lower = 1),
						## flexmix parameters
						#makeIntegerLearnerParam(id = "k", lower = 1),
						#makeIntegerVectorLearnerParam(id = "cluster", requires = expression(missing(k))),
						## control
						makeIntegerLearnerParam(id = "iter.max", lower = 1L, default = 200L),						
						makeNumericLearnerParam(id = "minprior", lower = 0, upper = 1, default = 0.05),
						makeNumericLearnerParam(id = "tolerance", lower = 0, default = 1e-06),	
						makeIntegerLearnerParam(id = "verbose", lower = 0L, default = 0L),						
						makeDiscreteLearnerParam(id = "classify", values = c("auto", "weighted", "hard", "CEM", "random", "SEM"), default = "auto"),						
						makeIntegerLearnerParam(id = "nrep", lower = 1L, default = 1L)
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
					weights = TRUE
			)			
		}
)



setMethod(
		f = "trainLearner",
		signature = signature(
				.learner = "classif.FLXMCLconstant",
				.task = "ClassifTask",
				.subset = "integer"
		),
		def = function(.learner, .task, .subset,  ...) {
			f1 = getFormula(.task)
			f2 = as.formula(paste("~ ", paste(getFeatureNames(.task), collapse = "+")))
			model = FLXMCLconstant(...)
			mf = match.call()
    		mcontrol = match(c("iter.max", "minprior", "tolerance", "verbose", "classify", "nrep"), names(mf), 0)
		    mfcontrol = mf[c(1, mcontrol)]
    		mfcontrol[[1]] = as.name("list")
			control = eval(mfcontrol)
			mkmeans = match("centers", names(mf), 0)
		    mfkmeans = mf[c(1, mkmeans)]
		    mfkmeans$x = getData(.task, .subset, target.extra = TRUE)$data
    		mfkmeans[[1]] = as.name("kmeans")
			cluster = eval(mfkmeans)$cluster
      		if (.task@desc@has.weights)
        		flexmix(f1, data = getData(.task, .subset), weights = .task@weights[.subset], 
        			concomitant = FLXPwlda(f2), model = model, control = control, 
        			cluster = cluster)
        			# k = eval(mf$k), cluster = eval(mf$cluster))
      		else
        		flexmix(f1, data = getData(.task, .subset), 
        			concomitant = FLXPwlda(f2), model = model, control = control,
        			cluster = cluster)
        			# k = eval(mf$k), cluster = eval(mf$cluster))
		}
)   



setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.FLXMCLconstant",
				.model = "WrappedModel",
				.newdata = "data.frame"
		),
		def = function(.learner, .model, .newdata, ...) {
			lev = attr(.model@learner.model@model[[1]]@y, "lev")
			p = mypredict(.model@learner.model, newdata = .newdata, aggregate = TRUE, ...)[[1]]
			if (.learner@predict.type == "response") {
				p = factor(colnames(p)[max.col(p)], levels = lev) ## does this always work?
			}
			return(p)			
		}
)
