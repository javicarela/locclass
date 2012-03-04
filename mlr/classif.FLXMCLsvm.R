setClass(
		"classif.FLXMCLsvm",
		contains = c("rlearner.classif")
)



setMethod(
		f = "initialize",
		signature = signature("classif.FLXMCLsvm"),
		def = function(.Object) {
				par.set = makeParamSet(
						## flexmix parameters
						makeIntegerLearnerParam(id = "k", lower = 1, requires = expression(missing(cluster))),
						makeIntegerVectorLearnerParam(id = "cluster", requires = expression(missing(k))),
						## control
						makeIntegerLearnerParam(id = "iter.max", lower = 1L, default = 200L),						
						makeNumericLearnerParam(id = "minprior", lower = 0, upper = 1, default = 0.05),
						makeNumericLearnerParam(id = "tolerance", lower = 0, default = 1e-06),	
						makeIntegerLearnerParam(id = "verbose", lower = 0L, default = 0L),						
						makeDiscreteLearnerParam(id = "classifiy", values = c("auto", "weighted", "hard", "CEM", "random", "SEM"), default = "auto"),						
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
          				makeNumericLearnerParam(id = "tolerance", default = 0.001, lower = 0),
						# epsilon not needed for classification
          				makeLogicalLearnerParam(id = "shrinking", default = TRUE),
						# cross ?
						makeLogicalLearnerParam(id = "fitted", default = TRUE),
						# probability specified through learner
						makeIntegerLearnerParam(id = "seed", default = 1L) 
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
				.learner = "classif.FLXMCLsvm",
				.task = "ClassifTask",
				.subset = "integer"
		),
		def = function(.learner, .task, .subset,  ...) {
			f1 = getFormula(.task)
			f2 = as.formula(paste("~ ", paste(getFeatureNames(.task), collapse = "+")))
			model = FLXMCLsvm(...)
			mf = match.call()
    		mcontrol = match(c("iter.max", "minprior", "tolerance", "verbose", "classify", "nrep"), names(mf), 0)
		    mfcontrol = mf[c(1, mcontrol)]
    		mfcontrol[[1]] = as.name("list")
			control = eval(mfcontrol)
      		if (.task@desc@has.weights)
        		flexmix(f1, data = getData(.task, .subset), weights = .task@weights[.subset], 
        			concomitant = FLXPwlda(f2), model = model, control = control, 
        			k = eval(mf$k), cluster = eval(mf$cluster))
      		else
        		flexmix(f1, data = getData(.task, .subset), 
        			concomitant = FLXPwlda(f2), model = model, control = control,
        			k = eval(mf$k), cluster = eval(mf$cluster))
		}
)   



setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.FLXMCLsvm",
				.model = "WrappedModel",
				.newdata = "data.frame"
		),
		def = function(.learner, .model, .newdata, ...) {
			lev = attr(.model@learner.model@model[[1]]@y, "lev")
			p = predict(.model@learner.model, newdata = .newdata, local.aggregate = TRUE, ...)[[1]]
			if (.learner@predict.type == "response") {
				p = factor(colnames(p)[max.col(p)], levels = lev) ## does this always work?
			}
			return(p)			
		}
)
