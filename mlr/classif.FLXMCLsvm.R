setClass(
		"classif.FLXMCLsvm",
		contains = c("rlearner.classif")
)



setMethod(
		f = "initialize",
		signature = signature("classif.FLXMCLsvm"),
		def = function(.Object) {
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
          				###makeNumericLearnerParam(id = "tolerance", default = 0.001, lower = 0), ## same names
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
				.learner = "classif.FLXMCLsvm",
				.model = "WrappedModel",
				.newdata = "data.frame"
		),
		def = function(.learner, .model, .newdata, ...) {
			lev = attr(.model@learner.model@model[[1]]@y, "lev")
			pred = mypredict(.model@learner.model, newdata = .newdata, aggregate = TRUE, ...)[[1]]
# print(str(pred))
# print(str(pred$decision))
			if (.learner@predict.type == "prob") {
				p = pred$posterior
# print(head(p))
				attr(p, "decision") <- pred$decision
			} else {
				## are columns of decision == zero? NAs
				## does voting work?
				ng <- length(lev)
				problems <- rbind(rep(lev, ng:1-1), unlist(sapply(2:ng, function(x) lev[x:ng])))
# print(problems)
				votes <- (pred$decision < 0) + 1
				candidates <- sapply(1:length(lev), function(z) return(problems[votes[,z],z]))				
# print(head(pred$decision,20))
# print(head(votes,20))
# print(head(candidates,20))
				if (ng == 2) {
					p = factor(candidates, levels = lev)
				} else {
					p = sapply(lev, function(x) rowSums(candidates == x))
					p = factor(lev[max.col(p)], levels = lev)
				}
			}
			return(p)			
		}
)
