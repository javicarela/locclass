setClass(
		"myclassif.nnet", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("myclassif.nnet"),
		def = function(.Object) {
			par.set = makeParamSet(
        makeIntegerLearnerParam(id="size", default=3L, lower=0L, pass.default=TRUE),
        makeIntegerLearnerParam(id="maxit", default=100L, lower=1L),
        # nnet seems to set these manually and hard for classification.....
#        makeLogicalLearnerParam(id="linout", default=FALSE, requires=expression(entropy==FALSE && softmax==FALSE && censored==FALSE)),
#        makeLogicalLearnerParam(id="entropy", default=FALSE, requires=expression(linout==FALSE && softmax==FALSE && censored==FALSE)),
#        makeLogicalLearnerParam(id="softmax", default=FALSE, requires=expression(entropy==FALSE && linout==FALSE && censored==FALSE)),
#        makeLogicalLearnerParam(id="censored", default=FALSE, requires=expression(linout==FALSE && softmax==FALSE && entropy==FALSE)),
        makeLogicalLearnerParam(id="skip", default=FALSE),
        makeNumericLearnerParam(id="rang", default=0.7),
        makeNumericLearnerParam(id="decay", default=0),
        makeLogicalLearnerParam(id="Hess", default=FALSE),
        makeLogicalLearnerParam(id="trace", default=TRUE),
        makeIntegerLearnerParam(id="MaxNWts", default=1000L),
        makeNumericLearnerParam(id="abstol", default=1.0e-4),
        makeNumericLearnerParam(id="reltol", default=1.0e-8)
      )
      			
			.Object = callNextMethod(.Object, pack="nnet", par.set=par.set)
    
      setProperties(.Object, 
        twoclass = TRUE,
        multiclass = TRUE,
        numerics = TRUE,
        factors = TRUE,
        prob = TRUE,
        weights = TRUE
      )
    }
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="myclassif.nnet", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = getTaskFormula(.task)
      if (.task@desc@has.weights)
        nnet(f, data=getTaskData(.task, .subset), weights=.task@weights[.subset], ...)
      else  
        nnet(f, data=getTaskData(.task, .subset), ...)			
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "myclassif.nnet", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			type = switch(.learner@predict.type, response="class", prob="raw")
			p = predict(.model@learner.model, newdata=.newdata, type=type, ...)
			if (type == "class")
				return(as.factor(p))
			else {
				if (length(.model@task.desc@class.levels) == 2) {
          y <- cbind(1-p, p) 
					colnames(y) = .model@task.desc@class.levels
					return(y)
				} else
					return(p)	
			}
		}
)	
