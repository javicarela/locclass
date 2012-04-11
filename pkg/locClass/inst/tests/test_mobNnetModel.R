#=================================================================================================================
context("mobNnetModel: mlr interface code")

test_that("mobNnetModel: mlr interface code works", {
	library(locClassData)
	library(party)
	library(mlr)
	source("../../../../mlr/classif.mobNnetModel.R")

	## generate data
	d <- vData(500)
	d <- as.data.frame(d)
	task <- makeClassifTask(data = d, target = "y")

	Wts <- runif(5, -0.5, 0.5) 
	
	## predict.type = "response"
	lrn <- makeLearner("classif.mobNnetModel", size = 1, minsplit = 200, trace = FALSE, Wts = Wts)
	tr1 <- train(lrn, task = task)
	pr1 <- predict(tr1, task = task)
	tr2 <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = d, model = nnetModel, size = 1,
		control = mob_control(objfun = deviance, minsplit = 200), trace = FALSE, Wts = Wts)
	pr2 <- predict(tr2, out = "class")
	expect_equal(as.numeric(pr1@df$response), pr2)
	# mean(pr1@df$truth != pr1@df$response)
	# predictNode(tr1)

	## predict.type = "prob"
	lrn <- makeLearner("classif.mobNnetModel", predict.type = "prob", size = 1, minsplit = 200, trace = FALSE, Wts = Wts)
	tr1 <- train(lrn, task = task)
	pr1 <- predict(tr1, task = task)
	tr2 <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = d, model = nnetModel, size = 1,
		control = mob_control(objfun = deviance, minsplit = 200), trace = FALSE, Wts = Wts)
	pr2 <- do.call("rbind", predict(tr2, out = "posterior"))
	expect_true(all(pr1@df[,3:4] == pr2))
	# mean(pr1@df$truth != pr1@df$response)
	# predictNode(tr)
})
