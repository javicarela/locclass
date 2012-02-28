#=================================================================================================================
context("mobQdaModel: mlr interface code")

test_that("mobQdaModel: mlr interface code works", {
	library(locClassData)
	library(party)
	source("../../../../mlr/classif.mobQdaModel.R")

	## generate data
	d <- vData(500)
	d <- as.data.frame(d)
	task <- makeClassifTask(data = d, target = "y")

	## predict.type = "response"
	qdaLearner <- makeLearner("classif.mobQdaModel", minsplit = 200)
	tr1 <- train(qdaLearner, task = task)
	pr1 <- predict(tr1, task = task)
	tr2 <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = d, model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 200))
	pr2 <- predict(tr2, out = "class")
	expect_equal(as.numeric(pr1@df$response), pr2)
	# mean(pr1@df$truth != pr1@df$response)
	# predictNode(tr1)

	## predict.type = "prob"
	qdaLearner <- makeLearner("classif.mobQdaModel", predict.type = "prob", minsplit = 200)
	tr1 <- train(qdaLearner, task = task)
	pr1 <- predict(tr1, task = task)
	tr2 <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = d, model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 200))
	pr2 <- do.call("rbind", predict(tr2, out = "posterior"))
	expect_true(all(pr1@df[,3:4] == pr2))
	# mean(pr1@df$truth != pr1@df$response)
	# predictNode(tr1)
})
