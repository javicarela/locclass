context("mobQdaModel: mlr interface code")

test_that("mobQdaModel: mlr interface code works", {
	library(locClassData)

	## generate data
	d <- vData(500)
	d <- as.data.frame(d)
	task <- makeClassifTask(data = d, target = "y")

	## predict.type = "response"
	lrn <- makeLearner("classif.mobQdaModel", minsplit = 200)
	tr1 <- train(lrn, task = task)
	pr1 <- predict(tr1, task = task)
	tr2 <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = d, model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 200))
	pr2 <- predict(tr2, out = "class")
	expect_equal(as.numeric(pr1$data$response), pr2)
	# mean(pr1$data$truth != pr1$data$response)
	# predictNode(tr1)

	## predict.type = "prob"
	lrn <- makeLearner("classif.mobQdaModel", predict.type = "prob", minsplit = 200)
	tr1 <- train(lrn, task = task)
	pr1 <- predict(tr1, task = task)
	tr2 <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = d, model = qdaModel,
		control = mob_control(objfun = deviance, minsplit = 200))
	pr2 <- do.call("rbind", predict(tr2, out = "posterior"))
	expect_true(all(pr1$data[,3:4] == pr2))
	# mean(pr1$data$truth != pr1$data$response)

	## type = "node"
	expect_equal(predictNode(tr1), predict(tr2, type = "node"))
})
