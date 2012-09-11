context("mobConstantModel: mlr interface code")

test_that("mobConstantModel: mlr interface code works", {
	library(locClassData)

	## generate data
	d <- vData(500)
	d <- as.data.frame(d)
	task <- makeClassifTask(data = d, target = "y")

	## predict.type = "response"
	lrn <- makeLearner("classif.mobConstantModel", minsplit = 20)
	tr1 <- train(lrn, task = task)
	pr1 <- predict(tr1, task = task)
	tr2 <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = d, model = constantModel,
		control = mob_control(objfun = deviance, minsplit = 20))
	pr2 <- predict(tr2)
	expect_equal(as.numeric(pr1$data$response), pr2)
	# mean(pr1$data$truth != pr1$data$response)
	# predictNode(tr1)

	## predict.type = "prob"
	lrn <- makeLearner("classif.mobConstantModel", predict.type = "prob", minsplit = 20)
	tr1 <- train(lrn, task = task)
	pr1 <- predict(tr1, task = task)
	tr2 <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = d, model = constantModel,
		control = mob_control(objfun = deviance, minsplit = 20))
	pr2 <- predict(tr2, out = "posterior")	
	p <- matrix(0, length(pr2), 2)
	colnames(p) = 1:2
	rownames(p) = rownames(d)
	for (i in seq_along(pr2)) {
		p[i, colnames(pr2[[i]])] = pr2[[i]]
	}
	expect_true(all(pr1$data[,3:4] == p))
	# mean(pr1$data$truth != pr1$data$response)

	## type = "node"
	expect_equal(predictNode(tr1), predict(tr2, type = "node"))
})
