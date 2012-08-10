context("mobSvmModel: mlr interface code")

test_that("mobSvmModel: mlr interface code works", {
	library(locClassData)

	d <- vData(500)
	d <- as.data.frame(d)
	task <- makeClassifTask(data = d, target = "y")

	## verhindern, dass immer die wsk. geschätzt wird?
	lrn <- makeLearner("classif.mobSvmModel", kernel = "linear", minsplit = 200)
	tr1 <- train(lrn, task = task)
	pr1 <- predict(tr1, task = task)
	tr2 <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = d, model = svmModel, kernel = "linear",
		control = mob_control(objfun = deviance, minsplit = 200))
	pr2 <- predict(tr2, out = "class")
	expect_equal(as.numeric(pr1$data$response), pr2)
	# mean(pr1$data$truth != pr1$data$response)
	# predictNode(tr1)
	
	lrn <- makeLearner("classif.mobSvmModel", predict.type = "prob", kernel = "linear", minsplit = 200)
	tr1 <- train(lrn, task = task)
	pr1 <- predict(tr1, task = task)
	tr2 <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = d, model = svmModel, kernel = "linear", probability = TRUE,
		control = mob_control(objfun = deviance, minsplit = 200))
	pr2 <- predict(tr2, out = "posterior", newdata = d, probability = TRUE)
	p = matrix(0, length(pr2), nlevels(d$y))
	colnames(p) = levels(d$y)
	for (i in seq_along(pr2)) {
		p[i, colnames(pr2[[i]])] = pr2[[i]]
	}
	expect_true(all(pr1$data[,3:4] == p))
	# mean(pr1$data$truth != pr1$data$response)
	# predictNode(tr1)
})

