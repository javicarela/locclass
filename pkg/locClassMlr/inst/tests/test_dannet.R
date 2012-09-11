context("dannet: mlr interface code")

test_that("dannet: mlr interface works", {
	task <- makeClassifTask(data = iris, target = "Species")

	# missing parameters
	lrn <- makeLearner("classif.dannet")
	expect_that(train(lrn, task), throws_error("either 'bw' or 'k' have to be specified"))

	Wts = runif(19, -0.5, 0.5)
	
	# class prediction
	lrn <- makeLearner("classif.dannet", par.vals = list(bw = 2, Wts = Wts, size = 2, trace = FALSE))
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- dannet(Species ~ ., data = iris, bw = 2, Wts = Wts, size = 2, trace = FALSE)
	pred2 <- predict(tr2)
	expect_equivalent(pred2$class, pred1$data$response)

	# posterior prediction
	lrn <- makeLearner("classif.dannet", par.vals = list(bw = 2, Wts = Wts, size = 2, trace = FALSE), predict.type = "prob")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- dannet(Species ~ ., data = iris, bw = 2, Wts = Wts, size = 2, trace = FALSE)
	pred2 <- predict(tr2)
	expect_true(all(pred2$posterior == pred1$data[,3:5]))
	expect_equivalent(pred2$class, pred1$data$response)
})
