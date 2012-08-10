context("damultinom: mlr interface code")

test_that("damultinom: mlr interface works", {
	task <- makeClassifTask(data = iris, target = "Species")

	# missing parameters
	lrn <- makeLearner("classif.damultinom")
	expect_that(train(lrn, task), throws_error("either 'bw' or 'k' have to be specified"))

	# class prediction
	lrn <- makeLearner("classif.damultinom", par.vals = list(bw = 2, trace = FALSE))
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- damultinom(Species ~ ., data = iris, bw = 2, trace = FALSE)
	pred2 <- predict(tr2)
	expect_equivalent(pred2$class, pred1$data$response)

	# posterior prediction
	lrn <- makeLearner("classif.damultinom", par.vals = list(bw = 2, trace = FALSE), predict.type = "prob")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- damultinom(Species ~ ., data = iris, bw = 2, trace = FALSE)
	pred2 <- predict(tr2)
	expect_true(all(pred2$posterior == pred1$data[,3:5]))
	expect_equivalent(pred2$class, pred1$data$response)
})
