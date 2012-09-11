context("dasvm: mlr interface code")

test_that("dasvm: mlr interface works", {
	task <- makeClassifTask(data = iris, target = "Species")

	# missing parameters
	lrn <- makeLearner("classif.dasvm")
	expect_that(train(lrn, task), throws_error("either 'bw' or 'k' have to be specified"))

	# class prediction
	lrn <- makeLearner("classif.dasvm", par.vals = list(bw = 10))
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- dasvm(Species ~ ., data = iris, bw = 10)
	pred2 <- predict(tr2)
	expect_equivalent(pred2, pred1$data$response)

	# posterior prediction
	lrn <- makeLearner("classif.dasvm", par.vals = list(bw = 10), predict.type = "prob")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- dasvm(Species ~ ., data = iris, bw = 10, probability = TRUE)
	pred2 <- predict(tr2, newdata = iris, probability = TRUE)
	expect_true(all(attr(pred2, "probabilities") == pred1$data[,3:5]))
	expect_equivalent(pred2, pred1$data$response)
})
