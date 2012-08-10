context("wsvm: mlr interface code")

test_that("wsvm: mlr interface works", {
	task <- makeClassifTask(data = iris, target = "Species")

	# class prediction
	lrn <- makeLearner("classif.wsvm")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- wsvm(Species ~ ., data = iris)
	pred2 <- predict(tr2)
	expect_equivalent(pred2, pred1$data$response)

	# posterior prediction
	lrn <- makeLearner("classif.wsvm", par.vals = list(kernel = "linear"), predict.type = "prob")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- wsvm(Species ~ ., data = iris, kernel = "linear", probability = TRUE)
	pred2 <- predict(tr2, newdata = iris, probability = TRUE)
	expect_true(all(attr(pred2, "probabilities") == pred1$data[,3:5]))
	expect_equivalent(pred2, pred1$data$response)
})
