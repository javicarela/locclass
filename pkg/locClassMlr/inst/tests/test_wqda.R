context("wqda: mlr interface code")

test_that("wqda: mlr interface works", {
	task <- makeClassifTask(data = iris, target = "Species")

	# class prediction
	lrn <- makeLearner("classif.wqda")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- wqda(Species ~ ., data = iris, method = "ML")
	pred2 <- predict(tr2)
	expect_equivalent(pred2$class, pred1$data$response)

	# posterior prediction
	lrn <- makeLearner("classif.wqda", par.vals = list(method = "ML"), predict.type = "prob")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- wqda(Species ~ ., data = iris, method = "ML")
	pred2 <- predict(tr2)
	expect_true(all(pred2$posterior == pred1$data[,3:5]))
	expect_equivalent(pred2$class, pred1$data$response)
})
