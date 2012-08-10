context("wlda: mlr interface code")

test_that("wlda: mlr interface works", {
	task <- makeClassifTask(data = iris, target = "Species")

	# class prediction
	lrn <- makeLearner("classif.wlda")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- wlda(Species ~ ., data = iris)
	pred2 <- predict(tr2)
	expect_equivalent(pred2$class, pred1$data$response)

	# posterior prediction
	lrn <- makeLearner("classif.wlda", par.vals = list(method = "ML"), predict.type = "prob")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- wlda(Species ~ ., data = iris, method = "ML")
	pred2 <- predict(tr2)
	expect_true(all(pred2$posterior == pred1$data[,3:5]))
	expect_equivalent(pred2$class, pred1$data$response)
})
