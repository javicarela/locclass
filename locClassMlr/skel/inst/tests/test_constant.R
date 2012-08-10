context("constant: mlr interface code")

test_that("constant: mlr interface works", {
	task <- makeClassifTask(data = iris, target = "Species")

	# class prediction
	lrn <- makeLearner("classif.constant")
	set.seed(120)
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	set.seed(120)
	tr2 <- constant(Species ~ ., data = iris)
	pred2 <- predict(tr2)
	expect_equivalent(pred2$class, pred1$data$response)

	# posterior prediction
	lrn <- makeLearner("classif.constant", predict.type = "prob")
	tr1 <- train(lrn, task)
	set.seed(120)
	pred1 <- predict(tr1, task = task)
	tr2 <- constant(Species ~ ., data = iris)
	set.seed(120)
	pred2 <- predict(tr2)
	expect_true(all(pred2$posterior == pred1$data[,3:5]))
})
