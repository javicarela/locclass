context("osmultinom: mlr interface code")

test_that("osmultinom: mlr interface works", {
	task <- makeClassifTask(data = iris, target = "Species")

	# missing parameters
	lrn <- makeLearner("classif.osmultinom")
	expect_that(train(lrn, task), throws_error("either 'bw' or 'k' have to be specified"))

	# class prediction
	lrn <- makeLearner("classif.osmultinom", par.vals = list(bw = 10, trace = FALSE))
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- osmultinom(Species ~ ., data = iris, bw = 10, trace = FALSE)
	cl <- pred2 <- predict(tr2, type = "class")
	expect_equivalent(pred2, pred1$data$response)

	# posterior prediction
	lrn <- makeLearner("classif.osmultinom", par.vals = list(bw = 10, trace = FALSE), predict.type = "prob")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- osmultinom(Species ~ ., data = iris, bw = 10, trace = FALSE)
	pred2 <- predict(tr2, type = "probs")
	expect_true(all(pred2 == pred1$data[,3:5]))
	expect_equivalent(cl, pred1$data$response)
})
