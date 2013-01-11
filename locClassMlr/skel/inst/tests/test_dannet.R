context("dannet: mlr interface code")

test_that("dannet: mlr interface works", {
	task <- makeClassifTask(data = iris, target = "Species")

	# missing parameters
	lrn <- makeLearner("classif.dannet")
	expect_that(train(lrn, task), throws_error("either 'bw' or 'k' have to be specified"))

	# default: reps = 1
	lrn <- makeLearner("classif.dannet", bw = 2, size = 1, trace = FALSE)
	tr1 <- train(lrn, task)
	expect_equal(tr1$learner.model$bw, 2)
	expect_equal(tr1$learner.model$n[2], 1)
	expect_equal(tr1$learner.model$reps, 1)
	# reps = 3
	lrn <- makeLearner("classif.dannet", bw = 2, size = 1, reps = 3, trace = FALSE)
	tr1 <- train(lrn, task)
	tr1 <- train(lrn, task)
	expect_equal(tr1$learner.model$bw, 2)
	expect_equal(tr1$learner.model$n[2], 1)
	expect_equal(tr1$learner.model$reps, 3)

	Wts = runif(19, -0.5, 0.5)
	
	# class prediction
	lrn <- makeLearner("classif.dannet", par.vals = list(bw = 2, Wts = Wts, size = 2, trace = FALSE))
	tr1 <- train(lrn, task)
	expect_equal(tr1$learner.model$reps, 1)
	pred1 <- predict(tr1, task = task)
	tr2 <- dannet(Species ~ ., data = iris, bw = 2, Wts = Wts, size = 2, trace = FALSE)
	expect_equal(tr2$reps, 1)
	pred2 <- predict(tr2)
	expect_equivalent(pred2$class, pred1$data$response)

	# posterior prediction
	lrn <- makeLearner("classif.dannet", par.vals = list(bw = 2, Wts = Wts, size = 2, trace = FALSE, reps = 3), predict.type = "prob")
	tr1 <- train(lrn, task)
	expect_equal(tr1$learner.model$reps, 3)
	pred1 <- predict(tr1, task = task)
	tr2 <- dannet(Species ~ ., data = iris, bw = 2, Wts = Wts, size = 2, trace = FALSE, reps = 3)
	expect_equal(tr2$reps, 3)
	pred2 <- predict(tr2)
	expect_true(all(pred2$posterior == pred1$data[,3:5]))
	expect_equivalent(pred2$class, pred1$data$response)
})
