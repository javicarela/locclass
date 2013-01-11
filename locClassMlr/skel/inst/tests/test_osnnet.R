context("osnnet: mlr interface code")

test_that("osnnet: mlr interface works", {
	task <- makeClassifTask(data = iris, target = "Species")

	# missing parameters
	lrn <- makeLearner("classif.osnnet")
	expect_that(train(lrn, task), throws_error("'size' is missing"))
	
	# default: reps = 1
	lrn <- makeLearner("classif.osnnet", size = 2, trace = FALSE, bw = 4, wf = "gaussian")
	tr1 <- train(lrn, task)
	expect_equal(tr1$learner.model$n[2], 2)
	expect_equal(tr1$learner.model$trace, FALSE)
	expect_equal(tr1$learner.model$bw, 4)
	expect_equal(tr1$learner.model$wf, "gaussian")
	expect_equal(tr1$learner.model$reps, 1)	
	# reps = 2
	lrn <- makeLearner("classif.osnnet", size = 2, trace = FALSE, bw = 4, wf = "gaussian", reps = 2)
	tr1 <- train(lrn, task)
	expect_equal(tr1$learner.model$n[2], 2)
	expect_equal(tr1$learner.model$trace, FALSE)
	expect_equal(tr1$learner.model$bw, 4)
	expect_equal(tr1$learner.model$wf, "gaussian")
	expect_equal(tr1$learner.model$reps, 2)

	Wts <- runif(19, -0.5, 0.5)
	
	# class prediction
	lrn <- makeLearner("classif.osnnet", par.vals = list(Wts = Wts, size = 2, bw = 10, trace = FALSE))
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- osnnet(Species ~ ., data = iris, Wts = Wts, size = 2, bw = 10, trace = FALSE)
	cl <- pred2 <- predict(tr2, type = "class")
	expect_equivalent(pred2, pred1$data$response)

	# posterior prediction
	lrn <- makeLearner("classif.osnnet", par.vals = list(Wts = Wts, size = 2, bw = 10, trace = FALSE), predict.type = "prob")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- osnnet(Species ~ ., data = iris, Wts = Wts, size = 2, bw = 10, trace = FALSE)
	pred2 <- predict(tr2)
	expect_true(all(pred2 == pred1$data[,3:5]))
	expect_equivalent(cl, pred1$data$response)
})
