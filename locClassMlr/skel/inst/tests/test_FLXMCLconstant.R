context("FLXMCLconstant: mlr interface code")

test_that("FLXMCLconstant: mlr interface works", {
	library(locClassData)

	data <- xor3Data(500)
	task <- makeClassifTask(data = as.data.frame(data), target = "y")

	#### model parameters are passed
	# centers
	lrn <- makeLearner("classif.FLXMCLconstant", centers = 9)
	tr1 <- train(lrn, task)
	expect_equal(length(tr1$learner.model@components), 9)

	## weighted
	# class prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLconstant", centers = 9)
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)

	# posterior prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLconstant", par.vals = list(centers = 9, iter.max = 200), predict.type = "prob")
	tr2 <- train(lrn, task)
	pred2 <- predict(tr2, task = task)

	expect_equal(pred1$data$response, pred2$data$response)
	mean(pred1$data$response != pred1$data$truth)
	mean(pred2$data$response != pred1$data$truth)
	mean(pred2$data$response != pred2$data$truth)
	
	set.seed(120)
	cluster <- replicate(5, kmeans(data$x, centers = 9)$cluster)
	tr3 <- myStepFlexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200, tolerance = 10^-2, verb = 1))
	tr3 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLconstant(), cluster = posterior(tr3), control = list(iter.max = 200, verb = 1))
	pred3 <- mypredict(tr3, aggregate = TRUE)

	expect_true(all(pred3[[1]] == pred2$data[,3:5]))
	
	## hard
	# class prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLconstant", centers = 9, classify = "hard")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)

	# posterior prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLconstant", par.vals = list(centers = 9, iter.max = 200), predict.type = "prob", classify = "hard")
	tr2 <- train(lrn, task)
	pred2 <- predict(tr2, task = task)

	expect_equal(pred1$data$response, pred2$data$response)
	mean(pred1$data$response != pred1$data$truth)
	mean(pred2$data$response != pred1$data$truth)
	mean(pred2$data$response != pred2$data$truth)

	tr3 <- myStepFlexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLconstant(), cluster = cluster, control = list(iter.max = 200, tolerance = 10^-2, classify = "hard", verb = 1))
	tr3 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLconstant(), cluster = posterior(tr3), control = list(iter.max = 200, classify = "hard", verb = 1))
	pred3 <- mypredict(tr3, aggregate = TRUE)

	expect_true(all(pred3[[1]] == pred2$data[,3:5]))

})
