context("FLXMCLnnet: mlr interface code")

test_that("FLXMCLnnet: mlr interface works", {
	library(locClassData)

	data <- xor3Data(500)
	task <- makeClassifTask(data = as.data.frame(data), target = "y")

	Wts <- runif(9,-0.5, 0.5)

	#### model parameters are passed
	# size, decay, trace, censored
	# default: reps = 1
	lrn <- makeLearner("classif.FLXMCLnnet", centers = 3, trace = FALSE, size = 2, decay = 0.2)
	tr1 <- train(lrn, task)
	expect_equal(length(tr1$learner.model@components), 3)
	pars <- tr1$learner.model@components[[1]][[1]]@parameters
	expect_equal(pars$decay, 0.2)
	expect_equal(pars$n[2], 2)
	expect_true(!pars$censored)
	expect_equal(pars$reps, 1)
	# reps = 2	
	lrn <- makeLearner("classif.FLXMCLnnet", centers = 3, trace = FALSE, size = 1, reps = 2)
	tr1 <- train(lrn, task)
	expect_equal(length(tr1$learner.model@components), 3)
	pars <- tr1$learner.model@components[[1]][[1]]@parameters
	expect_equal(pars$decay, 0)
	expect_equal(pars$n[2], 1)
	expect_true(!pars$censored)
	expect_equal(pars$reps, 2)

	## weighted	
	# class prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLnnet", centers = 3, Wts = Wts, size = 1, trace = FALSE)
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	
	# posterior prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLnnet", par.vals = list(Wts = Wts, size = 1, trace = FALSE, centers = 3, iter.max = 200), predict.type = "prob")
	tr2 <- train(lrn, task)
	pred2 <- predict(tr2, task = task)

	expect_equal(pred1$data$response, pred2$data$response)
	mean(pred1$data$response != pred1$data$truth)
	mean(pred2$data$response != pred1$data$truth)
	mean(pred2$data$response != pred2$data$truth)
	
	set.seed(120)
	cluster <- replicate(5, kmeans(data$x, centers = 3)$cluster)
	tr3 <- myStepFlexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLnnet(Wts = Wts, size = 1, trace = FALSE), cluster = cluster, control = list(iter.max = 200, tolerance = 10^-2))
	tr3 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLnnet(Wts = Wts, size = 1, trace = FALSE), cluster = posterior(tr3), control = list(iter.max = 200))
	pred3 <- mypredict(tr3, aggregate = TRUE)

	expect_true(all(pred3[[1]] == pred2$data[,3:5]))
	
	## hard
	# class prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLnnet", centers = 3, Wts = Wts, size = 1, trace = FALSE, classify = "hard")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)

	# posterior prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLnnet", par.vals = list(Wts = Wts, size = 1, trace = FALSE, centers = 3, iter.max = 200, classify = "hard"), predict.type = "prob")
	tr2 <- train(lrn, task)
	pred2 <- predict(tr2, task = task)

	expect_equal(pred1$data$response, pred2$data$response)

	set.seed(120)
	cluster <- replicate(5, kmeans(data$x, centers = 3)$cluster)
	tr3 <- myStepFlexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLnnet(Wts = Wts, size = 1, trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard", tolerance = 10^-2))
	tr3 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPmultinom(~ x.1 + x.2), model = FLXMCLnnet(Wts = Wts, size = 1, trace = FALSE), cluster = posterior(tr3), control = list(iter.max = 200, classify = "hard"))
	pred3 <- mypredict(tr3, aggregate = TRUE)

	expect_true(all(pred3[[1]] == pred2$data[,3:5]))
	
})
