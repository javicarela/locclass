context("FLXMCLlda: mlr interface code")

test_that("FLXMCLlda: mlr interface works", {
	library(locClassData)

	data <- xor3Data(500)
	task <- makeClassifTask(data = as.data.frame(data), target = "y")

	#### model parameters are passed
	# centers, method 
	lrn <- makeLearner("classif.FLXMCLlda", centers = 3)
	tr1 <- train(lrn, task)
	expect_equal(length(tr1$learner.model@components), 3)
	expect_equal(tr1$learner.model@components[[1]][[1]]@parameters$method, "ML")

	expect_error(lrn <- makeLearner("classif.FLXMCLqda", centers = 3, method = "unbiased"))

	## weighted
	# class prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLlda", centers = 3)
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	
	# posterior prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLlda", par.vals = list(centers = 3, iter.max = 200), predict.type = "prob")
	tr2 <- train(lrn, task)
	pred2 <- predict(tr2, task = task)

	expect_equal(pred1$data$response, pred2$data$response)
	mean(pred1$data$response != pred1$data$truth)
	mean(pred2$data$response != pred1$data$truth)
	mean(pred2$data$response != pred2$data$truth)
	
	set.seed(120)
	cluster <- replicate(5, kmeans(data$x, centers = 3)$cluster)
	tr3 <- myStepFlexmix(y ~ ., data = as.data.frame(data), model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200, tolerance = 10^-2))
	tr3 <- flexmix(y ~ ., data = as.data.frame(data), model = FLXMCLlda(), cluster = posterior(tr3), control = list(iter.max = 200))
	pred3 <- mypredict(tr3, aggregate = TRUE)

	expect_true(all(pred3[[1]]/rowSums(pred3[[1]]) == pred2$data[,3:5]))
	
	## hard
	# class prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLlda", centers = 3, classify = "hard")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)

	# posterior prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLlda", par.vals = list(centers = 3, iter.max = 200), predict.type = "prob", classify = "hard")
	tr2 <- train(lrn, task)
	pred2 <- predict(tr2, task = task)

	expect_equal(pred1$data$response, pred2$data$response)
	mean(pred1$data$response != pred1$data$truth)
	mean(pred2$data$response != pred1$data$truth)
	mean(pred2$data$response != pred2$data$truth)
	
	set.seed(120)
	cluster <- replicate(5, kmeans(data$x, centers = 3)$cluster)
	tr3 <- myStepFlexmix(y ~ ., data = as.data.frame(data), model = FLXMCLlda(), cluster = cluster, control = list(iter.max = 200, tolerance = 10^-2, classify = "hard"))
	tr3 <- flexmix(y ~ ., data = as.data.frame(data), model = FLXMCLlda(), cluster = posterior(tr3), control = list(iter.max = 200, classify = "hard"))
	pred3 <- mypredict(tr3, aggregate = TRUE)

	expect_true(all(pred3[[1]]/rowSums(pred3[[1]]) == pred2$data[,3:5]))	
})
