context("FLXMCLqda: mlr interface code")

test_that("FLXMCLqda: mlr interface works", {
	library(mlr)

	set.seed(150)
	data <- xor3Data(500)
	task <- makeClassifTask(data = as.data.frame(data), target = "y")

	# class prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLqda", centers = 3, method = "ML")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	
	# posterior prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLqda", par.vals = list(method = "ML", centers = 3, iter.max = 200), predict.type = "prob")
	tr2 <- train(lrn, task)
	pred2 <- predict(tr2, task = task)

	expect_equal(pred1$data$response, pred2$data$response)
	mean(pred1$data$response != pred1$data$truth)
	mean(pred1$data$response != pred2$data$truth)
	mean(pred2$data$response != pred2$data$truth)

	set.seed(120)
	cluster <- replicate(5, kmeans(data$x, centers = 3)$cluster)
	tr3 <- myStepFlexmix(y ~ ., data = as.data.frame(data), model = FLXMCLqda(method = "ML"), cluster = cluster, control = list(iter.max = 200, tolerance = 10^-2))
	tr3 <- flexmix(y ~ ., data = as.data.frame(data), model = FLXMCLqda(method = "ML"), cluster = posterior(tr3), control = list(iter.max = 200))
	pred3 <- mypredict(tr3, aggregate = TRUE)

	expect_true(all(pred3[[1]]/rowSums(pred3[[1]]) == pred2$data[,3:5]))
	
	# class prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLqda", centers = 3, classify = "hard")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)

	# posterior prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLqda", par.vals = list(centers = 3, iter.max = 200, classify = "hard"), predict.type = "prob")
	tr2 <- train(lrn, task)
	pred2 <- predict(tr2, task = task)
	
	expect_equal(pred1$data$response, pred2$data$response)
	mean(pred1$data$response != pred1$data$truth)
	mean(pred1$data$response != pred2$data$truth)
	mean(pred2$data$response != pred2$data$truth)

	set.seed(120)
	cluster <- replicate(5, kmeans(data$x, centers = 3)$cluster)
	tr3 <- myStepFlexmix(y ~ ., data = as.data.frame(data), model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200, tolerance = 10^-2, classify = "hard"))
	tr3 <- flexmix(y ~ ., data = as.data.frame(data), model = FLXMCLqda(), cluster = posterior(tr3), control = list(iter.max = 200, classify = "hard"))
	pred3 <- mypredict(tr3, aggregate = TRUE)

	expect_true(all(pred3[[1]]/(rowSums(pred3[[1]])) == pred2$data[,3:5]))
	
})
