context("FLXMCLsvm: mlr interface code")

test_that("FLXMCLsvm: mlr interface works", {
	library(locClassData)

	data <- xor3Data(500)
	task <- makeClassifTask(data = as.data.frame(data), target = "y")

	set.seed(120)
	cluster <- kmeans(data$x, centers = 3)$cluster

	# class prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLsvm", centers = 3, kernel = "linear", fitted = FALSE)
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	
	# posterior prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLsvm", par.vals = list(kernel = "linear", centers = 3, iter.max = 200), predict.type = "prob")
	tr2 <- train(lrn, task)
	pred2 <- predict(tr2, task = task)

	expect_equal(pred1$data$response, pred2$data$response) ## Problem: Vorhersagen von mlr basierend auf Wsk.en sind Mist, weil Wsk.schätzer Mist sind
	mean(pred1$data$response != pred1$data$truth)
	mean(pred2$data$response != pred1$data$truth)
	mean(pred2$data$response != pred2$data$truth)

	tr3 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = FLXMCLsvm(kernel = "linear"), cluster = cluster, control = list(iter.max = 200))
	pred3 <- mypredict(tr3, aggregate = TRUE)
	
	expect_true(all(pred3[[1]]$posterior == pred2$data[,3:5]))
	
	# class prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLsvm", kernel = "linear", centers = 3, classify = "hard")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)

	# posterior prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLsvm", par.vals = list(kernel = "linear", centers = 3, iter.max = 200, classify = "hard"), predict.type = "prob")
	tr2 <- train(lrn, task)
	pred2 <- predict(tr2, task = task)

	expect_equal(pred1$data$response, pred2$data$response)  ###
	mean(pred1$data$response != pred1$data$truth)
	mean(pred2$data$response != pred1$data$truth)
	mean(pred2$data$response != pred2$data$truth)

	tr3 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = FLXMCLsvm(kernel = "linear"), cluster = cluster, control = list(iter.max = 200, classify = "hard"))
	pred3 <- mypredict(tr3, aggregate = TRUE)

	expect_true(all(pred3[[1]]$posterior == pred2$data[,3:5]))
	
})
