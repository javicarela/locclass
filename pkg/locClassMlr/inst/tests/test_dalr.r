context("dalr: mlr interface code")

test_that("dalr: mlr interface works", {
	data(iris)
	iris2 <- iris[51:150,]
	iris2$Species <- factor(iris2$Species, levels = unique(iris2$Species))
	task <- makeClassifTask(data = iris2, target = "Species")

	# missing parameters
	lrn <- makeLearner("classif.dalr")
	expect_that(train(lrn, task), throws_error("either 'bw' or 'k' have to be specified"))

	# class prediction
	lrn <- makeLearner("classif.dalr", par.vals = list(bw = 10))
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- dalr(Species ~ ., data = iris2, bw = 10)
	pred2 <- predict(tr2)
	expect_equivalent(pred2$class, pred1$data$response)

	# posterior prediction
	lrn <- makeLearner("classif.dalr", par.vals = list(bw = 10), predict.type = "prob")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- dalr(Species ~ ., data = iris2, bw = 10)
	pred2 <- predict(tr2)
	expect_true(all(pred2$posterior == pred1$data[,3:4]))
	expect_equivalent(pred2$class, pred1$data$response)
})
