context("nnetRep: mlr interface code")

test_that("nnetRep: mlr interface works", {
	task <- makeClassifTask(data = iris, target = "Species")

	# missing parameters
	lrn <- makeLearner("classif.nnetRep", trace = FALSE)
	expect_that(train(lrn, task), throws_error("'size' is missing"))

	# class prediction
	# default: reps = 1
	lrn <- makeLearner("classif.nnetRep", par.vals = list(size = 2, trace = TRUE))
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	# reps = 3
	set.seed(123)
	lrn <- makeLearner("classif.nnetRep", par.vals = list(size = 2, trace = TRUE, reps = 3))
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	set.seed(123)
	tr2 <- nnetRep(3, Species ~ ., data = iris, size = 2, trace = TRUE)
	cl <- pred2 <- as.factor(predict(tr2, type = "class"))
	expect_equivalent(pred2, pred1$data$response)

	# posterior prediction
	set.seed(123)
	lrn <- makeLearner("classif.nnetRep", par.vals = list(size = 2, trace = FALSE, reps = 3), predict.type = "prob")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	set.seed(123)
	tr2 <- nnetRep(3, Species ~ ., data = iris, size = 2, trace = FALSE)
	pred2 <- predict(tr2)
	expect_true(all(pred2 == pred1$data[,3:5]))
	expect_equivalent(cl, pred1$data$response)
})
