#=================================================================================================================
context("mobLdaModel: mlr interface code")

test_that("mobLdaModel: mlr interface code works", {
	library(locClassData)
	library(party)
	source("../../../../mlr/classif.mobLdaModel.R")

	## generate data
	d <- vData(500)
	d <- as.data.frame(d)
	task <- makeClassifTask(data = d, target = "y")

	## predict.type = "response"
	ldaLearner <- makeLearner("classif.mobLdaModel", minsplit = 200)
	tr1 <- train(ldaLearner, task = task)
	pr1 <- predict(tr1, task = task)
	tr2 <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = d, model = ldaModel,
		control = mob_control(objfun = deviance, minsplit = 200))
	pr2 <- predict(tr2)
	expect_equal(as.numeric(pr1@df$response), pr2)
	# mean(pr1@df$truth != pr1@df$response)
	# predictNode(tr1)

	## predict.type = "prob"
	ldaLearner <- makeLearner("classif.mobLdaModel", predict.type = "prob", minsplit = 200)
	tr1 <- train(ldaLearner, task = task)
	pr1 <- predict(tr1, task = task)
	tr2 <- mob(y ~ x.1 + x.2 | x.1 + x.2, data = d, model = ldaModel,
		control = mob_control(objfun = deviance, minsplit = 200))
	pr2 <- do.call("rbind", predict(tr2, out = "posterior"))
	expect_true(all(pr1@df[,3:4] == pr2))
	# mean(pr1@df$truth != pr1@df$response)
	# predictNode(tr1)
})
