#=================================================================================================================
context("FLXMCLnnet")


test_that("FLXMCLnnet: missing classes in clusters", {
	data(iris)
	cluster <- kmeans(iris[,1:4], centers = 2)$cluster
	tr2 <- flexmix(Species ~ ., data = iris, concomitant = FLXPwlda(as.formula(paste("~", paste(colnames(iris)[1:4], collapse = "+")))), model = FLXMCLnnet(size = 1, trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard"))
	pred2 <- mypredict(tr2, aggregate = TRUE)
})
## no problem

test_that("FLXMCLnnet: removing clusters works", {
	set.seed(120)	
	library(locClassData)
	data <- flashData(500)
	cluster <- kmeans(data$x, centers = 12)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = FLXMCLnnet(size = 1, trace = FALSE), cluster = cluster, control = list(iter.max = 200))
	expect_equal(length(tr2@components), 8)
	expect_equal(ncol(tr2@posterior$scaled), 8)
})


#=================================================================================================================
context("FLXMCLnnet")

test_that("predict FLXMCLnnet", {
	set.seed(120)	
	library(locClassData)
	data <- flashData(500)
	cluster <- kmeans(data$x, centers = 2)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = FLXMCLnnet(trace = FALSE, size = 1), cluster = cluster, control = list(iter.max = 200))
	pred1 <- mypredict(tr2, aggregate = FALSE)
	pred2 <- mypredict(tr2, aggregate = FALSE, newdata = data)
	expect_equal(pred1, pred2)
	pred1 <- mypredict(tr2, aggregate = TRUE)
	pred2 <- mypredict(tr2, aggregate = TRUE, newdata = data)
	expect_equal(pred1, pred2)
})


#=================================================================================================================
context("FLXMCLnnet: mlr interface code")

test_that("FLXMCLnnet: mlr interface works", {
	library(mlr)
	library(locClassData)
	source("../../../../mlr/classif.FLXMCLnnet.R")

	data <- xor3Data(500)
	task <- makeClassifTask(data = as.data.frame(data), target = "y")

	Wts <- runif(9,-0.5, 0.5)

	set.seed(120)
	cluster <- kmeans(data$x, centers = 3)$cluster

	# class prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLnnet", centers = 3, Wts = Wts, size = 1, trace = TRUE)
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	
	# posterior prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLnnet", par.vals = list(Wts = Wts, size = 1, trace = FALSE, centers = 3, iter.max = 200), predict.type = "prob")
	tr2 <- train(lrn, task)
	pred2 <- predict(tr2, task = task)

	expect_equal(pred1@df$response, pred2@df$response)
	mean(pred1@df$response != pred1@df$truth)
	mean(pred2@df$response != pred1@df$truth)
	mean(pred2@df$response != pred2@df$truth)
	
	tr3 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = FLXMCLnnet(Wts = Wts, size = 1, trace = FALSE), cluster = cluster, control = list(iter.max = 200))
	pred3 <- mypredict(tr3, aggregate = TRUE)

	expect_true(all(pred3[[1]] == pred2@df[,3:5]))
	
	# class prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLnnet", centers = 3, Wts = Wts, size = 1, trace = TRUE, classify = "hard")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)

	# posterior prediction
	set.seed(120)
	lrn <- makeLearner("classif.FLXMCLnnet", par.vals = list(Wts = Wts, size = 1, trace = TRUE, centers = 3, iter.max = 200, classify = "hard"), predict.type = "prob")
	tr2 <- train(lrn, task)
	pred2 <- predict(tr2, task = task)

	expect_equal(pred1@df$response, pred2@df$response)

	tr3 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = FLXMCLnnet(Wts = Wts, size = 1, trace = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard"))
	pred3 <- mypredict(tr3, aggregate = TRUE)

	expect_true(all(pred3[[1]] == pred2@df[,3:5]))
	
})

#=================================================================================================================

# library(locClassData)
# #d <- vNormalData(500)
# d <- flashData(500)
# grid <- expand.grid(x.1=seq(-6,6,0.2), x.2=seq(-4,4,0.2))


# cluster <- kmeans(d$x, center = 2)$cluster
# model <- FLXMCLnnet(size = 1)
# res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1 + x.2), model = model, cluster = cluster)
# res

# # model <- FLXMCLnnet(size = 1)
# # res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1), model = model, k = 2)
# # res

# # model <- FLXMCLnnet(size = 1)
# # res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1), model = model, k = 2, control = list(classify = "hard"))
# # res
# # res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1), model = model, cluster = res@cluster)
# # res

# plot(d$x, col = res@cluster, cex = res@posterior$scaled[,1])
# plot(d$x, col = res@cluster, cex = res@posterior$scaled[,2])

# plot(d$x, col = d$y, cex = res@posterior$scaled[,1])
# plot(d$x, col = d$y, cex = res@posterior$scaled[,2])

# pred <- predict(res, newdata = as.data.frame(d), local.aggregate = TRUE)

# pred.grid <- predict(res, newdata = grid)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add = TRUE)

# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))), add = TRUE)

# pred.grid <- predict(res, newdata = grid, local.aggregate = TRUE)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add  = TRUE)

# n <- nnet(y ~ ., data = d, size = 2)
# pred.grid <- predict(n, grid)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[,1], length(seq(-6,6,0.2))), add  = TRUE)
