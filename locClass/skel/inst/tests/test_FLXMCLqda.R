#=================================================================================================================
context("FLXMCLqda")

## todo: problem with more than 2 classes
# test_that("FLXMCLqda: missing classes in clusters", {
	# data(iris)
	# cluster <- kmeans(iris[,1:4], centers = 2)$cluster
	# tr2 <- flexmix(Species ~ ., data = iris, concomitant = FLXPwlda(as.formula(paste("~", paste(colnames(iris)[1:4], collapse = "+")))), model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200, classify = "hard"))
# })


test_that("FLXMCLqda: removing clusters works", {
	set.seed(120)	
	library(locClassData)
	data <- flashData(500)
	cluster <- kmeans(data$x, centers = 12)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200))
	expect_equal(length(tr2@components), 8)
	expect_equal(ncol(tr2@posterior$scaled), 8)
})


#=================================================================================================================
context("FLXMCLqda")

test_that("predict FLXMCLqda", {
	set.seed(120)	
	library(locClassData)
	data <- flashData(500)
	cluster <- kmeans(data$x, centers = 2)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200))
	pred1 <- mypredict(tr2, aggregate = FALSE)
	pred2 <- mypredict(tr2, aggregate = FALSE, newdata = data)
	expect_equal(pred1, pred2)
	pred1 <- mypredict(tr2, aggregate = TRUE)
	pred2 <- mypredict(tr2, aggregate = TRUE, newdata = data)
	expect_equal(pred1, pred2)
})


#=================================================================================================================
context("FLXMCLqda: mlr interface code")

test_that("FLXMCLqda: mlr interface works", {
	library(mlr)
	library(locClassData)
	source("../../../../mlr/classif.FLXMCLqda.R")

	data <- xor3Data(500)
	task <- makeClassifTask(data = as.data.frame(data), target = "y")

	set.seed(120)
	cluster <- kmeans(data$x, centers = 3)$cluster

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

	expect_equal(pred1@df$response, pred2@df$response)
	mean(pred1@df$response != pred1@df$truth)
	mean(pred1@df$response != pred2@df$truth)
	mean(pred2@df$response != pred2@df$truth)

	tr3 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = FLXMCLqda(method = "ML"), cluster = cluster, control = list(iter.max = 200))
	pred3 <- mypredict(tr3, aggregate = TRUE)

	expect_true(all(pred3[[1]] == pred2@df[,3:4]))
	
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
	
	expect_equal(pred1@df$response, pred2@df$response)
	mean(pred1@df$response != pred1@df$truth)
	mean(pred1@df$response != pred2@df$truth)
	mean(pred2@df$response != pred2@df$truth)

	tr3 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = FLXMCLqda(), cluster = cluster, control = list(iter.max = 200, classify = "hard"))
	pred3 <- mypredict(tr3, aggregate = TRUE)

	expect_true(all(pred3[[1]] == pred2@df[,3:5]))
	
})

#=================================================================================================================
# library(locClassData)
# #d <- vNormalQuadraticData(500)
# d <- flashData(500)
# grid <- expand.grid(x.1=seq(-6,6,0.2), x.2=seq(-4,4,0.2))

# cluster <- kmeans(d$x, center = 2)$cluster
# model <- FLXMCLqda()
# res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1 + x.2), model = model, cluster = cluster)
# res


# # model <- FLXMCLqda()
# # res <- flexmix(y ~ ., data = as.data.frame(d), 
# # concomitant = FLXPmultinom(~ x.1), model = model, k = 2,
# # control = list(classify = "hard"))
# # res

# # res <- flexmix(y ~ ., data = as.data.frame(d), 
# # concomitant = FLXPmultinom(~ x.1), model = model, cluster = res@cluster)
# # res

# plot(d$x, col = res@cluster, cex = res@posterior$scaled[,1])
# plot(d$x, col = res@cluster, cex = res@posterior$scaled[,2])

# plot(d$x, col = d$y, cex = res@posterior$scaled[,1])
# plot(d$x, col = d$y, cex = res@posterior$scaled[,2])
# points(res@components[[1]][[1]]@parameters$means, col = "blue", pch = 19, cex = 2)
# points(res@components[[2]][[1]]@parameters$means, col = "green", pch = 19, cex = 2)
# # points(res@components[[3]][[1]]@parameters$means, col = "orange", pch = 19, cex = 2)
# # points(res@components[[4]][[1]]@parameters$means, col = "red", pch = 19, cex = 2)

# pred <- predict(res, newdata = as.data.frame(d), local.aggregate = TRUE)

# pred.grid <- predict(res, newdata = grid)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add = TRUE)

# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[2]][,1], length(seq(-6,6,0.2))), add = TRUE)

# pred.grid <- predict(res, newdata = grid, local.aggregate = TRUE)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]][,1], length(seq(-6,6,0.2))), add  = TRUE)
# points(d$x, col = as.numeric(d$y)+2)

# ## plot predicted local membership
# pred.loc.grid <- predict(res@concomitant, newdata = grid)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.loc.grid[,1], length(seq(-6,6,0.2))))



