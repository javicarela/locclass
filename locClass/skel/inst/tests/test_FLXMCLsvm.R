#=================================================================================================================
context("FLXMCLsvm")


test_that("FLXMCLsvm: missing classes in clusters", {
	data(iris)
	cluster <- kmeans(iris[,1:4], centers = 2)$cluster
	expect_that(tr2 <- flexmix(Species ~ ., data = iris, concomitant = FLXPwlda(as.formula(paste("~", paste(colnames(iris)[1:4], collapse = "+")))), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200, classify = "hard")), throws_error("Model is empty!"))
})


test_that("FLXMCLsvm: removing clusters works", {
	set.seed(120)	
	library(locClassData)
	data <- flashData(500)
	cluster <- kmeans(data$x, centers = 12)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = FLXMCLsvm(), cluster = cluster, control = list(iter.max = 200))
	expect_equal(length(tr2@components), 8)
	expect_equal(ncol(tr2@posterior$scaled), 8)
})


#=================================================================================================================
context("FLXMCLsvm")

test_that("predict FLXMCLsvm", {
	set.seed(120)	
	library(locClassData)
	data <- flashData(500)
	cluster <- kmeans(data$x, centers = 2)$cluster
	tr2 <- flexmix(y ~ ., data = as.data.frame(data), concomitant = FLXPwlda(~ x.1 + x.2), model = FLXMCLsvm(kernel = "linear", fitted = FALSE), cluster = cluster, control = list(iter.max = 200))
	pred1 <- mypredict(tr2, aggregate = FALSE)
	pred2 <- mypredict(tr2, aggregate = FALSE, newdata = data)
	expect_equal(pred1, pred2)
	pred1 <- mypredict(tr2, aggregate = TRUE)
	pred2 <- mypredict(tr2, aggregate = TRUE, newdata = data)
	expect_equal(pred1, pred2)
})


#=================================================================================================================

# library(locClass)
# d <- flashData(500)
# #d <- vNormalData(500)
# grid <- expand.grid(x.1=seq(-6,6,0.2), x.2=seq(-4,4,0.2))

# model <- FLXMCLsvm(kernel = "linear")
# cluster <- kmeans(d$x, center = 2)$cluster
# res <- flexmix(y ~ ., data = as.data.frame(d), concomitant = FLXPmultinom(~ x.1 + x.2), model = model, cluster = cluster)
# res

# plot(d$x, col = res@cluster, cex = res@posterior$scaled[,1])
# plot(d$x, col = res@cluster, cex = res@posterior$scaled[,2])

# plot(d$x, col = d$y, cex = res@posterior$scaled[,1])
# plot(d$x, col = d$y, cex = res@posterior$scaled[,2])

# pred <- predict(res, newdata = as.data.frame(d), local.aggregate = TRUE)

# pred.grid <- predict(res, newdata = grid)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid$Comp.1["decision",][[1]], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid$Comp.1["decision",][[1]], length(seq(-6,6,0.2))), add = TRUE)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid$Comp.2["decision",][[1]], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid$Comp.2["decision",][[1]], length(seq(-6,6,0.2))), add = TRUE)

# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid$Comp.1["posterior",][[1]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid$Comp.1["posterior",][[1]][,1], length(seq(-6,6,0.2))), add = TRUE)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid$Comp.2["posterior",][[1]][,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid$Comp.2["posterior",][[1]][,1], length(seq(-6,6,0.2))), add = TRUE)


# pred.grid <- predict(res, newdata = grid, local.aggregate = TRUE)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]]$decision, length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]]$decision, length(seq(-6,6,0.2))), add = TRUE)
# points(d$x, col = as.numeric(d$y)+2)

# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]]$posterior[,1], length(seq(-6,6,0.2))))
# contour(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.grid[[1]]$posterior[,1], length(seq(-6,6,0.2))), add = TRUE)
# points(d$x, col = as.numeric(d$y)+2)

# ## plot predicted local membership
# pred.loc.grid <- predict(res@concomitant, newdata = grid)
# image(seq(-6,6,0.2), seq(-4,4,0.2), matrix(pred.loc.grid[,1], length(seq(-6,6,0.2))))

