library(locClass)

mod <- e1071:::svm(Species ~ ., data = iris, probability = TRUE)
e1071:::predict.svm(mod, iris, probability = TRUE)


#=====================================================================================================================
## check if decision value weighting method works correctly
mod <- e1071:::svm(Species ~ Sepal.Length + Sepal.Width, data = iris)#, kernel = "linear")
x1 <- seq(4,8,0.05)
x2 <- seq(2,5,0.05)
iris.grid <- expand.grid(Sepal.Length = x1, Sepal.Width = x2)
pred <- e1071:::predict.svm(mod, iris.grid, decision.values = TRUE, main = colnames(decision)[1])
decision.grid <- attr(pred, "decision.values")

filled.contour(x1, x2, matrix(decision.grid[,1], length(x1)), main = colnames(decision.grid)[1])
filled.contour(x1, x2, matrix(decision.grid[,2], length(x1)), main = colnames(decision.grid)[2])
filled.contour(x1, x2, matrix(decision.grid[,3], length(x1)), main = colnames(decision.grid)[3])

plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species, main = colnames(decision.grid)[1])
contour(x1, x2, matrix(decision.grid[,1], length(x1)), add = TRUE, level = c(-1,0,1))

plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species, main = colnames(decision.grid)[2])
contour(x1, x2, matrix(decision.grid[,2], length(x1)), add = TRUE, level = c(-1,0,1))

plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species, main = colnames(decision.grid)[3])
contour(x1, x2, matrix(decision.grid[,3], length(x1)), add = TRUE, level = c(-1,0,1))

mod <- e1071:::svm(Species ~ Sepal.Length + Sepal.Width, data = iris)#, kernel = "linear")
pred <- e1071:::predict.svm(mod, iris, decision.values = TRUE)
decision <- attr(pred, "decision.values")

###
dec <- sapply(1:n, function(x) decision[x,grep(iris$Species[x], colnames(decision))]) 
prob <- sapply(1:n, function(x) min(abs(decision[x, grep(iris$Species[x], colnames(decision))])))
##

inds <- sapply(iris$Species, function(x) grep(x, colnames(decision))) 
n <- 150
dec <- cbind(decision[cbind(1:n, inds[1,1:n])], decision[cbind(1:n, inds[2,1:n])])
problem <- max.col(-abs(dec))
wf <- gaussian(bw = 0.5)
w <- wf(abs(dec[cbind(1:n,problem)])) 

plot(iris[,1], iris[,2], col = iris$Species, cex = w*10)
legend("bottomright", legend = levels(iris$Species), col = as.numeric(unique(iris$Species)), lty = 1)
contour(x1, x2, matrix(decision.grid[,1], length(x1)), add = TRUE, level = -1:1, label = colnames(decision.grid)[1])
contour(x1, x2, matrix(decision.grid[,2], length(x1)), add = TRUE, level = -1:1, label = colnames(decision.grid)[2])
contour(x1, x2, matrix(decision.grid[,3], length(x1)), add = TRUE,  level = -1:1,label = colnames(decision.grid)[3])
##### durch beta teilen wichtig???

mean(pred != iris$Species)

####
mod <- dasvm(Species ~ Sepal.Length + Sepal.Width, data = iris, wf = "gaussian", bw = 0.8, itr = 10, method = "decision", kernel = "linear")
x1 <- seq(4,8,0.05)
x2 <- seq(2,5,0.05)
#mod$case.weights
plot(iris[,1], iris[,2], col = iris$Species, cex = mod$case.weights[[1]]*10)
plot(iris[,1], iris[,2], col = iris$Species, cex = mod$case.weights[[2]]*10)
plot(iris[,1], iris[,2], col = iris$Species, cex = mod$case.weights[[3]]*10)
plot(iris[,1], iris[,2], col = iris$Species, cex = mod$case.weights[[11]]*10)
legend("bottomright", legend = levels(iris$Species), col = as.numeric(unique(iris$Species)), lty = 1)

iris.grid <- expand.grid(Sepal.Length = x1, Sepal.Width = x2)
pred <- predict(mod, newdata = iris.grid, decision.values = TRUE)
decision.grid <- attr(pred, "decision.values")
contour(x1, x2, matrix(decision.grid[,1], length(x1)), add = TRUE, level = -1:1, label = colnames(decision.grid)[1])
contour(x1, x2, matrix(decision.grid[,2], length(x1)), add = TRUE, level = -1:1, label = colnames(decision.grid)[2])
contour(x1, x2, matrix(decision.grid[,3], length(x1)), add = TRUE, level = -1:1, label = colnames(decision.grid)[3])

#=====================================================================================================================
## check if probability weighting method works correctly
mod <- e1071:::svm(Species ~ Sepal.Length + Sepal.Width, data = iris, probability = TRUE)#, kernel = "linear")
x1 <- seq(4,8,0.05)
x2 <- seq(2,5,0.05)
iris.grid <- expand.grid(Sepal.Length = x1, Sepal.Width = x2)
pred <- e1071:::predict.svm(mod, iris.grid, probability = TRUE)
prob.grid <- attr(pred, "probabilities")

filled.contour(x1, x2, matrix(prob.grid[,1], length(x1)), main = colnames(prob.grid)[1])
filled.contour(x1, x2, matrix(prob.grid[,2], length(x1)), main = colnames(prob.grid)[2])
filled.contour(x1, x2, matrix(prob.grid[,3], length(x1)), main = colnames(prob.grid)[3])

plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species, main = colnames(prob.grid)[1])
contour(x1, x2, matrix(prob.grid[,1], length(x1)), add = TRUE)

plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species, main = colnames(prob.grid)[2])
contour(x1, x2, matrix(prob.grid[,2], length(x1)), add = TRUE)

plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species, main = colnames(prob.grid)[3])
contour(x1, x2, matrix(prob.grid[,3], length(x1)), add = TRUE)


mod <- e1071:::svm(Species ~ Sepal.Length + Sepal.Width, data = iris, probability = TRUE)#, kernel = "linear")
pred <- e1071:::predict.svm(mod, iris, probability = TRUE)
post <- attr(pred, "probabilities")
spost <- apply(post, 1, sort, decreasing = TRUE)
wf <- gaussian(bw = 0.3)
w <- wf((spost[1,] - spost[2,]))    # largest if both probabilities are equal

plot(iris[,1], iris[,2], col = iris$Species, cex = w*10)
legend("bottomright", legend = levels(iris$Species), col = as.numeric(unique(iris$Species)), lty = 1)
contour(x1, x2, matrix(prob.grid[,1], length(x1)), add = TRUE, label = colnames(prob.grid)[1])
contour(x1, x2, matrix(prob.grid[,2], length(x1)), add = TRUE, label = colnames(prob.grid)[2])
contour(x1, x2, matrix(prob.grid[,3], length(x1)), add = TRUE, label = colnames(prob.grid)[3])

plot(iris[,1], iris[,2], col = iris$Species, cex = w*10)
legend("bottomright", legend = levels(iris$Species), col = as.numeric(unique(iris$Species)), lty = 1)
sprob.grid <- apply(prob.grid, 1, max)
contour(x1, x2, matrix(sprob.grid, length(x1)), add = TRUE, levels = c(0.45, 0.5, 0.55, 0.6))



mod <- dasvm(Species ~ Sepal.Length + Sepal.Width, data = iris, wf = "gaussian", bw = 0.5, method = "prob", probability = TRUE, kernel = "linear")
x1 <- seq(4,8,0.05)
x2 <- seq(2,5,0.05)
mod$case.weights
plot(iris[,1], iris[,2], col = iris$Species, cex = mod$case.weights[[1]]*10)
plot(iris[,1], iris[,2], col = iris$Species, cex = mod$case.weights[[2]]*10)
plot(iris[,1], iris[,2], col = iris$Species, cex = mod$case.weights[[3]]*10)
plot(iris[,1], iris[,2], col = iris$Species, cex = mod$case.weights[[4]]*10)
legend("bottomright", legend = levels(iris$Species), col = as.numeric(unique(iris$Species)), lty = 1)

iris.grid <- expand.grid(Sepal.Length = x1, Sepal.Width = x2)
pred <- predict(mod, newdata = iris.grid, probability = TRUE)
prob.grid <- attr(pred, "probabilities")
contour(x1, x2, matrix(prob.grid[,1], length(x1)), add = TRUE, label = colnames(prob.grid)[1])
contour(x1, x2, matrix(prob.grid[,2], length(x1)), add = TRUE, label = colnames(prob.grid)[2])
contour(x1, x2, matrix(prob.grid[,3], length(x1)), add = TRUE, label = colnames(prob.grid)[3])


#=====================================================================================================================

mod1 <- wsvm(Species ~ ., data = iris, probability = TRUE)
predict(mod1, iris, probability = TRUE)

#?e1071:::predict.svm



mod2 <- dasvm(Species ~ ., data = iris, bw = 4)
predict(mod2)
predict(mod2, newdata = iris)
predict(mod2, newdata = iris, decision.values = TRUE)

mod2 <- dasvm(Species ~ ., data = iris, bw = 4, probability = TRUE)
predict(mod2)
predict(mod2, newdata = iris)
predict(mod2, newdata = iris, decision.values = TRUE)
predict(mod2, newdata = iris, probability = TRUE)




###################################################################################################

#======================	
test_that("dasvm: misspecified arguments", {
	data(iris)
	# wrong variable names
	expect_error(dasvm(formula = Species ~ V1, data = iris, wf = "gaussian", bw = 10))
	# wrong class
	expect_error(dasvm(formula = iris, data = iris, wf = "gaussian", bw = 10))
	expect_error(dasvm(iris, data = iris, wf = "gaussian", bw = 10))
	# target variable also in x
	expect_error(dasvm(y = iris$Species, x = iris, wf = "gaussian", bw = 10))      ## system singular
	expect_warning(dasvm(Species ~ Species + Petal.Width, data = iris, wf = "gaussian", bw = 10))           ## warning, Species on RHS removed
	# missing x
	expect_error(dasvm(y = iris$Species, wf = "gaussian", bw = 10))
	## itr
	expect_error(dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, itr = -5))
	expect_error(dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, itr = 0))
	## wrong method argument
	# missing quotes
	expect_error(dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, method = ML))
	# method as vector
	expect_error(dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, method = c("ML","unbiased")))
})


test_that("dasvm throws a warning if grouping variable is numeric", {
	data(iris)
	# formula, data
	expect_that(dasvm(formula = Sepal.Length ~ ., data = iris, wf = "gaussian", bw = 10), gives_warning("'y' was coerced to a factor"))
	expect_warning(dasvm(formula = Petal.Width ~ ., data = iris, wf = "gaussian", bw = 10))  ## system singular
	# y, x
	expect_that(dasvm(y = iris[,1], x = iris[,-1], wf = "gaussian", bw = 10), gives_warning("'y' was coerced to a factor"))
	expect_error(dasvm(y = iris[,4], x = iris[,-1], wf = "gaussian", bw = 10))     ## system singular
	expect_warning(dasvm(y = iris$Petal.Width, x = iris[,-5], wf = "gaussian", bw = 10))
})


test_that("dasvm works if only one predictor variable is given", {
	data(iris)
	fit <- dasvm(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 5)
#	expect_equal(ncol(fit$means), 1)	
#	expect_equal(dim(fit$cov), rep(1, 2))	
})


test_that("dasvm: detectig singular covariance matrix works", {
	data(iris)
	# one training observation
	expect_error(dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 1))            ## system singular	
	# one training observation in one predictor variable
	expect_error(dasvm(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 1, subset = 1))   ## system singular
})


#test_that("dasvm: initial weighting works correctly", {
#	data(iris)
#	## check if weighted solution with initial weights = 1 equals unweighted solution
#	fit1 <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 2)
#	fit2 <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 2, case.weights = rep(1,150))
#	expect_equal(fit1[-1],fit2[-1])
#	## returned weights	
#	expect_equal(fit1$case.weights[[1]], rep(1,150))
#	expect_equal(fit1$case.weights, fit2$case.weights)
#	## weights and subsetting
#	# formula, data
#	fit <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = 11:60)
#	expect_equal(fit$case.weights[[1]], rep(1,50))
#	# formula, data, weights
#	a <- rep(1:3,50)[11:60]
#	a <- a/sum(a) * length(a)
#	fit <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 2, case.weights = rep(1:3, 50), subset = 11:60)
#	expect_equal(fit$case.weights[[1]], a)
#	# x, y
#	fit <- dasvm(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 2, subset = 11:60)
#	expect_equal(fit$case.weights[[1]], rep(1,50))	
#	# x, y, weights
#	fit <- dasvm(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 2, case.weights = rep(1:3, 50), subset = 11:60)
#	expect_equal(fit$case.weights[[1]], a)
#	## wrong specification of weights argument
#	# weights in a matrix
#	weight <- matrix(seq(1:150), nrow = 50)
#	expect_error(dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 2, case.weights = weight))
#	# weights < 0
#	expect_error(dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 2, case.weights = rep(-5, 150)))
#	# weights true/false
#	expect_error(dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 2, case.weights = TRUE))
#})


#test_that("dasvm breaks out of for-loop if only one class is left", {
#	expect_that(fit <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 1:50), gives_warning(c("groups versicolor, virginica are empty or weights in these groups are all zero", "training data from only one group, breaking out of iterative procedure")))
#	expect_equal(fit$itr, 1)
#	expect_equal(length(fit$case.weights), 1)
#})


test_that("dasvm: subsetting works", {
	data(iris)
	# formula, data
	fit1 <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = 1:80)
	fit2 <- dasvm(Species ~ ., data = iris[1:80,], wf = "gaussian", bw = 2)
	expect_equal(fit1[-1],fit2[-1])
	expect_equal(fit1$case.weights[[1]], rep(1,80))
	# formula, data, weights
#	fit1 <- dasvm(Species ~ ., data = iris, case.weights = rep(1:3, each = 50), wf = "gaussian", bw = 2, subset = 1:80)
#	fit2 <- dasvm(Species ~ ., data = iris[1:80,], case.weights = rep(1:3, each = 50)[1:80], wf = "gaussian", bw = 2)
#	expect_equal(fit1[-1],fit2[-1])
#	a <- rep(80, 4)
#	names(a) <- 0:3
#	expect_equal(sapply(fit1$case.weights, length), a)
#	b <- rep(1:3, each = 50)[1:80]
#	b <- b/sum(b) * length(b)
#	expect_equal(fit1$case.weights[[1]], b)
	# x, y
	fit1 <- dasvm(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 2, subset = 1:80)
	fit2 <- dasvm(y = iris$Species[1:80], x = iris[1:80,-5], wf = "gaussian", bw = 2)
	expect_equal(fit1[-1],fit2[-1])
	expect_equal(fit1$case.weights[[1]], rep(1,80))
	# x, y, weights
	fit1 <- dasvm(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 2, case.weights = rep(1:3, each = 50), subset = 1:80)
	fit2 <- dasvm(y = iris$Species[1:80], x = iris[1:80,-5], wf = "gaussian", bw = 2, case.weights = rep(1:3, each = 50)[1:80])
	expect_equal(fit1[-1],fit2[-1])
	a <- rep(80, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$case.weights, length), a)
	b <- rep(1:3, each = 50)[1:80]
	b <- b/sum(b) * length(b)
	expect_equal(fit1$case.weights[[1]], b)
	# wrong specification of subset argument
	expect_error(dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = iris[1:10,]))
	expect_error(dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = FALSE))
	expect_error(dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 0))
	expect_error(dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = -10:50))
})


test_that("dasvm: NA handling works correctly", {
	### NA in x
	data(iris)
	irisna <- iris
	irisna[1:10, c(1,3)] <- NA
	## formula, data
	# na.fail
	expect_error(dasvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dasvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit)
	fit2 <- dasvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60)
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$case.weights, length), a)
#	## formula, data, weights
#	# na.fail
#	expect_error(dasvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, case.weights = rep(1:3, 50), na.action = na.fail))
#	# check if na.omit works correctly
#	fit1 <- dasvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, case.weights = rep(1:3, 50), na.action = na.omit)
#	fit2 <- dasvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60, case.weights = rep(1:3, 50))
#	expect_equal(fit1[-c(8, 17)], fit2[-8])
#	a <- rep(50, 4)
#	names(a) <- 0:3
#	expect_equal(sapply(fit1$case.weights, length), a)

	## x, y
	# na.fail
	expect_error(dasvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dasvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit)
	fit2 <- dasvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60)
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$case.weights, length), a)
#	## x, y, weights
#	# na.fail
#	expect_error(dasvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, case.weights = rep(1:3, 50), na.action = na.fail))
#	# check if na.omit works correctly
#	fit1 <- dasvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, case.weights = rep(1:3, 50), na.action = na.omit)
#	fit2 <- dasvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60, case.weights = rep(1:3, 50))
#	expect_equal(fit1[-8], fit2[-8])
#	a <- rep(50, 4)
#	names(a) <- 0:3
#	expect_equal(sapply(fit1$case.weights, length), a)
	
	### NA in y
	irisna <- iris
	irisna$Species[1:10] <- NA
	## formula, data
	# na.fail
	expect_error(dasvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dasvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit)
	fit2 <- dasvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60)
	expect_equal(fit1[-c(1, 28)], fit2[-c(1,28)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$case.weights, length), a)
#	## formula, data, weights
#	# na.fail
#	expect_error(dasvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, case.weights = rep(1:3, 50), na.action = na.fail))
#	# check if na.omit works correctly
#	fit1 <- dasvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, case.weights = rep(1:3, 50), na.action = na.omit)
#	fit2 <- dasvm(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60, case.weights = rep(1:3, 50))
#	expect_equal(fit1[-c(8, 17)], fit2[-8])
#	a <- rep(50, 4)
#	names(a) <- 0:3
#	expect_equal(sapply(fit1$case.weights, length), a)

	## x, y
	# na.fail
	expect_error(dasvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dasvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit)
	fit2 <- dasvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60)
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$case.weights, length), a)

#	## x, grouping, weights
#	# na.fail
#	expect_error(dasvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, case.weights = rep(1:3, 50), na.action = na.fail))
#	# check if na.omit works correctly
#	fit1 <- dasvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, case.weights = rep(1:3, 50), na.action = na.omit)
#	fit2 <- dasvm(y = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60, case.weights = rep(1:3, 50))
#	expect_equal(fit1[-8], fit2[-8])
#	a <- rep(50, 4)
#	names(a) <- 0:3
#	expect_equal(sapply(fit1$case.weights, length), a)

	### NA in weights
#	weights <- rep(1:3,50)
#	weights[1:10] <- NA
#	## formula, data, weights
#	# na.fail
#	expect_error(dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 6:60, case.weights = weights, na.action = na.fail))
#	# check if na.omit works correctly
#	fit1 <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 6:60, case.weights = weights, na.action = na.omit)
#	fit2 <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 11:60, case.weights = weights)
#	expect_equal(fit1[-c(8, 17)], fit2[-8])
#	a <- rep(50, 4)
#	names(a) <- 0:3
#	expect_equal(sapply(fit1$case.weights, length), a)
#	## x, y, weights
#	# na.fail
#	expect_error(dasvm(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 6:60, case.weights = weights, na.action = na.fail))
#	# check if na.omit works correctly
#	fit1 <- dasvm(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 6:60, case.weights = weights, na.action = na.omit)
#	fit2 <- dasvm(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 11:60, case.weights = weights)
#	expect_equal(fit1[-8], fit2[-8])
#	a <- rep(50, 4)
#	names(a) <- 0:3
#	expect_equal(sapply(fit1$case.weights, length), a)

	### NA in subset
	subset <- 6:60
	subset[1:5] <- NA
	## formula, data
	# na.fail
	expect_error(dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, na.action = na.omit)
	fit2 <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 11:60)
	expect_equal(fit1[-c(1, 28)], fit2[-c(1,28)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$case.weights, length), a)
#	## formula, data, weights
#	# na.fail
#	expect_error(dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, case.weights = rep(1:3, 50), na.action = na.fail))
#	# check if na.omit works correctly
#	fit1 <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, case.weights = rep(1:3, 50), na.action = na.omit)
#	fit2 <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 11:60, case.weights = rep(1:3, 50))
#	expect_equal(fit1[-c(8, 17)], fit2[-8])
#	a <- rep(50, 4)
#	names(a) <- 0:3
#	expect_equal(sapply(fit1$case.weights, length), a)

	## x, y
	# na.fail
	expect_error(dasvm(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- dasvm(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, na.action = na.omit)
	fit2 <- dasvm(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 11:60)
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$case.weights, length), a)

#	## x, y, weights
#	# na.fail
#	expect_error(dasvm(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, case.weights = rep(1:3, 50), na.action = na.fail))
#	# check if na.omit works correctly
#	fit1 <- dasvm(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, case.weights = rep(1:3, 50), na.action = na.omit)
#	fit2 <- dasvm(y = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 11:60, case.weights = rep(1:3, 50))
#	expect_equal(fit1[-8], fit2[-8])
#	a <- rep(50, 4)
#	names(a) <- 0:3
#	expect_equal(sapply(fit1$case.weights, length), a)
})


test_that("dasvm: try all weight functions", {
	fit1 <- dasvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5)    
	fit2 <- dasvm(formula = Species ~ ., data = iris, wf = gaussian(0.5))    
	fit3 <- dasvm(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 0.5)    
	fit4 <- dasvm(x = iris[,-5], y = iris$Species, wf = gaussian(0.5))    
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit3[-c(1,28)], fit4[-c(1,28)])
#	expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	
	fit1 <- dasvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5, k = 30)    
	fit2 <- dasvm(formula = Species ~ ., data = iris, wf = gaussian(bw = 0.5, k = 30))    
	fit3 <- dasvm(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 0.5, k = 30)    
	fit4 <- dasvm(x = iris[,-5], y = iris$Species, wf = gaussian(0.5, 30))
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit3[-c(1,28)], fit4[-c(1,28)])
#	expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$case.weights[2:4], function(x) sum(x > 0)), a)
	
	fit1 <- dasvm(formula = Species ~ ., data = iris, wf = "epanechnikov", bw = 5, k = 50)
	fit2 <- dasvm(formula = Species ~ ., data = iris, wf = epanechnikov(bw = 5, k = 50))
	fit3 <- dasvm(x = iris[,-5], y = iris$Species, wf = "epanechnikov", bw = 5, k = 50)
	fit4 <- dasvm(x = iris[,-5], y = iris$Species, wf = epanechnikov(5, 50))    
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit3[-c(1,28)], fit4[-c(1,28)])
#	expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$case.weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- dasvm(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50)
	fit2 <- dasvm(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 50))
	fit3 <- dasvm(x = iris[,-5], y = iris$Species, wf = "rectangular", bw = 5, k = 50)
	fit4 <- dasvm(x = iris[,-5], y = iris$Species, wf = rectangular(5, 50))    
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit3[-c(1,28)], fit4[-c(1,28)])
#	expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$case.weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- dasvm(formula = Species ~ ., data = iris, wf = "triangular", bw = 5, k = 50)
	fit2 <- dasvm(formula = Species ~ ., data = iris, wf = triangular(5, k = 50))
	fit3 <- dasvm(x = iris[,-5], y = iris$Species, wf = "triangular", bw = 5, k = 50)
	fit4 <- dasvm(x = iris[,-5], y = iris$Species, wf = triangular(5, 50))    
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit3[-c(1,28)], fit4[-c(1,28)])
#	expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$case.weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- dasvm(formula = Species ~ ., data = iris, wf = "biweight", bw = 5, k = 50)
	fit2 <- dasvm(formula = Species ~ ., data = iris, wf = biweight(5, k = 50))
	fit3 <- dasvm(x = iris[,-5], y = iris$Species, wf = "biweight", bw = 5, k = 50)
	fit4 <- dasvm(x = iris[,-5], y = iris$Species, wf = biweight(5, 50))    
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit3[-c(1,28)], fit4[-c(1,28)])
#	expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$case.weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- dasvm(formula = Species ~ ., data = iris, wf = "optcosine", bw = 5, k = 50)
	fit2 <- dasvm(formula = Species ~ ., data = iris, wf = optcosine(5, k = 50))
	fit3 <- dasvm(x = iris[,-5], y = iris$Species, wf = "optcosine", bw = 5, k = 50)
	fit4 <- dasvm(x = iris[,-5], y = iris$Species, wf = optcosine(5, 50))    
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit3[-c(1,28)], fit4[-c(1,28)])
#	expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$case.weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- dasvm(formula = Species ~ ., data = iris, wf = "cosine", bw = 5, k = 50)
	fit2 <- dasvm(formula = Species ~ ., data = iris, wf = cosine(5, k = 50))
	fit3 <- dasvm(x = iris[,-5], y = iris$Species, wf = "cosine", bw = 5, k = 50)
	fit4 <- dasvm(x = iris[,-5], y = iris$Species, wf = cosine(5, 50))    
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit3[-c(1,28)], fit4[-c(1,28)])
#	expect_equal(fit2[c(1:7,9:14)], fit4[c(1:7,9:14)])
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$case.weights[2:4], function(x) sum(x > 0)), a)
})


test_that("dasvm: arguments related to weighting misspecified", {
	# bw, k not required
	expect_that(fit1 <- dasvm(Species ~ ., data = iris, wf = gaussian(0.5), k = 30, bw = 0.5), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	fit2 <- dasvm(Species ~ ., data = iris, wf = gaussian(0.5))
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])

	expect_that(fit1 <- dasvm(Species ~ ., data = iris, wf = gaussian(0.5), bw = 0.5), gives_warning("argument 'bw' is ignored"))	
	fit2 <- dasvm(Species ~ ., data = iris, wf = gaussian(0.5))
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, 0.5)	
	expect_equal(fit1$adaptive, FALSE)	

	expect_that(fit1 <- dasvm(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5, k = 30), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	expect_that(fit2 <- dasvm(Species ~ ., data = iris, wf = function(x) exp(-x), k = 30), gives_warning("argument 'k' is ignored"))
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	expect_that(fit1 <- dasvm(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5), gives_warning("argument 'bw' is ignored"))
	fit2 <- dasvm(Species ~ ., data = iris, wf = function(x) exp(-x))
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	# missing quotes
	expect_error(dasvm(formula = Species ~ ., data = iris, wf = gaussian)) ## error because length(weights) and nrow(x) are different

	# bw, k missing
	expect_that(dasvm(formula = Species ~ ., data = iris, wf = gaussian()), throws_error("either 'bw' or 'k' have to be specified"))
	expect_that(dasvm(formula = Species ~ ., data = iris, wf = gaussian(), k = 10), throws_error("either 'bw' or 'k' have to be specified"))
	expect_error(dasvm(Species ~ ., data = iris))
	
	# bw < 0
	expect_error(dasvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = -5))
	expect_error(dasvm(formula = Species ~ ., data = iris, wf = "cosine", k = 10, bw = -50))
	
	# bw vector
	expect_that(dasvm(formula = Species ~., data = iris, wf = "gaussian", bw = rep(1, nrow(iris))), gives_warning("only first element of 'bw' used"))
	
	# k < 0
	expect_error(dasvm(formula = Species ~ ., data = iris, wf = "gaussian", k =-7, bw = 50))

	# k too small
	expect_error(dasvm(formula = Species ~ ., data = iris, wf = "gaussian", k = 5, bw = 0.005))

	# k too large
	expect_error(dasvm(formula = Species ~ ., data = iris, k = 250, wf = "gaussian", bw = 50))

	# k vector
	expect_that(dasvm(formula = Species ~., data = iris, wf = "gaussian", k = rep(50, nrow(iris))), gives_warning("only first element of 'k' used"))
})


test_that("dasvm: weighting schemes work", {
	## wf with finite support
	# fixed bw
	fit1 <- dasvm(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5)
	fit2 <- dasvm(formula = Species ~ ., data = iris, wf = rectangular(bw = 5))
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)

	# adaptive bw, only knn 
	fit1 <- dasvm(formula = Species ~ ., data = iris, wf = "rectangular", k = 50)
	fit2 <- dasvm(formula = Species ~ ., data = iris, wf = rectangular(k = 50))
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	is.null(fit1$bw)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$bw, NULL)
	expect_true(fit1$nn.only)
	expect_true(fit1$adaptive)
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$case.weights[2:4], function(x) sum(x > 0)), a)

	# fixed bw, only knn
	fit1 <- dasvm(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50)
	fit2 <- dasvm(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 50))
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, 50)
	expect_true(fit1$nn.only)
	expect_true(!fit1$adaptive)
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$case.weights[2:4], function(x) sum(x > 0)), a)
	
	# nn.only not needed
	expect_that(dasvm(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, nn.only = TRUE), gives_warning("argument 'nn.only' is ignored"))

	# nn.only has to be TRUE if bw and k are both given
	expect_error(dasvm(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50, nn.only = FALSE))
	
	## wf with infinite support
	# fixed bw
	fit1 <- dasvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5)
	fit2 <- dasvm(formula = Species ~ ., data = iris, wf = gaussian(bw = 0.5))
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit1$bw, 0.5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)
	a <- rep(150, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$case.weights, function(x) sum(x > 0)), a)

	# adaptive bw, only knn
	fit1 <- dasvm(formula = Species ~ ., data = iris, wf = "gaussian", k = 50)
	fit2 <- dasvm(formula = Species ~ ., data = iris, wf = gaussian(k = 50))
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(fit1$adaptive)
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$case.weights[2:4], function(x) sum(x > 0)), a)

	# adaptive bw, all obs
	fit1 <- dasvm(formula = Species ~ ., data = iris, wf = "gaussian", k = 50, nn.only = FALSE)
	fit2 <- dasvm(formula = Species ~ ., data = iris, wf = gaussian(k = 50, nn.only = FALSE))
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, FALSE)
	expect_true(fit1$adaptive)
	a <- rep(150, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$case.weights, function(x) sum(x > 0)), a)

	# fixed bw, only knn
	fit1 <- dasvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50)
	fit2 <- dasvm(formula = Species ~ ., data = iris, wf = gaussian(bw = 1, k = 50))
	expect_equal(fit1[-c(1,28)], fit2[-c(1,28)])
	expect_equal(fit1$bw, 1)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(!fit1$adaptive)
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$case.weights[2:4], function(x) sum(x > 0)), a)
	
	# nn.only has to be TRUE if bw and k are both given
	expect_error(dasvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50, nn.only = FALSE))
})	


#=================================================================================================================
test_that("predict.dasvm works correctly with formula and data.frame interface and with missing newdata", {
	data(iris)
	ran <- sample(1:150,100)
	## formula, data
	fit <- dasvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran)
  	pred <- predict(fit)
#  	expect_equal(rownames(pred$posterior), rownames(iris)[ran])  	
	## formula, data, newdata
	fit <- dasvm(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran)  
  	predict(fit, newdata = iris[-ran,])
	## y, x
	fit <- dasvm(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 2, subset = ran)  
  	pred <- predict(fit)
#  	expect_equal(rownames(pred$posterior), rownames(iris)[ran])  	
	## y, x, newdata
	fit <- dasvm(x = iris[,-5], y = iris$Species, wf = "gaussian", bw = 2, subset = ran)  
  	predict(fit, newdata = iris[-ran,-5])
})


test_that("predict.dasvm works with missing classes in the training data", {
	data(iris)
	ran <- sample(1:150,100)
	expect_that(fit <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 1:100), gives_warning("group virginica is empty or weights in this group are all zero"))

})


test_that("predict.dasvm works with one single predictor variable", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- dasvm(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran)
#	expect_equal(ncol(fit$means), 1)
#	expect_equal(dim(fit$cov), rep(1, 2))
	predict(fit, newdata = iris[-ran,])
})


test_that("predict.dasvm works with one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran)
  	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred), 1)
#	expect_equal(dim(pred$posterior), c(1, 3))
	a <- factor("setosa", levels = c("setosa", "versicolor", "virginica"))
	names(a) = "5"
	expect_equal(pred, a)
	pred <- predict(fit, newdata = iris[58,])
	expect_equal(length(pred), 1)
#	expect_equal(dim(pred$posterior), c(1, 3))
	a <- factor("versicolor", levels = c("setosa", "versicolor", "virginica"))
	names(a) = "58"
	expect_equal(pred, a)
})	


test_that("predict.dasvm works with one single predictor variable and one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- dasvm(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran)
#	expect_equal(ncol(fit$means), 1)
#	expect_equal(dim(fit$cov), rep(1, 2))
	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred), 1)
#	expect_equal(dim(pred$posterior), c(1, 3))
})

test_that("predict.dasvm: NA handling in newdata works", {
	data(iris)
	ran <- sample(1:150,100)
	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	fit <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 50, subset = ran)
	pred <- predict(fit, newdata = irisna) ## default: na.omit
	expect_equal(length(pred), 133)
	expect_error(pred <- predict(fit, newdata = irisna, na.action = na.fail))
})

test_that("predict.dasvm: misspecified arguments", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- dasvm(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran)
    # errors in newdata
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
    # errors in prior
    expect_error(predict(fit, prior = rep(2,length(levels(iris$Species))), newdata = iris[-ran,]))
    expect_error(predict(fit, prior = TRUE, newdata = iris[-ran,]))
    expect_error(predict(fit, prior = 0.6, newdata = iris[-ran,]))
})  	
