context("dalda")

test_that("dalda: misspecified arguments", {
	data(iris)
	# wrong variable names
	expect_error(dalda(formula = Species ~ V1, data = iris, wf = "gaussian", bw = 10))
	# wrong class
	expect_error(dalda(formula = iris, data = iris, wf = "gaussian", bw = 10))
	#expect_error(dalda(iris, data = iris, wf = "gaussian", bw = 10))
	# target variable also in x
	expect_error(dalda(grouping = iris$Species, x = iris, wf = "gaussian", bw = 10))      					## system singular
	expect_warning(dalda(Species ~ Species + Petal.Width, data = iris, wf = "gaussian", bw = 10))           ## warning, Species on RHS removed
	# missing x
	expect_error(dalda(grouping = iris$Species, wf = "gaussian", bw = 10))
	## itr
	expect_that(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, itr = -5), throws_error("'itr' must be >= 1"))
	expect_that(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, itr = 0), throws_error("'itr' must be >= 1"))
	## wrong method argument
	# missing quotes
	expect_error(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, method = ML))
	# method as vector
	expect_that(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, method = c("ML","unbiased")), throws_error("'arg' must be of length 1"))
})


test_that("dalda throws a warning if grouping variable is numeric", {
	data(iris)
	# formula, data
	expect_that(dalda(formula = Sepal.Length ~ ., data = iris, wf = "gaussian", bw = 10), gives_warning("'grouping' was coerced to a factor"))
	expect_error(dalda(formula = Petal.Width ~ ., data = iris, wf = "gaussian", bw = 10))	## system singular
	# grouping, x
	expect_that(dalda(grouping = iris[,1], x = iris[,-1], wf = "gaussian", bw = 10), gives_warning("'grouping' was coerced to a factor"))
	expect_error(dalda(grouping = iris[,4], x = iris[,-1], wf = "gaussian", bw = 10))     	## system singular
})


test_that("dalda works if only one predictor variable is given", {
	data(iris)
	fit <- dalda(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 5)
	expect_equal(ncol(fit$means), 1)	
	expect_equal(dim(fit$cov), rep(1, 2))	
})


test_that("dalda: training data from only one class", {
	data(iris)
	expect_that(dalda(Species ~ ., data = iris, bw = 2, subset = 1:50), throws_error("training data from only one group given"))
	expect_that(dalda(grouping = iris$Species, x = iris[,-5], bw = 2, subset = 1:50), throws_error("training data from only one group given"))
})


test_that("dalda: one training observation", {
	data(iris)
	# one training observation
	expect_that(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 1), throws_error("training data from only one group given"))	
	# one training observation in one predictor variable
	expect_that(dalda(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 1, subset = 1), throws_error("training data from only one group given"))
})


test_that("dalda: initial weighting works correctly", {
	data(iris)
	## check if weighted solution with initial weights = 1 equals unweighted solution
	fit1 <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 2)
	fit2 <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 2, weights = rep(1,150))
	expect_equal(fit1[-9],fit2[-9])
	## returned weights	
	a <- rep(1,150)
	names(a) <- 1:150
	expect_equal(fit1$weights[[1]], a)
	expect_equal(fit1$weights, fit2$weights)
	## weights and subsetting
	# formula, data
	expect_that(fit <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = 11:60), gives_warning("group virginica is empty or weights in this group are all zero"))
	a <- rep(1,50)
	names(a) <- 11:60
	expect_equal(fit$weights[[1]], a)
	# formula, data, weights
	a <- rep(1:3,50)[11:60]
	a <- a/sum(a) * length(a)
	names(a) <- 11:60
	expect_that(fit <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 2, weights = rep(1:3, 50), subset = 11:60), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit$weights[[1]], a)
	# x, grouping
	a <- rep(1,50)
	names(a) <- 11:60
	expect_that(fit <- dalda(x = iris[,-5], grouping = iris$Species, wf = "gaussian", bw = 2, subset = 11:60), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit$weights[[1]], a)	
	# x, grouping, weights
	a <- rep(1:3,50)[11:60]
	a <- a/sum(a) * length(a)
	names(a) <- 11:60
	expect_that(fit <- dalda(x = iris[,-5], grouping = iris$Species, wf = "gaussian", bw = 2, weights = rep(1:3, 50), subset = 11:60), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit$weights[[1]], a)
	## wrong specification of weights argument
	# weights in a matrix
	weight <- matrix(seq(1:150), nrow = 50)
	expect_error(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 2, weights = weight))
	# weights < 0
	expect_error(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 2, weights = rep(-5, 150)))
	# weights true/false
	expect_error(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 2, weights = TRUE))
})


test_that("dalda breaks out of for-loop if only one class is left", {
	expect_that(fit <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, k = 50), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_equal(fit$itr, 3)
	expect_equal(length(fit$weights), 4)
	expect_that(fit <- dalda(Species ~ ., data = iris, wf = "gaussian", k = 10, subset = 1:100), gives_warning(c("groups versicolor, virginica are empty or weights in these groups are all zero", "training data from only one group, breaking out of iterative procedure")))
	expect_equal(fit$itr, 1)
	expect_equal(length(fit$weights), 2)
})
#sapply(fit$weights, function(x) return(list(sum(x[1:50]), sum(x[51:100]), sum(x[101:150]))))

test_that("dalda: subsetting works", {
	data(iris)
	# formula, data
	expect_that(fit1 <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = 1:80), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(Species ~ ., data = iris[1:80,], wf = "gaussian", bw = 2), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-9],fit2[-9])
	a <- rep(1,80)
	names(a) <- 1:80
	expect_equal(fit1$weights[[1]], a)
	# formula, data, weights
	expect_that(fit1 <- dalda(Species ~ ., data = iris, weights = rep(1:3, each = 50), wf = "gaussian", bw = 2, subset = 1:80), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(Species ~ ., data = iris[1:80,], weights = rep(1:3, each = 50)[1:80], wf = "gaussian", bw = 2), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-9],fit2[-9])
	a <- rep(80, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	b <- rep(1:3, each = 50)[1:80]
	b <- b/sum(b) * length(b)
	names(b) <- 1:80
	expect_equal(fit1$weights[[1]], b)
	# x, grouping
	expect_that(fit1 <- dalda(grouping = iris$Species, x = iris[,-5], wf = "gaussian", bw = 2, subset = 1:80), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(grouping = iris$Species[1:80], x = iris[1:80,-5], wf = "gaussian", bw = 2), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-9],fit2[-9])
	a <- rep(1,80)
	names(a) <- 1:80
	expect_equal(fit1$weights[[1]], a)
	# x, grouping, weights
	expect_that(fit1 <- dalda(grouping = iris$Species, x = iris[,-5], wf = "gaussian", bw = 2, weights = rep(1:3, each = 50), subset = 1:80), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(grouping = iris$Species[1:80], x = iris[1:80,-5], wf = "gaussian", bw = 2, weights = rep(1:3, each = 50)[1:80]), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-9],fit2[-9])
	a <- rep(80, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	b <- rep(1:3, each = 50)[1:80]
	b <- b/sum(b) * length(b)
	names(b) <- 1:80
	expect_equal(fit1$weights[[1]], b)
	# wrong specification of subset argument
	expect_error(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = iris[1:10,]))
	expect_error(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = FALSE))
	expect_error(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 0))
	expect_error(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = -10:50))
})


test_that("dalda: NA handling works correctly", {
	### NA in x
	data(iris)
	irisna <- iris
	irisna[1:10, c(1,3)] <- NA
	## formula, data
	# na.fail
	expect_that(dalda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- dalda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-c(9, 18)], fit2[-9])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## formula, data, weights
	# na.fail
	expect_that(dalda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:3, 50), na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- dalda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:3, 50), na.action = na.omit), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:3, 50)), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-c(9, 18)], fit2[-9])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)

	## x, grouping
	# na.fail
	expect_that(dalda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- dalda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-9], fit2[-9])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## x, grouping, weights
	# na.fail
	expect_that(dalda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:3, 50), na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- dalda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:3, 50), na.action = na.omit), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:3, 50)), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-9], fit2[-9])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	
	### NA in grouping
	irisna <- iris
	irisna$Species[1:10] <- NA
	## formula, data
	# na.fail
	expect_that(dalda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- dalda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-c(9, 18)], fit2[-9])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## formula, data, weights
	# na.fail
	expect_that(dalda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:3, 50), na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- dalda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:3, 50), na.action = na.omit), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:3, 50)), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-c(9, 18)], fit2[-9])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## x, grouping
	# na.fail
	expect_that(dalda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- dalda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, na.action = na.omit), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-9], fit2[-9])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## x, grouping, weights
	# na.fail
	expect_that(dalda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:3, 50), na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- dalda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 6:60, weights = rep(1:3, 50), na.action = na.omit), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(grouping = irisna$Species, x = irisna[,-5], wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:3, 50)), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-9], fit2[-9])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)

	### NA in weights
	weights <- rep(1:3,50)
	weights[1:10] <- NA
	## formula, data, weights
	# na.fail
	expect_that(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 6:60, weights = weights, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 6:60, weights = weights, na.action = na.omit), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 11:60, weights = weights), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-c(9, 18)], fit2[-9])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## x, grouping, weights
	# na.fail
	expect_that(dalda(grouping = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 6:60, weights = weights, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- dalda(grouping = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 6:60, weights = weights, na.action = na.omit), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(grouping = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 11:60, weights = weights), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-9], fit2[-9])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)

	### NA in subset
	subset <- 6:60
	subset[1:5] <- NA
	## formula, data
	# na.fail
	expect_that(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, na.action = na.omit), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-c(9, 18)], fit2[-9])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## formula, data, weights
	# na.fail
	expect_that(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, weights = rep(1:3, 50), na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = subset, weights = rep(1:3, 50), na.action = na.omit), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:3, 50)), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-c(9, 18)], fit2[-9])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## x, grouping
	# na.fail
	expect_that(dalda(grouping = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- dalda(grouping = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, na.action = na.omit), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(grouping = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 11:60), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-9], fit2[-9])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
	## x, grouping, weights
	# na.fail
	expect_that(dalda(grouping = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, weights = rep(1:3, 50), na.action = na.fail), throws_error("missing values in object"))
	# check if na.omit works correctly
	expect_that(fit1 <- dalda(grouping = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = subset, weights = rep(1:3, 50), na.action = na.omit), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(grouping = iris$Species, x = iris[,-5], wf = "gaussian", bw = 10, subset = 11:60, weights = rep(1:3, 50)), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(fit1[-9], fit2[-9])
	a <- rep(50, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, length), a)
})


test_that("dalda: try all weight functions", {
	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5)    
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = gaussian(0.5))    
	fit3 <- dalda(x = iris[,-5], grouping = iris$Species, wf = "gaussian", bw = 0.5)    
	fit4 <- dalda(x = iris[,-5], grouping = iris$Species, wf = gaussian(0.5))    
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit3[-9], fit4[-9])
	expect_equal(fit2[c(1:8,10:15)], fit4[c(1:8,10:15)])
	
	expect_that(fit1 <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5, k = 30), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(formula = Species ~ ., data = iris, wf = gaussian(bw = 0.5, k = 30)), gives_warning("group setosa is empty or weights in this group are all zero"))   
	expect_that(fit3 <- dalda(x = iris[,-5], grouping = iris$Species, wf = "gaussian", bw = 0.5, k = 30), gives_warning("group setosa is empty or weights in this group are all zero"))    
	expect_that(fit4 <- dalda(x = iris[,-5], grouping = iris$Species, wf = gaussian(0.5, 30)), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit3[-9], fit4[-9])
	expect_equal(fit2[c(1:8,10:15)], fit4[c(1:8,10:15)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)
	
	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "epanechnikov", bw = 5)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = epanechnikov(bw = 5))
	fit3 <- dalda(x = iris[,-5], grouping = iris$Species, wf = "epanechnikov", bw = 5)
	fit4 <- dalda(x = iris[,-5], grouping = iris$Species, wf = epanechnikov(5))    
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit3[-9], fit4[-9])
	expect_equal(fit2[c(1:8,10:15)], fit4[c(1:8,10:15)])
	a <- rep(150, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	expect_that(fit1 <- dalda(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 30), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 30)), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_that(fit3 <- dalda(x = iris[,-5], grouping = iris$Species, wf = "rectangular", bw = 5, k = 30), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_that(fit4 <- dalda(x = iris[,-5], grouping = iris$Species, wf = rectangular(5, 30)), gives_warning("group setosa is empty or weights in this group are all zero"))    
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit3[-9], fit4[-9])
	expect_equal(fit2[c(1:8,10:15)], fit4[c(1:8,10:15)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "triangular", bw = 5)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = triangular(5))
	fit3 <- dalda(x = iris[,-5], grouping = iris$Species, wf = "triangular", bw = 5)
	fit4 <- dalda(x = iris[,-5], grouping = iris$Species, wf = triangular(5))    
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit3[-9], fit4[-9])
	expect_equal(fit2[c(1:8,10:15)], fit4[c(1:8,10:15)])
	a <- rep(150, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	expect_that(fit1 <- dalda(formula = Species ~ ., data = iris, wf = "biweight", bw = 5, k = 30), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(formula = Species ~ ., data = iris, wf = biweight(5, k = 30)), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_that(fit3 <- dalda(x = iris[,-5], grouping = iris$Species, wf = "biweight", bw = 5, k = 30), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_that(fit4 <- dalda(x = iris[,-5], grouping = iris$Species, wf = biweight(5, 30)), gives_warning("group setosa is empty or weights in this group are all zero"))    
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit3[-9], fit4[-9])
	expect_equal(fit2[c(1:8,10:15)], fit4[c(1:8,10:15)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "optcosine", bw = 5)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = optcosine(5))
	fit3 <- dalda(x = iris[,-5], grouping = iris$Species, wf = "optcosine", bw = 5)
	fit4 <- dalda(x = iris[,-5], grouping = iris$Species, wf = optcosine(5))    
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit3[-9], fit4[-9])
	expect_equal(fit2[c(1:8,10:15)], fit4[c(1:8,10:15)])
	a <- rep(150, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	expect_that(fit1 <- dalda(formula = Species ~ ., data = iris, wf = "cosine", bw = 5, k = 30), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(formula = Species ~ ., data = iris, wf = cosine(5, k = 30)), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_that(fit3 <- dalda(x = iris[,-5], grouping = iris$Species, wf = "cosine", bw = 5, k = 30), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_that(fit4 <- dalda(x = iris[,-5], grouping = iris$Species, wf = cosine(5, 30)), gives_warning("group setosa is empty or weights in this group are all zero"))   
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit3[-9], fit4[-9])
	expect_equal(fit2[c(1:8,10:15)], fit4[c(1:8,10:15)])
	a <- rep(30, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)
})


test_that("dalda: local solution with rectangular window function and large bw and global solution coincide", {
	fit1 <- wlda(formula = Species ~ ., data = iris)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = rectangular(20))
	expect_equal(fit1[-c(7,9)], fit2[-c(7,9:15)])
	expect_equal(fit1$weights, fit2$weights[[1]])
})


test_that("dalda: arguments related to weighting misspecified", {
	# bw, k not required
	expect_that(fit1 <- dalda(Species ~ ., data = iris, wf = gaussian(0.5), k = 30, bw = 0.5), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	fit2 <- dalda(Species ~ ., data = iris, wf = gaussian(0.5))
	expect_equal(fit1[-9], fit2[-9])

	expect_that(fit1 <- dalda(Species ~ ., data = iris, wf = gaussian(0.5), bw = 0.5), gives_warning("argument 'bw' is ignored"))	
	fit2 <- dalda(Species ~ ., data = iris, wf = gaussian(0.5))
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, 0.5)	
	expect_equal(fit1$adaptive, FALSE)	

	expect_that(fit1 <- dalda(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5, k = 30), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	expect_that(fit2 <- dalda(Species ~ ., data = iris, wf = function(x) exp(-x), k = 30), gives_warning("argument 'k' is ignored"))
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	expect_that(fit1 <- dalda(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5), gives_warning("argument 'bw' is ignored"))
	fit2 <- dalda(Species ~ ., data = iris, wf = function(x) exp(-x))
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, NULL)	

	# missing quotes
	expect_error(dalda(formula = Species ~ ., data = iris, wf = gaussian)) ## error because length(weights) and nrow(x) are different

	# bw, k missing
	expect_that(dalda(formula = Species ~ ., data = iris, wf = gaussian()), throws_error("either 'bw' or 'k' have to be specified"))
	expect_that(dalda(formula = Species ~ ., data = iris, wf = gaussian(), k = 10), throws_error("either 'bw' or 'k' have to be specified"))
	expect_that(dalda(Species ~ ., data = iris), throws_error("either 'bw' or 'k' have to be specified"))
	
	# bw < 0
	expect_that(dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = -5), throws_error("'bw' must be positive"))
	expect_that(dalda(formula = Species ~ ., data = iris, wf = "cosine", k = 10, bw = -50), throws_error("'bw' must be positive"))
	
	# bw vector
	expect_that(dalda(formula = Species ~., data = iris, wf = "gaussian", bw = rep(1, nrow(iris))), gives_warning("only first element of 'bw' used"))
	
	# k < 0
	expect_that(dalda(formula = Species ~ ., data = iris, wf = "gaussian", k =-7, bw = 50), throws_error("'k' must be positive"))

	# k too small
	#expect_error(dalda(formula = Species ~ ., data = iris, wf = "gaussian", k = 5, bw = 0.005))

	# k too large
	expect_that(dalda(formula = Species ~ ., data = iris, k = 250, wf = "gaussian", bw = 50), throws_error("'k' is larger than 'n'"))

	# k vector
	expect_that(dalda(formula = Species ~., data = iris, wf = "gaussian", k = rep(50, nrow(iris))), gives_warning("only first element of 'k' used"))
})


test_that("dalda: weighting schemes work", {
	## wf with finite support
	# fixed bw
	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = rectangular(bw = 5))
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)

	# adaptive bw, only knn 
	expect_that(fit1 <- dalda(formula = Species ~ ., data = iris, wf = "rectangular", k = 50), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(formula = Species ~ ., data = iris, wf = rectangular(k = 50)), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$bw, NULL)
	expect_true(fit1$nn.only)
	expect_true(fit1$adaptive)
	a <- rep(51, 3) ###
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	# fixed bw, only knn
	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 50))
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, 50)
	expect_true(fit1$nn.only)
	expect_true(!fit1$adaptive)
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)
	
	# nn.only not needed
	expect_that(dalda(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, nn.only = TRUE), gives_warning("argument 'nn.only' is ignored"))

	# nn.only has to be TRUE if bw and k are both given
	expect_that(dalda(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50, nn.only = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
	
	## wf with infinite support
	# fixed bw
	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = gaussian(bw = 0.5))
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit1$bw, 0.5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)
	a <- rep(150, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, function(x) sum(x > 0)), a)

	# adaptive bw, only knn
	expect_that(fit1 <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", k = 50), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(formula = Species ~ ., data = iris, wf = gaussian(k = 50)), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(fit1$adaptive)
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)

	# adaptive bw, all obs
	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", k = 50, nn.only = FALSE)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = gaussian(k = 50, nn.only = FALSE))
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, FALSE)
	expect_true(fit1$adaptive)
	a <- rep(150, 4)
	names(a) <- 0:3
	expect_equal(sapply(fit1$weights, function(x) sum(x > 0)), a)

	# fixed bw, only knn
	expect_that(fit1 <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_that(fit2 <- dalda(formula = Species ~ ., data = iris, wf = gaussian(bw = 1, k = 50)), gives_warning("group setosa is empty or weights in this group are all zero"))
	expect_equal(fit1[-9], fit2[-9])
	expect_equal(fit1$bw, 1)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(!fit1$adaptive)
	a <- rep(50, 3)
	names(a) <- 1:3
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x > 0)), a)
	
	# nn.only has to be TRUE if bw and k are both given
	expect_that(dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50, nn.only = FALSE), throws_error("if 'bw' and 'k' are given argument 'nn.only' must be TRUE"))
})	


#=================================================================================================================
context("predict.dalda")

test_that("predict.dalda works correctly with formula and data.frame interface and with missing newdata", {
	data(iris)
	ran <- sample(1:150,100)
	## formula, data
	fit <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran)
  	pred <- predict(fit)
  	expect_equal(rownames(pred$posterior), rownames(iris)[ran])  	
  	expect_equal(names(pred$class), rownames(iris)[ran])  	
	## formula, data, newdata
	fit <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran)  
  	pred <- predict(fit, newdata = iris[-ran,])
  	expect_equal(rownames(pred$posterior), rownames(iris)[-ran])  	
  	expect_equal(names(pred$class), rownames(iris)[-ran])  	
	## grouping, x
	fit <- dalda(x = iris[,-5], grouping = iris$Species, wf = "gaussian", bw = 2, subset = ran)  
  	pred <- predict(fit)
  	expect_equal(rownames(pred$posterior), rownames(iris)[ran])
  	expect_equal(names(pred$class), rownames(iris)[ran])
	## grouping, x, newdata
	fit <- dalda(x = iris[,-5], grouping = iris$Species, wf = "gaussian", bw = 2, subset = ran)  
  	pred <- predict(fit, newdata = iris[-ran,-5])
  	expect_equal(rownames(pred$posterior), rownames(iris)[-ran])
  	expect_equal(names(pred$class), rownames(iris)[-ran])
})


test_that("predict.dalda: retrieving training data works", {
	data(iris)
	## no subset
	# formula, data
	fit <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris)
  	expect_equal(pred1, pred2)
	# y, x
	fit <- dalda(x = iris[,-5], grouping = iris$Species, wf = "gaussian", bw = 2)  
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[,-5])
  	expect_equal(pred1, pred2)
	## subset
	ran <- sample(1:150,100)
	# formula, data
	fit <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran)
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,])
  	expect_equal(pred1, pred2)
	# y, x
	fit <- dalda(x = iris[,-5], grouping = iris$Species, wf = "gaussian", bw = 2, subset = ran)  
  	pred1 <- predict(fit)
  	pred2 <- predict(fit, newdata = iris[ran,-5])
  	expect_equal(pred1, pred2)
})


test_that("predict.dalda works with missing classes in the training data", {
	data(iris)
	ran <- sample(1:150,100)
	expect_that(fit <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 1:100), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(length(fit$prior), 2)
	a <- rep(50, 2)
	names(a) <- names(fit$counts)
	expect_equal(fit$counts, a)
	expect_equal(fit$N, 100)
	expect_equal(nrow(fit$means), 2)
	pred <- predict(fit, newdata = iris[-ran,])
	expect_equal(nlevels(pred$class), 3)
	expect_equal(ncol(pred$posterior), 2)
	# a <- rep(0,50)
	# names(a) <- rownames(pred$posterior)
	# expect_equal(pred$posterior[,3], a)
})


test_that("predict.dalda works with one single predictor variable", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- dalda(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran)
	expect_equal(ncol(fit$means), 1)
	expect_equal(dim(fit$cov), rep(1, 2))
	pred <- predict(fit, newdata = iris[-ran,])
})


test_that("predict.dalda works with one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran)
  	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred$class), 1)
	expect_equal(dim(pred$posterior), c(1, 3))
	a <- factor("setosa", levels = c("setosa", "versicolor", "virginica"))
	names(a) = "5"
	expect_equal(pred$class, a)
	pred <- predict(fit, newdata = iris[58,])
	expect_equal(length(pred$class), 1)
	expect_equal(dim(pred$posterior), c(1, 3))
	a <- factor("versicolor", levels = c("setosa", "versicolor", "virginica"))
	names(a) = "58"
	expect_equal(pred$class, a)
})	


test_that("predict.dalda works with one single predictor variable and one single test observation", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- dalda(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran)
	expect_equal(ncol(fit$means), 1)
	expect_equal(dim(fit$cov), rep(1, 2))
	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred$class), 1)
	expect_equal(dim(pred$posterior), c(1, 3))
})

   
test_that("predict.dalda: NA handling in newdata works", {
	data(iris)
	ran <- sample(1:150,100)
	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	fit <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 50, subset = ran)
	expect_warning(pred <- predict(fit, newdata = irisna))
	expect_equal(all(is.na(pred$class[1:17])), TRUE)
	expect_equal(all(is.na(pred$posterior[1:17,])), TRUE)	
})


test_that("predict.dalda: misspecified arguments", {
	data(iris)
	ran <- sample(1:150,100)
	fit <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = ran)
    # errors in newdata
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
    # errors in prior
    expect_error(predict(fit, prior = rep(2,length(levels(iris$Species))), newdata = iris[-ran,]))
    expect_error(predict(fit, prior = TRUE, newdata = iris[-ran,]))
    expect_error(predict(fit, prior = 0.6, newdata = iris[-ran,]))
})  

#=================================================================================================================
context("dalda: mlr interface code")

test_that("dalda: mlr interface works", {
	library(mlr)
	source("../../../../mlr/classif.dalda.R")
	task <- makeClassifTask(data = iris, target = "Species")

	# missing parameters
	expect_that(train("classif.dalda", task), gives_warning("either 'bw' or 'k' have to be specified"))

	# class prediction
	lrn <- makeLearner("classif.dalda", par.vals = list(bw = 10))
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- dalda(Species ~ ., data = iris, bw = 10)
	pred2 <- predict(tr2)
	expect_equivalent(pred2$class, pred1@df$response)

	# posterior prediction
	lrn <- makeLearner("classif.dalda", par.vals = list(bw = 10), predict.type = "prob")
	tr1 <- train(lrn, task)
	pred1 <- predict(tr1, task = task)
	tr2 <- dalda(Species ~ ., data = iris, bw = 10)
	pred2 <- predict(tr2)
	expect_true(all(pred2$posterior == pred1@df[,3:5]))
	expect_equivalent(pred2$class, pred1@df$response)
})


#=================================================================================================================
# mod <- dalda(Species ~ Sepal.Length + Sepal.Width, data = iris, wf = "gaussian", bw = 0.5)
# x1 <- seq(4,8,0.05)
# x2 <- seq(2,5,0.05)
# plot(iris[,1], iris[,2], col = iris$Species, cex = mod$weights[[1]])
# plot(iris[,1], iris[,2], col = iris$Species, cex = mod$weights[[2]])
# plot(iris[,1], iris[,2], col = iris$Species, cex = mod$weights[[3]])
# plot(iris[,1], iris[,2], col = iris$Species, cex = mod$weights[[4]])
# legend("bottomright", legend = levels(iris$Species), col = as.numeric(unique(iris$Species)), lty = 1)

# iris.grid <- expand.grid(Sepal.Length = x1, Sepal.Width = x2)
# pred <- predict(mod, newdata = iris.grid)
# prob.grid <- pred$posterior
# contour(x1, x2, matrix(prob.grid[,1], length(x1)), add = TRUE, label = colnames(prob.grid)[1])
# contour(x1, x2, matrix(prob.grid[,2], length(x1)), add = TRUE, label = colnames(prob.grid)[2])
# contour(x1, x2, matrix(prob.grid[,3], length(x1)), add = TRUE, label = colnames(prob.grid)[3])
