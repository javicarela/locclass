#======================	
	mod <- dalda(Species ~ Sepal.Length + Sepal.Width, data = iris, wf = "gaussian", bw = 0.5)
	x1 <- seq(4,8,0.05)
	x2 <- seq(2,5,0.05)
	plot(iris[,1], iris[,2], col = iris$Species, cex = mod$weights[[1]]*10)
	plot(iris[,1], iris[,2], col = iris$Species, cex = mod$weights[[2]]*10)
	plot(iris[,1], iris[,2], col = iris$Species, cex = mod$weights[[3]]*10)
	plot(iris[,1], iris[,2], col = iris$Species, cex = mod$weights[[4]]*10)
	legend("bottomright", legend = levels(iris$Species), col = as.numeric(unique(iris$Species)), lty = 1)

iris.grid <- expand.grid(Sepal.Length = x1, Sepal.Width = x2)
pred <- predict(mod, newdata = iris.grid)
prob.grid <- pred$posterior
contour(x1, x2, matrix(prob.grid[,1], length(x1)), add = TRUE, label = colnames(prob.grid)[1])
contour(x1, x2, matrix(prob.grid[,2], length(x1)), add = TRUE, label = colnames(prob.grid)[2])
contour(x1, x2, matrix(prob.grid[,3], length(x1)), add = TRUE, label = colnames(prob.grid)[3])



	mod <- daqda(Species ~ Sepal.Length + Sepal.Width, data = iris, wf = "gaussian", bw = 0.5)
	x1 <- seq(4,8,0.05)
	x2 <- seq(2,5,0.05)
	plot(iris[,1], iris[,2], col = iris$Species, cex = mod$weights[[1]]*10)
	plot(iris[,1], iris[,2], col = iris$Species, cex = mod$weights[[2]]*10)
	plot(iris[,1], iris[,2], col = iris$Species, cex = mod$weights[[3]]*10)
	plot(iris[,1], iris[,2], col = iris$Species, cex = mod$weights[[4]]*10)
	legend("bottomright", legend = levels(iris$Species), col = as.numeric(unique(iris$Species)), lty = 1)

iris.grid <- expand.grid(Sepal.Length = x1, Sepal.Width = x2)
pred <- predict(mod, newdata = iris.grid)
prob.grid <- pred$posterior
contour(x1, x2, matrix(prob.grid[,1], length(x1)), add = TRUE, label = colnames(prob.grid)[1])
contour(x1, x2, matrix(prob.grid[,2], length(x1)), add = TRUE, label = colnames(prob.grid)[2])
contour(x1, x2, matrix(prob.grid[,3], length(x1)), add = TRUE, label = colnames(prob.grid)[3])



#======================	


test_that("dalda works", {
	data(iris)
	
	## formula
	# wrong variable names
	expect_error(dalda(formula = Species ~ V1, data = iris, wf = "gaussian", bw = 10))
	
	# numeric grouping variable
	expect_that(dalda(formula = Sepal.Length ~ ., data = iris, wf = "gaussian", bw = 10), gives_warning("'grouping' was coerced to a factor"))
	expect_error(dalda(formula = Petal.Width ~ ., data = iris, wf = "gaussian", bw = 10))  ## system singular
	
	# wrong class
	expect_error(dalda(formula = iris, data = iris, wf = "gaussian", bw = 10))
	expect_error(dalda(iris, data = iris, wf = "gaussian", bw = 10))
	
	## data.frame/matrix
	# numeric grouping variable
	expect_that(dalda(grouping = iris[,1], x = iris[,-1], wf = "gaussian", bw = 10), gives_warning("'grouping' was coerced to a factor"))     ## works, but warning that target variable was coerced to a factor
	expect_error(dalda(grouping = iris[,4], x = iris[,-1], wf = "gaussian", bw = 10))     ## system singular
	
	# target variable also in x
	expect_error(dalda(grouping = iris$Species, x = iris, wf = "gaussian", bw = 10))      ## system singular
	expect_warning(dalda(Species ~ Species + Petal.Width, data = iris, wf = "gaussian", bw = 10))           ## warning, Species on RHS removed
	
	# missing x
	expect_error(dalda(grouping = iris$Species, wf = "gaussian", bw = 10))
	  	  
	## missing classes: only one class
	expect_that(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 1:50), gives_warning(c("groups versicolor, virginica are empty or weights in these groups are all zero", "training data from only one group, breaking out of iterative procedure"))) ## break out of for-loop
  	  
	## subset
	# wrong class
	expect_error(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = iris[1:10,]))
	expect_error(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = FALSE))
	expect_error(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 0))
	# nonsensical indices
	expect_error(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = -10:50))

	# initial weights & subset
	expect_that(d <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 1:100), gives_warning("group virginica is empty or weights in this group are all zero"))
	expect_equal(length(d$prior), 2)
	expect_equal(nrow(d$counts), rep(50,2))
	expect_equal(nrow(d$N), 100)
	expect_equal(nrow(d$means), 2)
	expect_equal(d$weights[["0"]], rep(1,100))
	d <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 19, subset = 1:100, weights = 1:150)
	expect_equal(d$weights[["0"]], 1:150)

	## na.action
	irisna <- iris
	irisna[1:10,c(1,3)] <- NA	
	# default na.omit
	d <- dalda(Species ~ ., data = irisna, wf = "gaussian", bw = 10)
	expect_equal(sapply(d$weights, length), rep(140, 4))
	# check if na.omit works correctly
	fit1 <- dalda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, na.action = na.omit)
	fit2 <- dalda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, subset = 11:150)
	expect_equal(fit1[-c(8, 17)], fit2[-8])
	# na.fail
	expect_error(dalda(Species ~ ., data = irisna, wf = "gaussian", bw = 10, na.action = na.fail))

	# one predictor variable
	d <- dalda(Species ~ Petal.Width, data = iris, k = 50)
	expect_equal(ncol(d$means), 1)	
	expect_equal(dim(d$cov), rep(1, 2))	

	# one training observation
	expect_error(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, subset = 1))            ## system singular
	
	# one training observation in one predictor variable
	expect_error(dalda(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 1, subset = 1))   ## system singular

	# itr
	expect_error(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, itr = -5))
	expect_error(dalda(Species ~ ., data = iris, wf = "gaussian", bw = 10, itr = 0))
	
	# bw, k not necessary
	#dalda(Species ~ ., data = iris, wf = "gaussian", bw = 0.5, k = 30)
	expect_that(fit1 <- dalda(Species ~ ., data = iris, wf = gaussian(0.5), k = 30, bw = 0.5), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	fit2 <- dalda(Species ~ ., data = iris, wf = gaussian(0.5))
	expect_equal(fit1[-8], fit2[-8])

	expect_that(fit1 <- dalda(Species ~ ., data = iris, wf = gaussian(0.5), bw = 0.5), gives_warning("argument 'bw' is ignored"))	
	fit2 <- dalda(Species ~ ., data = iris, wf = gaussian(0.5))
	expect_equal(fit1[-8], fit2[-8])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, 0.5)	
	expect_equal(fit1$adaptive, FALSE)	

	expect_that(fit1 <- dalda(Species ~ ., data = iris, wf = gaussian(0.5), k = 30, bw = 0.5), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	expect_that(fit1 <- dalda(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5, k = 30), gives_warning(c("argument 'k' is ignored", "argument 'bw' is ignored")))
	expect_that(fit2 <- dalda(Species ~ ., data = iris, wf = function(x) exp(-x), k = 30), gives_warning("argument 'k' is ignored"))
	expect_equal(fit1[-8], fit2[-8])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, FALSE)	

	expect_that(fit1 <- dalda(Species ~ ., data = iris, wf = function(x) exp(-x), bw = 0.5), gives_warning("argument 'bw' is ignored"))
	fit2 <- dalda(Species ~ ., data = iris, wf = function(x) exp(-x))
	expect_equal(fit1[-8], fit2[-8])
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)	
	expect_equal(fit1$bw, NULL)	
	expect_equal(fit1$adaptive, FALSE)	

	# missing quotes
	expect_error(dalda(formula = Species ~ ., data = iris, wf = gaussian)) ## error because length(weights) and nrow(x) are different

	# bw missing
 	##expect_error(dalda(formula = Species ~ ., data = iris, wf = "gaussian", k = 50))
	expect_that(dalda(formula = Species ~ ., data = iris, wf = gaussian()), gives_error("either 'bw' or 'k' have to be specified"))
	expect_that(dalda(formula = Species ~ ., data = iris, wf = gaussian(), k = 10), gives_error("either 'bw' or 'k' have to be specified"))
	# bw < 0
	expect_error(dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = -5))
	expect_error(dalda(formula = Species ~ ., data = iris, wf = "cosine", k = 10, bw = -50))
	# bw vector
	expect_that(dalda(formula = Species ~., data = iris, wf = "gaussian", bw = rep(1, nrow(iris))), gives_warning("only first element of 'bw' used"))
	
	# bw and k missing
	expect_error(dalda(Species ~ ., data = iris))
	#dalda(Species ~ ., data = iris, bw = 3)

	# k < 0
	expect_error(dalda(formula = Species ~ ., data = iris, wf = "gaussian", k =-7, bw = 50))
	# k too small
	expect_error(dalda(formula = Species ~ ., data = iris, wf = "gaussian", k = 5, bw = 0.005))
	# k too large
	expect_error(dalda(formula = Species ~ ., data = iris, k = 250, wf = "gaussian", bw = 50))
	# k vector
	expect_that(dalda(formula = Species ~., data = iris, wf = "gaussian", k = rep(50, nrow(iris))), gives_warning("only first element of 'k' used"))

	# try all available weight functions
	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5)    
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = gaussian(0.5))    
	expect_equal(fit1[-8], fit2[-8])
	
	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5, k = 30)    
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = gaussian(bw = 0.5, k = 30))    
	expect_equal(fit1[-8], fit2[-8])
	expect_equal(lapply(fit1$weights[2:4], function(x) sum(x > 0)), rep(30, 3))
	
	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "epanechnikov", bw = 5, k = 30)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = epanechnikov(bw = 5, k = 30))
	expect_equal(fit1[-8], fit2[-8])

	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "rectangular", bw = 2, k = 30)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = rectangular(bw = 2, k = 30))
	expect_equal(fit1[-8], fit2[-8])

	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "triangular", bw = 5, k = 30)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = triangular(5, k = 30))
	expect_equal(fit1[-8], fit2[-8])

	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "biweight", bw = 5, k = 30)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = biweight(5, k = 30))
	expect_equal(fit1[-8], fit2[-8])

	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "optcosine", bw = 5, k = 30)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = optcosine(5, k = 30))
	expect_equal(fit1[-8], fit2[-8])

	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "cosine", bw = 5, k = 30)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = cosine(5, k = 30))
	expect_equal(fit1[-8], fit2[-8])

	# individual weight functions
	#dalda(Species ~ ., data = iris, wf = function(x) exp(-x))
	#dalda(Species ~ ., data = iris, wf = function(x) exp(-x), k = 30)    ## warning: k ignored
	
	### variants weighting
	## nonlocal
	#fit1 <- dalda(formula = Species ~ ., data = iris, wf = "rectangular", k = 150)
	#fit2 <- dalda(formula = Species ~ ., data = iris, wf = rectangular(k = 150))
	#all.equal(fit1[-8], fit2[-8])
	#fit1$itr == 1

	## wf with finite support
	# fixed bw
	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = rectangular(bw = 5))
	expect_equal(fit1[-8], fit2[-8])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)

	# adaptive bw, only knn 
	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "rectangular", k = 50)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = rectangular(k = 50))
	expect_equal(fit1[-8], fit2[-8])
	is.null(fit1$bw)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$bw, NULL)
	expect_true(fit1$nn.only)
	expect_true(fit1$adaptive)
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x>0)), rep(50, 3))

	# fixed bw, only knn
	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = rectangular(bw = 5, k = 50))
	expect_equal(fit1[-8], fit2[-8])
	expect_equal(fit1$bw, 5)
	expect_equal(fit1$k, 50)
	expect_true(fit1$nn.only)
	expect_true(!fit1$adaptive)
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x>0)), rep(50, 3))
	
	# nn.only not needed
	expect_that(dalda(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, nn.only = TRUE), gives_warning("argument 'nn.only' is ignored"))

	# nn.only has to be TRUE if bw and k are both given
	expect_error(dalda(formula = Species ~ ., data = iris, wf = "rectangular", bw = 5, k = 50, nn.only = FALSE))
	
	## wf with infinite support
	# fixed bw
	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 0.5)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = gaussian(bw = 0.5))
	expect_equal(fit1[-8], fit2[-8])
	expect_equal(fit1$bw, 0.5)
	expect_equal(fit1$k, NULL)
	expect_equal(fit1$nn.only, NULL)
	expect_true(!fit1$adaptive)
	expect_equal(sapply(fit1$weights, function(x) sum(x>0)), rep(150, 4))

	# adaptive bw, only knn
	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", k = 50)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = gaussian(k = 50))
	expect_equal(fit1[-8], fit2[-8])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, NULL)
	expect_true(fit1$adaptive)
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x>0)), rep(50, 3))

	# adaptive bw, all obs
	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", k = 50, nn.only = FALSE)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = gaussian(k = 50, nn.only = FALSE))
	expect_equal(fit1[-8], fit2[-8])
	expect_equal(fit1$bw, NULL)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, FALSE)
	expect_true(fit1$adaptive)
	expect_equal(sapply(fit1$weights, function(x) sum(x>0)), rep(150, 4))

	# fixed bw, only knn
	fit1 <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50)
	fit2 <- dalda(formula = Species ~ ., data = iris, wf = gaussian(bw = 1, k = 50))
	expect_equal(fit1[-8], fit2[-8])
	expect_equal(fit1$bw, 1)
	expect_equal(fit1$k, 50)
	expect_equal(fit1$nn.only, TRUE)
	expect_true(!fit1$adaptive)
	expect_equal(sapply(fit1$weights[2:4], function(x) sum(x>0)), rep(50, 3))
	
	# nn.only has to be TRUE if bw and k are both given
	expect_error(dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50, nn.only = FALSE))
	
	## initial weights
	fit <- dalda(formula = Species ~ ., data = iris, wf = "gaussian", bw = 1, k = 50, weights = rep(1:3,50))
	expect_equal(fit$weights[[1]], rep(1:3, 50))

})


test_that("predict.dalda", {
  	ran <- sample(1:150,100)  
  	fit <- dalda(formula = Species ~ ., data = iris, wf = "rectangular", k = 80, subset = ran)  
  	predict(fit, newdata= iris[-ran,])
})

test_that("predict.dalda: training observations from at least one class are missing", {
  	ran <- sample(1:150,100)  
	expect_warning(fit <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 2, subset = 1:100))
	p <- predict(fit, newdata = iris[-ran,])
	nlevels(p$class) == 3
	ncol(p$posterior) == 2
})	
	
test_that("predict.dalda: one predictor variable (and one test observation)", {
  	ran <- sample(1:150,100)  
	fit <- dalda(Species ~ Petal.Width, data = iris, wf = "gaussian", bw = 2, subset = ran)
	predict(fit, newdata = iris[-ran,])
	predict(fit, newdata = iris[5,])
})

test_that("predict.dalda: one test observation", {
	fit <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 2)
    # one test observation
  	predict(fit, newdata = iris[5,])
  	predict(fit, newdata = iris[58,])
})	
	
test_that("predict.dalda: newdata argument", {
  	expect_error(predict(fit, newdata = TRUE))
  	expect_error(predict(fit, newdata = -50:50)) ## ???
})
  
test_that("predict.dalda: prior argument", {
  	ran <- sample(1:150,100)  
  	expect_error(predict(fit, prior = rep(2,length(levels(iris$Species))), newdata= iris[-ran,]))
  	expect_error(predict(fit, prior = TRUE, newdata= iris[-ran,]))
  	expect_error(predict(fit, prior = 0.6, newdata= iris[-ran,]))
})
 	
test_that("predict.dalda: NAs in newdata", {
  	ran <- sample(1:150,100)  
 	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	fit <- dalda(Species ~ ., data = iris, wf = "gaussian", bw = 50, subset = ran)
	predict(fit, newdata = irisna[-ran,]) 	
})