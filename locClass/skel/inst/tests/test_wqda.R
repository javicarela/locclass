test_that("wqda works", {
	data(iris)

	## formula
	# wrong variable names
	expect_error(wqda(formula = Species ~ V1, data = iris))

	# numeric grouping variable
	expect_warning(wqda(formula = Petal.Width ~ ., data = iris))

	# wrong class
	expect_error(wqda(formula = iris, data = iris))

	## data.frame/matrix
	# numeric grouping variable
	expect_warning(wqda(grouping = iris[,1], x = iris[,-1]))

	# target variable also in x
	#expect_error(wqda(grouping = iris$Species, x = iris))          ## funktioniert, sollte aber nicht

	# missing x
	expect_error(wqda(grouping = iris$Species))

	## subset
	# wrong class
	expect_error(wqda(Species ~ ., data = iris, subset = iris[1:10,]))
	expect_error(wqda(Species ~ ., data = iris, subset = FALSE))
	expect_error(wqda(Species ~ ., data = iris, subset = 0))
	# nonsensical indices
	expect_error(wqda(Species ~ ., data = iris, subset = -10:50))

  	## na.action
	irisna <- iris
	irisna[1:10,c(1,3)] <- NA	
	# default na.omit
	expect_warning(wqda(Species ~ ., data = irisna, subset = 6:60),"group virginica is empty or weights in this group are all zero")
	# na.fail
	expect_error(wqda(Species ~ ., data = irisna, subset = 6:60, na.action = na.fail))
	# check if na.omit works correctly
	fit1 <- wqda(Species ~ ., data = irisna, subset = 6:60, na.action = na.omit)
	fit2 <- wqda(Species ~ ., data = irisna, subset = 11:60)
	expect_equal(fit1[-c(8, 11)], fit2[-8])
	
	#fit1 <- wqda(Species ~ ., data = irisna, weights = 1:150, subset = 6:60, na.action = na.omit)

	# one predictor variable
	wqda(Species ~ Petal.Width, data = iris, subset = 6:60)
	
	# one training observation
	#expect_error(wqda(Species ~ ., data = iris, subset = 1))            ## funktioniert???
	
	# one training observation in one predictor variable
	#expect_error(wqda(Species ~ Petal.Width, data = iris, subset = 1))   ## funktioniert

	# check if weighted solution with weights = 1 equals weighted batch solution
	l2 <- wqda(Species ~ ., data = iris, weights = rep(1,150))
	l1 <- wqda(Species ~ ., data = iris)
	expect_equal(l1[c(1:3,5:7)],l2[c(1:3,5:7)])
	expect_equal(length(l1$weights), 150)

	# check if updated solution with lambda = 0.8 equals weighted batch solution
	# l <- onlda(Species ~ ., data = iris, subset = 1:110)
	# l2 <- onlda(Species ~ ., data = iris, object = l, subset = 111:150, lambda = 0.8)
	# l1 <- wqda(Species ~ ., data = iris, weights = c(rep(0.8,110), rep(1,40)))
	# checkEquals(l1[c(1,3,5)],l2[c(1,3,5)])                                              ### ???

	## updates in conjunction with missing classes
	# check if updated solution with lambda = 0.8 equals weighted batch solution
	# l <- onlda(Species ~ ., data = iris, subset = 1:60)
	# l2 <- onlda(Species ~ ., data = iris, object = l, subset = 61:110, lambda = 0.8)
	# l1 <- wqda(Species ~ ., data = iris[-(111:150),], weights = c(rep(0.8,60), rep(1,50)))
	# checkEquals(l1[c(1,3,5)],l2[c(1,3,5)])                                                  ### ???

	## wrong weights
	# weights in a matrix
	weight <- matrix(seq(1:150),nrow=50)
	expect_error(wqda(Species ~ ., data = iris, weights = weight))
	# weights < 0
	expect_error(wqda(Species ~ ., data = iris, weights = rep(-5, 150)))
	# weights true/false
	expect_error(wqda(Species ~ ., data = iris, weights = TRUE))
  
	## wrong method argument
	# missing quotes
	expect_error(wqda(Species ~ ., data = iris, method = ML))
	# method as vector
	expect_error(wqda(Species ~ ., data = iris, method = c("ML","unbiased")))
})


test_that("predict.wqda works", {
	data(iris)
	ran <- sample(1:150,100)  
  
	# missing classes
	expect_warning(fit <- wqda(Species ~ ., data = iris, subset = 1:100))
	p <- predict(fit, newdata = iris[-ran,])
	expect_equal(nlevels(p$class), 3)
	expect_equal(ncol(p$posterior), 2)
	
	# one predictor variable
	fit <- wqda(Species ~ Petal.Width, data = iris, subset = ran)
	expect_equal(ncol(fit$means), 1)
	expect_equal(dim(fit$cov[[1]]), rep(1, 2))
	predict(fit, newdata = iris[-ran,])
	
	# one predictor variable and one test observation
	fit <- wqda(Species ~ Petal.Width, data = iris, subset = ran)
	expect_equal(ncol(fit$means), 1)
	expect_equal(dim(fit$cov[[1]]), rep(1, 2))
	pred <- predict(fit, newdata = iris[5,])
	expect_equal(length(pred$class), 1)
	expect_equal(dim(pred$posterior), c(1, 3))
	
	# 
	fit <- wqda(formula = Species ~ ., data = iris, subset = ran)  
  	predict(fit, newdata = iris[-ran,])
  	
    # one test observation
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
   
    # errors in newdata
    expect_error(predict(fit, newdata = TRUE))
    expect_error(predict(fit, newdata = -50:50))
  
    # errors in prior
    expect_error(predict(fit, prior = rep(2,length(levels(iris$Species))), newdata = iris[-ran,]))
    expect_error(predict(fit, prior = TRUE, newdata = iris[-ran,]))
    expect_error(predict(fit, prior = 0.6, newdata = iris[-ran,]))
    
	irisna <- iris
	irisna[1:17,c(1,3)] <- NA
	# NA in newdata
	fit <- wqda(Species ~ ., data = iris, subset = ran)
	expect_warning(pred <- predict(fit, newdata = irisna))
	expect_equal(all(is.na(pred$class[1:17])), TRUE)
	expect_equal(all(is.na(pred$posterior[1:17,])), TRUE)	
})