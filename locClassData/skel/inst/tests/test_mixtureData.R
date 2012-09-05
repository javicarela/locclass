context("mixtureData")

test_that("mixtureData, mixturePosterior: all call variants", {
	data <- mixtureData(n = 50, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = diag(2))
	## lambda vector
	# sigma matrix
	mixtureData(n = 50, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = diag(2))
	mixturePosterior(data$x, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = diag(2))
	# sigma list of matrices
	mixtureData(n = 50, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = list(diag(2), diag(2)))
	mixturePosterior(data$x, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = list(diag(2), diag(2)))
	# sigma list of lists
	mixtureData(n = 50, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = list(list(diag(2), diag(2)),list(diag(2), diag(2))))
	mixturePosterior(data$x, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = list(list(diag(2), diag(2)),list(diag(2), diag(2))))
	## lambda list
	# sigma matrix
	mixtureData(n = 50, prior = c(0.5,0.5), lambda = list(c(0.5,0.5), c(0.2, 0.8)), mu = list(diag(2), diag(2)*2), sigma = diag(2))
	mixturePosterior(data$x, prior = c(0.5,0.5), lambda = list(c(0.5,0.5), c(0.2, 0.8)), mu = list(diag(2), diag(2)*2), sigma = diag(2))
	# sigma list of matrices
	mixtureData(n = 50, prior = c(0.5,0.5), lambda = list(c(0.5,0.5), c(0.2, 0.8)), mu = list(diag(2), diag(2)*2), sigma = list(diag(2), diag(2)))
	mixturePosterior(data$x, prior = c(0.5,0.5), lambda = list(c(0.5,0.5), c(0.2, 0.8)), mu = list(diag(2), diag(2)*2), sigma = list(diag(2), diag(2)))
	# sigma list of lists
	mixtureData(n = 50, prior = c(0.5,0.5), lambda = list(c(0.5,0.5), c(0.2, 0.8)), mu = list(diag(2), diag(2)*2), sigma = list(list(diag(2), diag(2)),list(diag(2), diag(2))))
	mixturePosterior(data$x, prior = c(0.5,0.5), lambda = list(c(0.5,0.5), c(0.2, 0.8)), mu = list(diag(2), diag(2)*2), sigma = list(list(diag(2), diag(2)),list(diag(2), diag(2))))
})


## testen, dass posterior und data dieselben Fehlermeldungen geben

test_that("mixtureData, mixturePosterior: misspecified arguments", {
	data <- mixtureData(n = 50, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = diag(2))

	expect_that(mixtureData(n = 50, prior = c(0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = diag(2)), throws_error("'length(mu)' and 'length(prior)' do not match"))
	expect_that(mixturePosterior(data$x, prior = c(0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = diag(2)), throws_error("'length(mu)' and 'length(prior)' do not match"))

	expect_that(mixtureData(n = 50, prior = c(0.5, 0.5), lambda = c(0.5,0.5), mu = list(diag(2), matrix(2, ncol = 1, nrow = 2)), sigma = diag(2)), throws_error("numbers of columns for elements in 'mu' differs"))
	expect_that(mixturePosterior(data$x, prior = c(0.5, 0.5), lambda = c(0.5,0.5), mu = list(diag(2), matrix(2, ncol = 1, nrow = 2)), sigma = diag(2)), throws_error("numbers of columns for elements in 'mu' differs"))
	

	expect_that(mixtureData(n = 50, prior = c(0.5, 0.5), lambda = c(0.5,0.5), mu = list(diag(2)), sigma = diag(2)), throws_error("'length(mu)' and 'length(prior)' do not match"))
	expect_that(mixturePosterior(data$x, prior = c(0.5, 0.5), lambda = c(0.5,0.5), mu = list(diag(2)), sigma = diag(2)), throws_error("'length(mu)' and 'length(prior)' do not match"))

	expect_that(mixtureData(n = 50, prior = c(0.5, 0.5), lambda = list(c(0.5,0.5),1,1), mu = list(diag(2), diag(2)*2), sigma = diag(2)), throws_error("'length(lambda)' and 'length(prior)' do not match"))
	expect_that(mixturePosterior(data$x, prior = c(0.5, 0.5), lambda = list(c(0.5,0.5),1,1), mu = list(diag(2), diag(2)*2), sigma = diag(2)), throws_error("'length(lambda)' and 'length(prior)' do not match"))
	
	expect_that(mixtureData(n = 50, prior = c(0.5, 0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = list(diag(2), diag(3))), throws_error("dimensionality of 'sigma_k' incorrect"))
	expect_that(mixturePosterior(data$x, prior = c(0.5, 0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = list(diag(2), diag(3))), throws_error("dimensionality of 'sigma_k' incorrect"))


	expect_that(mixtureData(n = 50, prior = c(0.5, 0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = list(list(diag(2), diag(2)), list(diag(3), diag(2)))), throws_error("dimensionality of elements in 'sigma' incorrect"))
	expect_that(mixturePosterior(data$x, prior = c(0.5, 0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = list(list(diag(2), diag(2)), list(diag(3), diag(2)))), throws_error("dimensionality of elements in 'sigma' incorrect"))


	expect_that(mixtureData(n = 50, prior = c(0.5, 0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = list(list(diag(2), diag(2)), 2)), throws_error("'sigma_k' is neither list nor matrix"))
	expect_that(mixturePosterior(data$x, prior = c(0.5, 0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = list(list(diag(2), diag(2)), 2)), throws_error("'sigma_k' is neither list nor matrix"))

	
	expect_that(mixtureData(n = 50, prior = c(0.5, 0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = list(diag(2), diag(2), diag(2))), throws_error("'length(sigma)' and 'length(prior)' do not match"))
	expect_that(mixturePosterior(data$x, prior = c(0.5, 0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = list(diag(2), diag(2), diag(2))), throws_error("'length(sigma)' and 'length(prior)' do not match"))
	

	expect_that(mixtureData(n = 50, prior = c(0.5, 0.5), lambda = list(c(0.5,0.5),1), mu = list(diag(2), diag(2)*2), sigma = diag(2)), throws_error("number of mixture components in 'lambda' and 'mu' do not match"))
	expect_that(mixturePosterior(data$x, prior = c(0.5, 0.5), lambda = list(c(0.5,0.5),1), mu = list(diag(2), diag(2)*2), sigma = diag(2)), throws_error("number of mixture components in 'lambda' and 'mu' do not match"))

	expect_that(mixtureData(n = 50, prior = c(0.5, 0.5), lambda = diag(2), mu = list(diag(2), diag(2)*2), sigma = diag(2)), throws_error("'lambda' is neither list nor vector"))
	expect_that(mixturePosterior(data$x, prior = c(0.5, 0.5), lambda = diag(2), mu = list(diag(2), diag(2)*2), sigma = diag(2)), throws_error("'lambda' is neither list nor vector"))

## evtl. passendere Fehlermeldung
	expect_that(mixtureData(n = 50, prior = c(0.5, 0.5), lambda = list(diag(2), diag(2)), mu = list(diag(2), diag(2)*2), sigma = diag(2)), throws_error("number of mixture components in 'lambda' and 'mu' do not match"))
	expect_that(mixturePosterior(data$x, prior = c(0.5, 0.5), lambda = list(diag(2), diag(2)), mu = list(diag(2), diag(2)*2), sigma = diag(2)), throws_error("number of mixture components in 'lambda' and 'mu' do not match"))

	expect_that(mixtureData(n = 50, prior = c(0.5, 0.5), lambda = list(diag(2), diag(2)), mu = list(diag(2), diag(2)*2), sigma = 2), throws_error("'sigma' is neither list nor matrix"))
	expect_that(mixturePosterior(data$x, prior = c(0.5, 0.5), lambda = list(diag(2), diag(2)), mu = list(diag(2), diag(2)*2), sigma = 2), throws_error("'sigma' is neither list nor matrix"))

})


# test_that("bayes, mixturePosterior, mixtureBayesClass are consistent", {
	
# })


test_that("mixtureData, bayes, mixturePosterior, mixtureBayesClass and mixtureLabels can deal with n = 1", {
	data <- mixtureData(n = 1, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = diag(2))
	b <- bayes(data)
	b1 <- mixturePosterior(data$x, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = diag(2))
	b2 <- mixtureBayesClass(data$x, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = diag(2))
	expect_equal(b$posterior, b1)
	expect_equal(b$ybayes, b2)
	mixtureLabels(data$x, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(diag(2), diag(2)*2), sigma = diag(2))
})


test_that("mixtureData, bayes, mixturePosterior, mixtureBayesClass and mixtureLabels can deal with one dimensional-data", {
	## n = 1
	data <- mixtureData(n = 1, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(matrix(1:2, ncol = 1), matrix(3:4, ncol = 1)), sigma = matrix(2))
	b <- bayes(data)
	b1 <- mixturePosterior(data$x, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(matrix(1:2, ncol = 1), matrix(3:4, ncol = 1)), sigma = matrix(2))
	b2 <- mixtureBayesClass(data$x, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(matrix(1:2, ncol = 1), matrix(3:4, ncol = 1)), sigma = matrix(2))
	expect_equal(b$posterior, b1)
	expect_equal(b$ybayes, b2)
	mixtureLabels(data$x, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(matrix(1:2, ncol = 1), matrix(3:4, ncol = 1)), sigma = matrix(2))
	## n = 5
	data <- mixtureData(n = 5, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(matrix(1:2, ncol = 1), matrix(3:4, ncol = 1)), sigma = matrix(2))
	b <- bayes(data)
	b1 <- mixturePosterior(data$x, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(matrix(1:2, ncol = 1), matrix(3:4, ncol = 1)), sigma = matrix(2))
	b2 <- mixtureBayesClass(data$x, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(matrix(1:2, ncol = 1), matrix(3:4, ncol = 1)), sigma = matrix(2))
	expect_equal(b$posterior, b1)
	expect_equal(b$ybayes, b2)
	mixtureLabels(data$x, prior = c(0.5,0.5), lambda = c(0.5,0.5), mu = list(matrix(1:2, ncol = 1), matrix(3:4, ncol = 1)), sigma = matrix(2))
})


test_that("mixtureData, bayes, mixturePosterior, mixtureBayesClass and mixtureLabels can deal with few observations and many components", {
	## n = 1
	data <- mixtureData(n = 1, prior = c(0.5,0.5), lambda = rep(0.1,10), mu = list(cbind(1:10,0), cbind(0,1:10)), sigma = diag(2))
	b <- bayes(data)
	b1 <- mixturePosterior(data$x, prior = c(0.5,0.5), lambda = rep(0.1,10), mu = list(cbind(1:10,0), cbind(0,1:10)), sigma = diag(2))
	b2 <- mixtureBayesClass(data$x, prior = c(0.5,0.5), lambda = rep(0.1,10), mu = list(cbind(1:10,0), cbind(0,1:10)), sigma = diag(2))
	expect_equal(b$posterior, b1)
	expect_equal(b$ybayes, b2)
	mixtureLabels(data$x, prior = c(0.5,0.5), lambda = rep(0.1,10), mu = list(cbind(1:10,0), cbind(0,1:10)), sigma = diag(2))
	## n = 5
	data <- mixtureData(n = 5, prior = c(0.5,0.5), lambda = rep(0.1,10), mu = list(cbind(1:10,0), cbind(0,1:10)), sigma = diag(2))
	b <- bayes(data)
	b1 <- mixturePosterior(data$x, prior = c(0.5,0.5), lambda = rep(0.1,10), mu = list(cbind(1:10,0), cbind(0,1:10)), sigma = diag(2))
	b2 <- mixtureBayesClass(data$x, prior = c(0.5,0.5), lambda = rep(0.1,10), mu = list(cbind(1:10,0), cbind(0,1:10)), sigma = diag(2))
	expect_equal(b$posterior, b1)
	expect_equal(b$ybayes, b2)
	mixtureLabels(data$x, prior = c(0.5,0.5), lambda = rep(0.1,10), mu = list(cbind(1:10,0), cbind(0,1:10)), sigma = diag(2))
})

