context("flexibleData")

K <- 3
centersMix <- 5
centersOther <- 10

test_that("flexibleData works for 'normal' parameter settings", {
	a <- flexibleDataParams(n = 100, probMix = 0.5, centersMix = centersMix, sigmaMix = 0.2, centersOther = centersOther, sigmaOther = 0.5, d = 5, propUseless = 0.5, prior = rep(1/K,K))
	d <- flexibleData(a)
	# prior
	expect_equal(length(a$prior), K)
	# K
	expect_equal(levels(d$y), as.character(1:K))
	expect_equal(a$K, K)
	# d
	expect_equal(a$d, 5)
	# dUseless
	expect_equal(a$dUseless, 2)
	# n
	expect_equal(nrow(d$x), 100)
	expect_equal(length(d$y), 100)
	expect_equal(a$nMix + a$nOther, 100)	
	# centersMix, muMix
	expect_true(all(sapply(a$muMix, is.matrix)))
	expect_equal(sum(sapply(a$muMix, nrow)), centersMix)
	expect_equal(length(a$muMix), a$K)
	expect_equal(sapply(a$muMix, ncol), rep(a$d,a$K))
	expect_true(all(sapply(a$muMix, function(x) all(x[,a$d - 0:(a$dUseless-1)] == 0))))
	# sigmaMix
	expect_equal(a$sigmaMix, rep(0.2, a$K))
	# lambdaMix
	expect_equal(length(a$lambdaMix), a$K)
	expect_true(all(sapply(a$lambdaMix, function(x) all(x >= 0 & x <= 1))))
	expect_equal(sapply(a$lambdaMix, sum), rep(1, a$K))
	# centersOther, muOther
	expect_true(is.matrix(a$muOther))
	expect_equal(nrow(a$muOther), centersOther)
	expect_equal(ncol(a$muOther), a$d)
	# sigmaOther
	expect_equal(a$sigmaOther, 0.5)
	# check attributes
	expect_equal(attributes(d)[-c(1:2)], a)
})


test_that("flexibleData works for 'propUseless = 1'", {
	a <- flexibleDataParams(n = 100, probMix = 0.5, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 5, propUseless = 1, prior = rep(1/3,3))
	d <- flexibleData(a)
	# prior
	expect_equal(length(a$prior), K)
	# K
	expect_equal(levels(d$y), as.character(1:K))
	expect_equal(a$K, K)
	# d
	expect_equal(a$d, 5)
	# dUseless
	expect_equal(a$dUseless, 5)
	# n
	expect_equal(nrow(d$x), 100)
	expect_equal(length(d$y), 100)
	expect_equal(a$nMix + a$nOther, 100)	
	# centersMix, muMix
	expect_true(all(sapply(a$muMix, is.matrix)))
	expect_equal(sum(sapply(a$muMix, nrow)), centersMix)
	expect_equal(length(a$muMix), a$K)
	expect_equal(sapply(a$muMix, ncol), rep(a$d,a$K))
	expect_true(all(sapply(a$muMix, function(x) all(x[,a$d - 0:(a$dUseless-1)] == 0))))
	# expect_equal(a$muMix[[1]], matrix(0, ncol = 5, nrow = nrow(a$muMix[[1]])))
	# expect_equal(a$muMix[[2]], matrix(0, ncol = 5, nrow = nrow(a$muMix[[2]])))
	# expect_equal(a$muMix[[3]], matrix(0, ncol = 5, nrow = nrow(a$muMix[[3]])))
	# sigmaMix
	expect_equal(a$sigmaMix, rep(0.2, a$K))
	# lambdaMix
	expect_equal(length(a$lambdaMix), a$K)
	expect_true(all(sapply(a$lambdaMix, function(x) all(x >= 0 & x <= 1))))
	expect_equal(sapply(a$lambdaMix, sum), rep(1, a$K))
	# centersOther, muOther
	expect_true(is.matrix(a$muOther))
	expect_equal(nrow(a$muOther), centersOther)
	expect_equal(ncol(a$muOther), a$d)
	# sigmaOther
	expect_equal(a$sigmaOther, 0.5)
	# check attributes
	expect_equal(attributes(d)[-c(1:2)], a)
})


test_that("flexibleData works for 'd = 1'", {
	## d, dUseless = 1
	a <- flexibleDataParams(n = 100, probMix = 0.5, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 1, propUseless = 1, prior = rep			(1/3,3))
	d <- flexibleData(a)
	# prior
	expect_equal(length(a$prior), K)
	# K
	expect_equal(levels(d$y), as.character(1:K))
	expect_equal(a$K, K)
	# d
	expect_equal(a$d, 1)
	# dUseless
	expect_equal(a$dUseless, 1)
	# n
	expect_equal(nrow(d$x), 100)
	expect_equal(length(d$y), 100)
	expect_equal(a$nMix + a$nOther, 100)	
	# centersMix, muMix
	expect_true(all(sapply(a$muMix, is.matrix)))
	expect_equal(sum(sapply(a$muMix, nrow)), centersMix)
	expect_equal(length(a$muMix), a$K)
	expect_equal(sapply(a$muMix, ncol), rep(a$d,a$K))
	expect_true(all(sapply(a$muMix, function(x) all(x[,a$d - 0:(a$dUseless-1)] == 0))))
	# expect_equal(a$muMix[[1]], matrix(0, ncol = 1, nrow = nrow(a$muMix[[1]])))
	# expect_equal(a$muMix[[2]], matrix(0, ncol = 1, nrow = nrow(a$muMix[[2]])))
	# expect_equal(a$muMix[[3]], matrix(0, ncol = 1, nrow = nrow(a$muMix[[3]])))
	# sigmaMix
	expect_equal(a$sigmaMix, rep(0.2, a$K))
	# lambdaMix
	expect_equal(length(a$lambdaMix), a$K)
	expect_true(all(sapply(a$lambdaMix, function(x) all(x >= 0 & x <= 1))))
	expect_equal(sapply(a$lambdaMix, sum), rep(1, a$K))
	# centersOther, muOther
	expect_true(is.matrix(a$muOther))
	expect_equal(nrow(a$muOther), centersOther)
	expect_equal(ncol(a$muOther), a$d)
	# sigmaOther
	expect_equal(a$sigmaOther, 0.5)
	# check attributes
	expect_equal(attributes(d)[-c(1:2)], a)

	## d, dUseful = 1 
	a <- flexibleDataParams(n = 100, probMix = 0.5, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 1, propUseless = 0, prior = rep			(1/3,3))
	d <- flexibleData(a)
	# prior
	expect_equal(length(a$prior), K)
	# K
	expect_equal(levels(d$y), as.character(1:K))
	expect_equal(a$K, K)
	# d
	expect_equal(a$d, 1)
	# dUseless
	expect_equal(a$dUseless, 0)
	# n
	expect_equal(nrow(d$x), 100)
	expect_equal(length(d$y), 100)
	expect_equal(a$nMix + a$nOther, 100)	
	# centersMix, muMix
	expect_true(all(sapply(a$muMix, is.matrix)))
	expect_equal(sum(sapply(a$muMix, nrow)), centersMix)
	expect_equal(length(a$muMix), a$K)
	expect_equal(sapply(a$muMix, ncol), rep(a$d,a$K))
	# expect_true(all(sapply(a$muMix, function(x) all(x[,a$d - 0:(a$dUseless-1)] == 0))))
	# expect_equal(a$muMix[[1]], matrix(0, ncol = 1, nrow = nrow(a$muMix[[1]])))
	# expect_equal(a$muMix[[2]], matrix(0, ncol = 1, nrow = nrow(a$muMix[[2]])))
	# expect_equal(a$muMix[[3]], matrix(0, ncol = 1, nrow = nrow(a$muMix[[3]])))
	# sigmaMix
	expect_equal(a$sigmaMix, rep(0.2, a$K))
	# lambdaMix
	expect_equal(length(a$lambdaMix), a$K)
	expect_true(all(sapply(a$lambdaMix, function(x) all(x >= 0 & x <= 1))))
	expect_equal(sapply(a$lambdaMix, sum), rep(1, a$K))
	# centersOther, muOther
	expect_true(is.matrix(a$muOther))
	expect_equal(nrow(a$muOther), centersOther)
	expect_equal(ncol(a$muOther), a$d)
	# sigmaOther
	expect_equal(a$sigmaOther, 0.5)
	# check attributes
	expect_equal(attributes(d)[-c(1:2)], a)
})


test_that("flexibleData works for 'n = 1'", {
	## nOther or nMix = 0
	## d > 1
	a <- flexibleDataParams(n = 1, probMix = 0.5, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 10, propUseless = 0, prior = rep			(1/3,3))
	d <- flexibleData(a)
	# prior
	expect_equal(length(a$prior), K)
	# K
	expect_equal(levels(d$y), as.character(1:K))
	expect_equal(a$K, K)
	# d
	expect_equal(a$d, 10)
	# dUseless
	expect_equal(a$dUseless, 0)
	# n
	expect_equal(nrow(d$x), 1)
	expect_equal(length(d$y), 1)
	expect_equal(a$nMix + a$nOther, 1)	
	# centersMix, muMix
	expect_true(all(sapply(a$muMix, is.matrix)))
	expect_equal(sum(sapply(a$muMix, nrow)), centersMix)
	expect_equal(length(a$muMix), a$K)
	expect_equal(sapply(a$muMix, ncol), rep(a$d,a$K))
	# expect_true(all(sapply(a$muMix, function(x) all(x[,a$d - 0:(a$dUseless-1)] == 0))))
	# sigmaMix
	expect_equal(a$sigmaMix, rep(0.2, a$K))
	# lambdaMix
	expect_equal(length(a$lambdaMix), a$K)
	expect_true(all(sapply(a$lambdaMix, function(x) all(x >= 0 & x <= 1))))
	expect_equal(sapply(a$lambdaMix, sum), rep(1, a$K))
	# centersOther, muOther
	expect_true(is.matrix(a$muOther))
	expect_equal(ncol(a$muOther), a$d)
	expect_equal(nrow(a$muOther), centersOther)
	# sigmaOther
	expect_equal(a$sigmaOther, 0.5)
	# check attributes
	expect_equal(attributes(d)[-c(1:2)], a)

	## d = 1
	a <- flexibleDataParams(n = 1, probMix = 0.5, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 1, propUseless = 0, prior = rep			(1/3,3))
	d <- flexibleData(a)
	# prior
	expect_equal(length(a$prior), K)
	# K
	expect_equal(levels(d$y), as.character(1:K))
	expect_equal(a$K, K)
	# d
	expect_equal(a$d, 1)
	# dUseless
	expect_equal(a$dUseless, 0)
	# n
	expect_equal(nrow(d$x), 1)
	expect_equal(length(d$y), 1)
	expect_equal(a$nMix + a$nOther, 1)	
	# centersMix, muMix
	expect_true(all(sapply(a$muMix, is.matrix)))
	expect_equal(sum(sapply(a$muMix, nrow)), centersMix)
	expect_equal(length(a$muMix), a$K)
	expect_equal(sapply(a$muMix, ncol), rep(a$d,a$K))
	# expect_true(all(sapply(a$muMix, function(x) all(x[,a$d - 0:(a$dUseless-1)] == 0))))
	# sigmaMix
	expect_equal(a$sigmaMix, rep(0.2, a$K))
	# lambdaMix
	expect_equal(length(a$lambdaMix), a$K)
	expect_true(all(sapply(a$lambdaMix, function(x) all(x >= 0 & x <= 1))))
	expect_equal(sapply(a$lambdaMix, sum), rep(1, a$K))
	# centersOther, muOther
	expect_true(is.matrix(a$muOther))
	expect_equal(ncol(a$muOther), a$d)
	expect_equal(nrow(a$muOther), centersOther)
	# sigmaOther
	expect_equal(a$sigmaOther, 0.5)
	# check attributes
	expect_equal(attributes(d)[-c(1:2)], a)
})


test_that("flexibleData works for 'probMix = 0, 1'", {
	## probMix = 0
	a <- flexibleDataParams(n = 100, probMix = 0, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 5, propUseless = 0, prior = rep(1/3,3))
	d <- flexibleData(a)
	# prior
	expect_equal(length(a$prior), K)
	# K
	expect_equal(levels(d$y), as.character(1:K))
	expect_equal(a$K, K)
	# d
	expect_equal(a$d, 5)
	# dUseless
	expect_equal(a$dUseless, 0)
	# n
	expect_equal(nrow(d$x), 100)
	expect_equal(length(d$y), 100)
	expect_equal(a$nOther, 100)
	expect_equal(a$nMix, 0)
	# centersMix, muMix
	expect_true(all(sapply(a$muMix, is.matrix)))
	expect_equal(sum(sapply(a$muMix, nrow)), centersMix)
	expect_equal(length(a$muMix), a$K)
	expect_equal(sapply(a$muMix, ncol), rep(a$d,a$K))
	# expect_true(all(sapply(a$muMix, function(x) all(x[,a$d - 0:(a$dUseless-1)] == 0))))
	# sigmaMix
	expect_equal(a$sigmaMix, rep(0.2, a$K))
	# lambdaMix
	expect_equal(length(a$lambdaMix), a$K)
	expect_true(all(sapply(a$lambdaMix, function(x) all(x >= 0 & x <= 1))))
	expect_equal(sapply(a$lambdaMix, sum), rep(1, a$K))
	# centersOther, muOther
	expect_true(is.matrix(a$muOther))
	expect_equal(nrow(a$muOther), centersOther)
	expect_equal(ncol(a$muOther), a$d)
	# sigmaOther
	expect_equal(a$sigmaOther, 0.5)
	# check attributes
	expect_equal(attributes(d)[-c(1:2)], a)

	## probOther = 0
	a <- flexibleDataParams(n = 100, probMix = 1, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 5, propUseless = 0, prior = rep(1/3,3))
	d <- flexibleData(a)
	# prior
	expect_equal(length(a$prior), K)
	# K
	expect_equal(levels(d$y), as.character(1:K))
	expect_equal(a$K, K)
	# d
	expect_equal(a$d, 5)
	# dUseless
	expect_equal(a$dUseless, 0)
	# n
	expect_equal(nrow(d$x), 100)
	expect_equal(length(d$y), 100)
	expect_equal(a$nOther, 0)
	expect_equal(a$nMix, 100)
	# centersMix, muMix
	expect_true(all(sapply(a$muMix, is.matrix)))
	expect_equal(sum(sapply(a$muMix, nrow)), centersMix)
	expect_equal(length(a$muMix), a$K)
	expect_equal(sapply(a$muMix, ncol), rep(a$d,a$K))
	# expect_true(all(sapply(a$muMix, function(x) all(x[,a$d - 0:(a$dUseless-1)] == 0))))
	# sigmaMix
	expect_equal(a$sigmaMix, rep(0.2, a$K))
	# lambdaMix
	expect_equal(length(a$lambdaMix), a$K)
	expect_true(all(sapply(a$lambdaMix, function(x) all(x >= 0 & x <= 1))))
	expect_equal(sapply(a$lambdaMix, sum), rep(1, a$K))
	# centersOther, muOther
	expect_true(is.null(a$muOther))
	# sigmaOther
	expect_true(is.null(a$sigmaOther))
	# check attributes
	expect_equal(attributes(d)[-c(1:2)], a)
})


test_that("flexibleData works if priors are zero", {
	a <- flexibleDataParams(n = 100, probMix = 0.5, centersMix = centersMix, sigmaMix = 0.2, centersOther = centersOther, sigmaOther = 0.5, d = 5, propUseless = 0.5, prior = c(0,0.5,0.5))
	d <- flexibleData(a)
	# prior
	expect_equal(length(a$prior), K)
	# K
	expect_equal(levels(d$y), as.character(1:K))
	expect_equal(length(unique(d$y)), 2)
	expect_equal(a$K, K)
	# d
	expect_equal(a$d, 5)
	# dUseless
	expect_equal(a$dUseless, 2)
	# n
	expect_equal(nrow(d$x), 100)
	expect_equal(length(d$y), 100)
	expect_equal(a$nMix + a$nOther, 100)	
	# centersMix, muMix
	expect_true(all(sapply(a$muMix, is.matrix)))
	expect_equal(sum(sapply(a$muMix, nrow)), centersMix)
	expect_equal(length(a$muMix), a$K)
	expect_equal(sapply(a$muMix, ncol), rep(a$d,a$K))
	expect_true(all(sapply(a$muMix, function(x) all(x[,a$d - 0:(a$dUseless-1)] == 0))))
	# sigmaMix
	expect_equal(a$sigmaMix, rep(0.2, a$K))
	# lambdaMix
	expect_equal(length(a$lambdaMix), a$K)
	expect_true(all(sapply(a$lambdaMix, function(x) all(x >= 0 & x <= 1))))
	expect_equal(sapply(a$lambdaMix, sum), rep(1, a$K))
	# centersOther, muOther
	expect_true(is.matrix(a$muOther))
	expect_equal(nrow(a$muOther), centersOther)
	expect_equal(ncol(a$muOther), a$d)
	# sigmaOther
	expect_equal(a$sigmaOther, 0.5)
	# check attributes
	expect_equal(attributes(d)[-c(1:2)], a)
})



test_that("flexibleData works if centersMix is 1 larger than the number of classes", {
	# centersMix = 3, K = 2
	a <- flexibleDataParams(n = 100, probMix = 0.5, centersMix = 3, sigmaMix = 0.2, centersOther = 2, sigmaOther = 0.5, d = 5, propUseless = 0.5, prior = c(0.5,0.5))
	d <- flexibleData(a)
})



test_that("flexibleData works if centersOther = 1", {
	# centersOther = 1
	centersMix <- 3
	a <- flexibleDataParams(n = 100, probMix = 0.5, centersMix = 3, sigmaMix = 0.2, centersOther = 1, sigmaOther = 0.5, d = 5, propUseless = 0.5, prior = c(0,0.5,0.5))
	d <- flexibleData(a)
})



test_that("seeding for flexibleData works", {
	## no seed at all -> neither distribution params nor data are equal
	a1 <- flexibleDataParams(n = 100, probMix = 0, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 5, propUseless = 0, prior = rep(1/3,3))
	d1 <- flexibleData(a1)
	a2 <- flexibleDataParams(n = 100, probMix = 0, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 5, propUseless = 0, prior = rep(1/3,3))
	d2 <- flexibleData(a2)

	expect_true(is.character(all.equal(a1, a2)))
	expect_true(is.character(all.equal(d1, d2)))
	expect_true(is.character(all.equal(d1$x, d2$x)))
	expect_true(is.character(all.equal(d1$y, d2$y)))

	# save state of the RNG
	prev.seed <- .Random.seed
	
	## param Seed, no dataSeed -> distribution params are equal, data are equal
	set.seed(321)
	a1 <- flexibleDataParams(n = 100, probMix = 0, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 5, propUseless = 0, prior = rep(1/3,3))
	d1 <- flexibleData(a1)
	set.seed(321)
	a2 <- flexibleDataParams(n = 100, probMix = 0, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 5, propUseless = 0, prior = rep(1/3,3))
	d2 <- flexibleData(a2)

	expect_equal(a1, a2)
	expect_equal(d1, d2)
		
	## param Seed, no dataSeed -> distribution params are equal, data are not equal if the previous state of the RNG is not restored
	set.seed(321)
	a1 <- flexibleDataParams(n = 100, probMix = 0, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 5, propUseless = 0, prior = rep(1/3,3))
	assign(".Random.seed", prev.seed, envir=.GlobalEnv)
	d1 <- flexibleData(a1)
	prev.seed2 <- .Random.seed
	set.seed(321)
	a2 <- flexibleDataParams(n = 100, probMix = 0, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 5, propUseless = 0, prior = rep(1/3,3))
	assign(".Random.seed", prev.seed2, envir=.GlobalEnv)
	d2 <- flexibleData(a2)

	expect_equal(a1, a2)
	expect_true(is.character(all.equal(d1, d2)))
	expect_true(is.character(all.equal(d1$x, d2$x)))
	expect_true(is.character(all.equal(d1$y, d2$y)))
		
	## dataSeed, no paramSeed -> distribution params and data are not equal
	assign(".Random.seed", prev.seed, envir=.GlobalEnv)
	a1 <- flexibleDataParams(n = 100, probMix = 0.2, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 5, propUseless = 0, prior = rep(1/3,3))
	set.seed(123)
	d1 <- flexibleData(a1)
	a2 <- flexibleDataParams(n = 100, probMix = 0.2, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 5, propUseless = 0, prior = rep(1/3,3))
	set.seed(123)
	d2 <- flexibleData(a2)

	expect_true(is.character(all.equal(a1, a2)))
	expect_true(is.character(all.equal(d1, d2)))
	expect_true(is.character(all.equal(d1$x, d2$x)))
	expect_true(is.character(all.equal(d1$y, d2$y)))

	# d1 and d2 have different probMix arguments -> neither distribution params nor data are equal
	set.seed(123)
	a1 <- flexibleDataParams(n = 100, probMix = 0.8, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 5, propUseless = 0, prior = rep(1/3,3))
	d1 <- flexibleData(a1)
	set.seed(123)
	a2 <- flexibleDataParams(n = 100, probMix = 0, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 5, propUseless = 0, prior = rep(1/3,3))
	d2 <- flexibleData(a2)

	expect_true(is.character(all.equal(d1, d2)))
	expect_true(is.character(all.equal(a1, a2)))
	expect_true(is.character(all.equal(d1$x, d2$x)))
	expect_true(is.character(all.equal(d1$y, d2$y)))
	
	## paramSeed and dataSeed -> distribution params and data are equal
	set.seed(321)
	a1 <- flexibleDataParams(n = 100, probMix = 0, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 5, propUseless = 0, prior = rep(1/3,3))
	set.seed(123)
	d1 <- flexibleData(a1)
	set.seed(321)
	a2 <- flexibleDataParams(n = 100, probMix = 0, centersMix = 5, sigmaMix = 0.2, centersOther = 10, sigmaOther = 0.5, d = 5, propUseless = 0, prior = rep(1/3,3))
	set.seed(123)
	d2 <- flexibleData(a2)
	expect_equal(d1, d2)
	expect_equal(d1$x, d2$x)
	expect_equal(d1$y, d2$y)
})
