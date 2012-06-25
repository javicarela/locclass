#' Determine shape of classes and decision boundaries.
#'
#' A sample of \code{m} points is drawn and their Bayes classes are determined. 
#' We consider the points for each class separately.
#' We consider the connecting lines between all pairs of center points belonging to the same Bayes class and check if they cross 
#' a foreign class. 
#' It is counted how many foreign classes are crossed and how many class boundaries are crossed.
#' Moreover, based on this information, it is determined if classes consist of several subclasses and if classes/subclasses are convex.
#' 
#' @title Determine shape of classes and decision boundaries
#'
#' @param object Results from some data generating function.
#' @param m Number of data points used.
#' @param q Proportion of points near the margin that are taken from the \code{m} points.
#' @param \dots Currently unused.
#'
#' @return A \code{list} as long as the number of classes with the following components:
#' 	\item{nSubclasses}{The number of subclasses found}
#'	\item{propSubclasses}{}
#'	\item{convexityClasses}{}
#'	\item{convexitySubclasses}{}
#'	\item{nForeign}{}
#'	\item{nBoundaries}{}
#'	The last two components are:
#'	\item{data}{}
#'	\item{ybayes}{}
#'
#' @export
#' @import igraph


classShape <- function(object, ...)
	UseMethod("classShape")



#' @rdname classShape
#' @method classShape locClass.flashData
#'
#' @S3method classShape locClass.flashData

classShape.locClass.flashData <- function(object, m = 500, q = 0.1, ...) {
	NextMethod(object, m, q, ...)
}



#' @rdname classShape
#' @method classShape locClass.flexibleData
#'
#' @S3method classShape locClass.flexibleData

classShape.locClass.flexibleData <- function(object, m = 500, q = 0.1, ...) {
	a <- attributes(object)
	K <- length(a$prior)
	d <- ncol(object$x)
	n <- nrow(object$x)
	data <- object
	if (n < m) {		# generate m - n additional observations
		dif <- m - n
		nMix <- rbinom(1, size = dif, prob = a$probMix)
		nOther <- dif - nMix # was ist, wenn in object nOther = 0 war?
		data2 <- flexibleDataHelper(prior = a$prior, K = K, d = d, nMix = nMix, muMix = a$muMix, sigmaMix = a$sigmaMix, lambdaMix = a$lambdaMix, nOther = nOther, muOther = a$muOther, sigmaOther = a$sigmaOther, lambdaOther = a$lambdaOther)
		data$x <- rbind(data$x, data2$x)
		data$y <- c(data$y, data2$y)
	} else {			# sample m out of n observations
		s <- sample(n, size = m, replace = FALSE)
		data$x <- data$x[s,]
		data$y <- data$y[s]
	}	
	return(classShaperHelper1(data, m , q, a, ...))
	# ybayes <- bayes(data)$ybayes
	# return(c(lapply(1:K, function(k) classShapeHelper(data$x[ybayes == k,], a = a)), list(data = data, ybayes = ybayes)))
}



#' @rdname classShape
#' @method classShape locClass.hvData
#'
#' @S3method classShape locClass.hvData

classShape.locClass.hvData <- function(object, m = 500, q = 0.1, ...) {
	a <- attributes(object)
	n <- nrow(object$x)
	data <- object
	if (n < m) {		# generate m - n additional observations
		dif <- m - n
		data2 <- hvData(dif, d = a$d, k = a$k)
		data$x <- rbind(data$x, data2$x)
		data$y <- c(data$y, data2$y)		
	} else {			# sample m out of n observations
		s <- sample(n, size = m, replace = FALSE)
		data$x <- data$x[s,]
		data$y <- data$y[s]		
	}
	return(classShaperHelper1(data, m , q, a, ...))
	# ybayes <- bayes(data)$ybayes
	# K <- nlevels(ybayes)
	# return(c(lapply(1:K, function(k) classShapeHelper(data$x[ybayes == k,], a = a)), list(data = data, ybayes = ybayes)))
}



#' @rdname classShape
#' @method classShape locClass.hvQuadraticData
#'
#' @S3method classShape locClass.hvQuadraticData

classShape.locClass.hvQuadraticData <- function(object, m = 500, q = 0.1, ...) {
	a <- attributes(object)
	n <- nrow(object$x)
	data <- object
	if (n < m) {		# generate m - n additional observations
		dif <- m - n
		data2 <- hvQuadraticData(dif, d = a$d, k = a$k)
		data$x <- rbind(data$x, data2$x)
		data$y <- c(data$y, data2$y)		
	} else {			# sample m out of n observations
		s <- sample(n, size = m, replace = FALSE)
		data$x <- data$x[s,]
		data$y <- data$y[s]		
	}
	return(classShaperHelper1(data, m , q, a, ...))
	# ybayes <- bayes(data)$ybayes
	# K <- nlevels(ybayes)
	# return(c(lapply(1:K, function(k) classShapeHelper(data$x[ybayes == k,], a = a)), list(data = data, ybayes = ybayes)))
}



#' @rdname classShape
#' @method classShape locClass.mixtureData
#'
#' @S3method classShape locClass.mixtureData

classShape.locClass.mixtureData <- function(object, m = 500, q = 0.1, ...) {
	a <- attributes(object)
	n <- nrow(object$x)
	data <- object
	K <- length(a$prior)
	if (n < m) {		# generate m - n additional observations
		dif <- m - n
		data2 <- mixtureData(dif, prior = a$prior, lambda = a$lambda, mu = a$mu, sigma = a$sigma)
		data$x <- rbind(data$x, data2$x)
		data$y <- c(data$y, data2$y)		
	} else {			# sample m out of n observations
		s <- sample(n, size = m, replace = FALSE)
		data$x <- data$x[s,]
		data$y <- data$y[s]		
	}
	return(classShaperHelper1(data, m , q, a, ...))
	# ybayes <- bayes(data)$ybayes
	# return(c(lapply(1:K, function(k) classShapeHelper(data$x[ybayes == k,], a = a)), list(data = data, ybayes = ybayes)))
}



#' @rdname classShape
#' @method classShape locClass.outlierCorrectData
#'
#' @S3method classShape locClass.outlierCorrectData

classShape.locClass.outlierCorrectData <- function(object, m = 500, q = 0.1, ...) {
	NextMethod(object, m, q, ...)
}



#' @rdname classShape
#' @method classShape locClass.outlierWrongData
#'
#' @S3method classShape locClass.outlierWrongData

classShape.locClass.outlierWrongData <- function(object, m = 500, q = 0.1, ...) {
	NextMethod(object, m, q, ...)
}



#' @rdname classShape
#' @method classShape locClass.ringData
#'
#' @S3method classShape locClass.ringData

classShape.locClass.ringData <- function(object, m = 500, q = 0.1, ...) {
	NextMethod(object, m, q, ...)
}



#' @rdname classShape
#' @method classShape locClass.spiralData
#'
#' @S3method classShape locClass.spiralData

classShape.locClass.spiralData <- function(object, m = 500, q = 0.1, ...) {
	a <- attributes(object)
	n <- nrow(object$x)
	data <- object
	if (n < m) {		# generate m - n additional observations
		dif <- m - n
		data2 <- spiralData(dif, cycles = a$cycles)
		data$x <- rbind(data$x, data2$x)
		data$y <- c(data$y, data2$y)		
	} else {			# sample m out of n observations
		s <- sample(n, size = m, replace = FALSE)
		data$x <- data$x[s,]
		data$y <- data$y[s]		
	}
	return(classShaperHelper1(data, m , q, a, ...))
	# ybayes <- bayes(data)$ybayes
	# K <- nlevels(ybayes)
	# return(c(lapply(1:K, function(k) classShapeHelper(data$x[ybayes == k,], a = a)), list(data = data, ybayes = ybayes)))
}



#' @rdname classShape
#' @method classShape locClass.twonormLinearData
#'
#' @S3method classShape locClass.twonormLinearData

classShape.locClass.twonormLinearData <- function(object, m = 500, q = 0.1, ...) {
	NextMethod(object, m, q, ...)
}



#' @rdname classShape
#' @method classShape locClass.twonormQuadraticData
#'
#' @S3method classShape locClass.twonormQuadraticData

classShape.locClass.twonormQuadraticData <- function(object, m = 500, q = 0.1, ...) {
	NextMethod(object, m, q, ...)
}



#' @rdname classShape
#' @method classShape locClass.vData
#'
#' @S3method classShape locClass.vData

classShape.locClass.vData <- function(object, m = 500, q = 0.1, ...) {
	a <- attributes(object)
	n <- nrow(object$x)
	data <- object
	if (n < m) {		# generate m - n additional observations
		dif <- m - n
		data2 <- vData(dif, d = a$d, k = a$k)
		data$x <- rbind(data$x, data2$x)
		data$y <- c(data$y, data2$y)		
	} else {			# sample m out of n observations
		s <- sample(n, size = m, replace = FALSE)
		data$x <- data$x[s,]
		data$y <- data$y[s]		
	}
	return(classShaperHelper1(data, m , q, a, ...))
	# ybayes <- bayes(data)$ybayes
	# K <- nlevels(ybayes)
	# return(c(lapply(1:K, function(k) classShapeHelper(data$x[ybayes == k,], a = a)), list(data = data, ybayes = ybayes)))
}



#' @rdname classShape
#' @method classShape locClass.vNormalLinearData
#'
#' @S3method classShape locClass.vNormalLinearData

classShape.locClass.vNormalLinearData <- function(object, m = 500, q = 0.1, ...) {
	NextMethod(object, m, q, ...)
}



#' @rdname classShape
#' @method classShape locClass.vNormalQuadraticData
#'
#' @S3method classShape locClass.vNormalQuadraticData

classShape.locClass.vNormalQuadraticData <- function(object, m = 500, q = 0.1, ...) {
	NextMethod(object, m, q, ...)
}



#' @rdname classShape
#' @method classShape locClass.wData
#'
#' @S3method classShape locClass.wData

classShape.locClass.wData <- function(object, m = 500, q = 0.1, ...) {
	a <- attributes(object)
	n <- nrow(object$x)
	data <- object
	if (n < m) {		# generate m - n additional observations
		dif <- m - n
		data2 <- wData(dif, d = a$d, k = a$k)
		data$x <- rbind(data$x, data2$x)
		data$y <- c(data$y, data2$y)		
	} else {			# sample m out of n observations
		s <- sample(n, size = m, replace = FALSE)
		data$x <- data$x[s,]
		data$y <- data$y[s]		
	}
	return(classShaperHelper1(data, m , q, a, ...))
	# ybayes <- bayes(data)$ybayes
	# K <- nlevels(ybayes)
	# return(c(lapply(1:K, function(k) classShapeHelper(data$x[ybayes == k,], a = a)), list(data = data, ybayes = ybayes)))
}



#' @rdname classShape
#' @method classShape locClass.xorData
#'
#' @S3method classShape locClass.xorData

classShape.locClass.xorData <- function(object, m = 500, q = 0.1, ...) {
	NextMethod(object, m, q, ...)
}



#' @rdname classShape
#' @method classShape locClass.xor3Data
#'
#' @S3method classShape locClass.xor3Data

classShape.locClass.xor3Data <- function(object, m = 500, q = 0.1, ...) {
	NextMethod(object, m, q, ...)
}



#  @param m As explained above. 
#  @param q As explained above.
#' @noRd

classShaperHelper1 <- function(data, m, q, a, ...) {
	b <- bayes(data)
	margin <- apply(b$posterior, 1, sort, decreasing = TRUE)
	margin <- margin[1,] - margin[2,]
	ord <- order(margin)
	index <- ord[1:(q*m)]
	data$x <- data$x[index,]
	data$y <- data$y[index]
	ybayes <- b$ybayes[index]
	K <- nlevels(ybayes)
	return(c(lapply(1:K, function(k) classShapeHelper(data$x[ybayes == k,], a = a)), list(data = data, ybayes = ybayes)))
}



#  @param m Matrix of data points that belong to the same Bayes class.
#  @param a Attributes of artificial data sets. 
#' @noRd

classShapeHelper <- function(m, a) {
	# adjacency checks for one pair of data points
	# 1. if there is a foreign class on the direct connection between these points and
	#    returns 1 if all considered points on the connection belong to class k and 0 otherwise
	# 2. how many foreign classes are crossed (depends on K)
	# 3. how often a class boundary is crossed (likely to increase with K, )
	adjacency <- function (pair, resolution = 10, a) {
		direction <- m[pair[1],] - m[pair[2],]
		segments <- seq(0, 1, length.out = resolution)
## FIXME: hier besser festen Abstand nehmen ?
		path <- list()
		path$x <- t(sapply(segments, function(x) m[pair[2],] + x * direction))
		attributes(path) <- c(attributes(path), a[-1])
		b <- bayes(path)$ybayes
## sanity check
#print(b[1] == k)
#print(b[resolution] == k)
# reicht nicht foreign = TRUE wenn length(unique(b)) == 1
#		return(c(foreign = as.numeric(all(b == k)), nForeign = length(unique(as.numeric(b))) - 1, nBoundaries = sum(diff(as.numeric(b)) != 0)))
		l <- length(unique(as.numeric(b)))
		return(c(foreign = l == 1, nForeign = l - 1, nBoundaries = sum(diff(as.numeric(b)) != 0)))
## FIXME: normieren mit der Anzahl der Klassen? redundante Information?
	}
	pairs <- combn(nrow(m), 2)									# all pairs of data points
	adj <- apply(pairs, 2, adjacency, a = a)
print(t(rbind(pairs, adj)))
	nForeign <- table(adj["nForeign",])
	nBoundaries <- table(adj["nBoundaries",])
	A <- matrix(0, nrow(m), nrow(m)) 							# build adjacency matrix
	A[t(pairs)] <- adj["foreign",]
	g <- graph.adjacency(A, mode = "undirected") 				# build graph
	g <- simplify(g)
	subg <- decompose.graph(g)									# determine connected components
	nSubclasses <- length(subg)
	tab <- sapply(subg, vcount)
	tab <- tab/sum(tab)											# proportion of centers in different subclasses
	# convexity: Is graph / are subgraphs complete?
	if (nSubclasses > 1) {
		convexityClasses <- FALSE								# class cannot be convex if there are subclasses
																# check if subgraphs are complete
		convexitySubclasses <- sapply(subg, function(s) return(ecount(s) == choose(vcount(s), 2)))
	} else {
		convexityClasses <- ecount(g) == choose(vcount(g),2)	# check if graph is complete
		convexitySubclasses <- NULL								# there are no subclasses
	}	
	return(list(nSubclasses = nSubclasses, propSubclasses = tab, convexityClasses = convexityClasses, convexitySubclasses = convexitySubclasses, nForeign = nForeign, nBoundaries = nBoundaries))
}
