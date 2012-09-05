#' Calculate various characteristics of data sets for e.g. meta analysis.
#'
#' @title Calculate Data Set Characteristics
#'
#' @param object Object of class \code{locClassData}
# @param noise ...
#' @param sparseness ...
# @param classEntropy ...
#' @param distribution ...
#' @param relevance ...
#' @param separability ...
#' @param classShape ...
#' @param chBasics ...
#' @param \dots ...
#'
#' @references
#' StatLog
#' van der Walt Diss
#'
#' @rdname characterize
#' @export

characterize <- function(object, ...)
	UseMethod("characterize")

#' @rdname characterize
#' @export

basics <- function(object, ...)
	UseMethod("basics")

#' @rdname characterize
#' @export

sparseness <- function(object, ...)
	UseMethod("sparseness")

#' @rdname characterize
#' @export

distribution <- function(object, ...)
	UseMethod("distribution")

#' @rdname characterize
#' @export

relevance <- function(object, ...)
	UseMethod("relevance")

#' @rdname characterize
#' @export

separability <- function(object, ...)
	UseMethod("separability")

## FIXME:
## Nkmin: nur aus den Klassen wählen, wo überhaupt beobachtungen da sind?
## ENH: Schätzmethode?
## generell: auch varianzen bestimmter maße benutzen????
## class specific sparseness: weighted mean


#' @S3method characterize locClass
#' @method characterize locClass
#' @rdname characterize

characterize.locClass <- function(object, sparseness = TRUE, distribution = TRUE, relevance = TRUE, separability = TRUE, classShape = TRUE, ...) {
	ch <- basics(object)
	if (sparseness)
		ch <- c(ch, sparseness(object, ch))
	if (distribution)
		ch <- c(ch, distribution(object, ch))
	if (relevance)
		ch <- c(ch, relevance(object, ch))
	if (separability)
		ch <- c(ch, separability(object, ch))
	if (classShape)
		ch <- c(ch, classShape(object, ch))
	ch$Nk <- as.list(ch$Nk) # oder rausnehmen???
	return(as.data.frame(ch))
}



#' @S3method basics locClass
#' @method basics locClass
#' @rdname characterize

basics.locClass <- function(object, ...) {
	ch <- list()
	## basic properties, mainly used for calculation or normalization of further data characteristics
	ch$N <- nrow(object$x)						# # of observations
	ch$V <- ncol(object$x)						# dimensionality
	ch$K <- nlevels(object$y)					# # of classes
	ch$KM <- ch$K - length(unique(object$y))	# # of missing classes
	ch$Nk <- table(object$y)					# # of observations per class
	ch$noise <- bayes(object)$noise				# Bayes error
	return(ch)	
}



#' @S3method sparseness locClass
#' @method sparseness locClass
#' @rdname characterize

sparseness.locClass <- function(object, chBasics, ...) {
	ch <- list()
	## sparseness measures on whole data set
	ch$NVR <- chBasics$N/chBasics$V						# # of observations per dimension
	ch$DS <- chBasics$N^(1/chBasics$V)					# data sparseness (assume exponential relationship)
	# class-specific sparseness measures
	ch$NkMin <- min(chBasics$Nk)						# minimum # of observations per class
	#ch$NkMax <- max(basics$Nk)
	ch$NkMean <- mean(chBasics$Nk)					# mean # of observations per class
	ch$NkVRMin <- ch$NkMin/chBasics$V					# observation/dimension ratio
	#NkVRmax <- ch$NkMax/basics$V
	ch$NkVRMean <- mean(chBasics$Nk/chBasics$V)
	ch$DSkMin <- ch$NkMin^(1/chBasics$V)				# data sparseness
	# DSkmax <- ch$NkMax^(1/chBasics$V)
	ch$DSkMean <- mean(chBasics$Nk^(1/chBasics$V))

	# anz. beobachtungen pro klasse mittelwert, varianz min, max
	
	return(ch)
}




#' @S3method distribution locClass
#' @method distribution locClass
#' @rdname characterize

distribution.locClass <- function(object, chBasics, ...) {
	ch <- list()
		
	## distribution characteristics class
	require(entropy)
	ch$ENH <- entropy.empirical(as.vector(chBasics$Nk))/log(chBasics$K)		# normalized Shannon entropy
	## intrinsic dimensionality, dazu mutual information zw. features und class ausrechnen
	## mi.empirical() freuqnecies selbst besorgen
	ch$ENHP <- entropy.empirical(as.vector(chBasics$Nk))/log(chBasics$K - chBasics$KM)	# normalized Shannon entropy of present classes

	## distribution characteristics predictors
	xk <- split(as.data.frame(object$x), object$y)
	# correlation, weighted mean
	if (chBasics$V > 1) {
		macs <- sapply(xk, function(x) (sum(abs(cor(x))) - chBasics$V)/(chBasics$V*(chBasics$V-1)))
		# we calculate the correlation matrix for each class separately and sum up the absolute values, we subtract the
		# sum of diagonal values which equals V; there are V*(V-1)/2 pairs of variables, off-diagonal elements
		# of the correlation matrix are considered twice in the sum, hence we need the factor 2 in the denominator which
		# explains the denominator V*(V-1)
		ch$mac <- weighted.mean(macs, chBasics$Nk)
	} else {
		ch$mac <- NA
	}
	
	require(ICS)
	## skewness and kurtosis weglassen?
	# univariat keinen durchschnitt bilden, sondern min und max o.ä.
	## tests for multivariate normality
	skew <- sapply(xk, function(x) mvnorm.skew.test(x)$p.value)
	kurt <- sapply(xk, function(x) mvnorm.kur.test(x, method = "simulation")$p.value)
	
	require(energy)
	etest <- sapply(xk, function(x) mvnorm.etest(x)$p.value)
	ch$skewMin <- min(skew)
	ch$skewMean <- weighted.mean(skew, chBasics$Nk)
	ch$skewMax <- max(skew)
	ch$kurtMin <- min(kurt)
	ch$kurtMean <- weighted.mean(kurt, chBasics$Nk)
	ch$kurtMax <- max(kurt)
	ch$etestMin <- min(etest)
	ch$etestMean <- weighted.mean(etest, chBasics$Nk)
	ch$etestMax <- max(etest)
	
	
	## Statlog: homogenität kovarianzmatrizen nötig?

	## variations in feature sd? sinnvoll, wenn daten skaliert werden?
	
	
	return(ch)
}



#' @S3method relevance locClass
#' @method relevance locClass
#' @rdname characterize

relevance.locClass <- function(object, chBasics, ...) {
	ch <- list()
	ch$EIC <- 1 - chBasics$noise/(1 - max(chBasics$Nk)/chBasics$N)
	return(ch)
}



#' @S3method separability locClass
#' @method separability locClass
#' @rdname characterize

separability.locClass <- function(object, chBasics, ...) {
	ch <- list()
	
	## loo error of LDA - Bayes error
	require(MASS)
	ch$LDAE <- mean(lda(y ~ ., data = as.data.frame(object), CV = TRUE)$class != object$y)
	ch$LS <- ch$LDAE - chBasics$noise
	## first canonical correlation
	cancors <- cancor(object$x, diag(chBasics$K)[as.numeric(object$y),])$cor
	ch$FCC <- max(cancors)
	## normalized first canonical correlation
	ch$NFCC <- ch$FCC + chBasics$noise
	# if fcc is large there cannot be much noise 
	# if fcc is small there are two causes: the decision boundary is not linear or there is much noise
	# if decision boundary is linear, but the noise level is high the first canonical correlation is small
	# in order to get a measure for the form of the decision boundary the noise is added
	## variation explained by the first canonical variable
	# lambda <- cancors^2/(1 - cancors^2) ## so in benchmark implementiert, warum?
	lambda <- cancors^2
	ch$frac1 <- max(lambda)/sum(lambda)	
	
	## loo error of QDA - Bayes error
	ch$QDAE <- mean(qda(y ~ ., data = as.data.frame(object), CV = TRUE)$class != object$y)
	ch$QS <- ch$QDAE - chBasics$noise

	return(ch)
}



#' @S3method characterize locClass.flexibleData
#' @method characterize locClass.flexibleData
#' @rdname characterize

characterize.locClass.flexibleData <- function(object, ...) {
	ch <- NextMethod(object, ...)
	a <- attributes(object)
	
	## data sparseness ratio
	## known relationship between x and y, number of parameters -> relationship N and V
	Nmin <- 5 ###???
	ch$DSR <- ch$N/Nmin

	## information content of predictors
	ch$IC <- 1 - ch$noise/(1 - max(a$prior))

	## normalized Shannon entropy
	require(entropy)
	ch$NH <- entropy.empirical(a$prior)/log(ch$K)

	ch$NMix <- a$nMix	
	ch$NOther = a$nOther

	## proprotion of irrelevant variables
	ch$PIV <- a$dUseless/ch$V

	centersMix <- sapply(a$muMix, nrow)
	ch$centersMix <- sum(centersMix)
	ch$minCentersMix <- min(centersMix)
## FIXME: mean(centersMix) gewichtetes arithmetisches Mittel?
	ch$meanCentersMix <- mean(centersMix)
	ch$maxCentersMix <- max(centersMix)
	
	#ch$sigmaMix <- a$sigmaMix

	if (a$nOther > 0) {
		ch$centersOther <- nrow(a$muOther)
		#ch$sigmaOther <- a$sigmaOther		
	} else {
		ch$centersOther <- NA
		ch$sigmaOther <- NA
	}
		
## lambdaMix? entropy?

	return(ch)
}
