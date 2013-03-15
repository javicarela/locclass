#' Calculate various characteristics of data sets for e.g. meta analysis.
#'
#' @title Calculate Data Set Characteristics
#'
#' @param object Object of class \code{locClassData}
# @param noise ...
#' @param sparseness ...
# @param classEntropy ...
#' @param distribution ...
# @param relevance ...
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

separability <- function(object, ...)
	UseMethod("separability")

## FIXME:
## Nkmin: nur aus den Klassen wählen, wo überhaupt beobachtungen da sind?
## ENH: Schätzmethode?
## generell: auch varianzen bestimmter maße benutzen???? histogramme?
## class specific sparseness: weighted mean
## outliers



#' @S3method characterize locClass
#' @method characterize locClass
#' @rdname characterize

characterize.locClass <- function(object, sparseness = TRUE, distribution = TRUE, separability = TRUE, classShape = TRUE, ...) {
	ch <- basics(object)
	if (sparseness)
		ch <- c(ch, sparseness(object, ch))
	if (distribution)
		ch <- c(ch, distribution(object, ch))
	if (separability)
		ch <- c(ch, separability(object, ch))
	if (classShape)
		ch <- c(ch, classShape(object, ...))
	ch$Nk <- as.list(ch$Nk) # oder rausnehmen???
	return(ch)
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
	ch$Nk <- table(object$y)					# # of observations per class
	ch$KM <- sum(ch$Nk == 0) #ch$K - length(unique(object$y))	# # of missing classes
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
	## class-specific sparseness measures
	ch$NkMin <- min(chBasics$Nk[chBasics$Nk > 0])		# minimum # of observations per class
	ch$NkMean <- mean(chBasics$Nk[chBasics$Nk > 0])		# mean # of observations per class
	ch$NkMax <- max(chBasics$Nk[chBasics$Nk > 0])
	## only classes with at least 1 training observation are taken into account
	ch$NkVRMin <- ch$NkMin/chBasics$V					# observation/dimension ratio
	ch$NkVRMean <- mean(chBasics$Nk/chBasics$V)
	ch$NkVRMax <- ch$NkMax/chBasics$V
	ch$DSkMin <- ch$NkMin^(1/chBasics$V)				# data sparseness (assume exponential relationship)
	ch$DSkMean <- mean(chBasics$Nk^(1/chBasics$V))
	ch$DSkMax <- ch$NkMax^(1/chBasics$V)
	# anz. beobachtungen pro klasse mittelwert, varianz min, max
	return(ch)
}




#' @S3method distribution locClass
#' @method distribution locClass
#' @rdname characterize

distribution.locClass <- function(object, chBasics, ...) {
	ch <- list()
		
	## distribution characteristics class variable
	require(entropy)
	ch$ENH <- entropy.empirical(as.vector(chBasics$Nk))/log(chBasics$K)		# normalized Shannon entropy
	ch$ENHP <- entropy.empirical(as.vector(chBasics$Nk))/log(chBasics$K - chBasics$KM)	# normalized Shannon entropy of present classes
	## intrinsic dimensionality, dazu mutual information zw. features und class ausrechnen
	## mi.empirical() freuqnecies selbst besorgen
	## FIXME: mutual information und weitere koeffizienten; nicht sinnvoll für stetige erklärende Variablen

	## distribution characteristics predictor variables
	## redundancy
	xk <- split(as.data.frame(object$x), object$y)
	ind <- sapply(xk, nrow) > 0					# in case of missing classes
	## pooled correlation and pooled multiple correlation
	correlation <- function(x) {
		rho <- cor(x)
		rhos <- (sum(abs(rho)) - chBasics$V)/(chBasics$V*(chBasics$V-1))
		# we calculate the correlation matrix for each class separately and sum up the absolute values, we subtract the
		# sum of diagonal values which equals V; there are V*(V-1)/2 pairs of variables, off-diagonal elements
		# of the correlation matrix are considered twice in the sum, hence we need the factor 2 in the denominator which
		# explains the denominator V*(V-1)
		Rs <- mean(sapply(1:chBasics$V, function(z) rho[z,-z,drop = FALSE] %*% solve(rho[-z,-z]) %*% rho[-z,z,drop = FALSE]))
		return(c(rhos = rhos, Rs = Rs))
	}
	if (chBasics$V > 1) {
		r <- sapply(xk[ind], correlation)
		#sapply(xk[ind], function(x) (sum(abs(cor(x))) - chBasics$V)/(chBasics$V*(chBasics$V-1)))
		ch$rhoMin <- min(r["rhos",])
		ch$rhoMean <- weighted.mean(r["rhos",], chBasics$Nk[ind])
		ch$rhoMax <- max(r["rhos",])
		# Rs <- colMeans(sapply(xk[ind], multipleCorrelation))
		ch$RMin <- min(r["Rs",])
		ch$RMean <- weighted.mean(r["Rs",], chBasics$Nk[ind])
		ch$RMax <- max(r["Rs",])
		## FIXME: distribution of R over predictors
		## FIXME: wahre priors nehmen?
	} else {
		ch$rhoMin <- NA
		ch$rhoMean <- NA
		ch$rhoMax <- NA
		ch$RMin <- NA
		ch$RMean <- NA
		ch$RMax <- NA
	}
	## overall correlation instead of class-specific?
	
	## univariate skewness and kurtosis weglassen?
	# univariat keinen durchschnitt bilden, sondern min und max o.ä.
	## tests for multivariate normality
	if (chBasics$V > 1) {
		require(ICS)
		skew <- sapply(xk[ind], function(x) mvnorm.skew.test(x)$p.value)
		kurt <- sapply(xk[ind], function(x) mvnorm.kur.test(x, method = "simulation")$p.value)
		ch$skewMin <- min(skew)
		ch$skewMean <- weighted.mean(skew, chBasics$Nk[ind])
		ch$skewMax <- max(skew)
		ch$kurtMin <- min(kurt)
		ch$kurtMean <- weighted.mean(kurt, chBasics$Nk[ind])
		ch$kurtMax <- max(kurt)
		require(energy)
		etest <- sapply(xk[ind], function(x) mvnorm.etest(x)$p.value)
		ch$etestMin <- min(etest)
		ch$etestMean <- weighted.mean(etest, chBasics$Nk[ind])
		ch$etestMax <- max(etest)
	} else {
		## mvnorm.skew.test and mvnorm.kur.test do not work for dimensionality 1
		## mvnorm.etest: bug for dimensionality 1
		## FIXME
		# sapply(xk[ind], skewness)
		# sapply(xk[ind], kurtosis)		
		ch$skewMin <- NA
		ch$skewMean <- NA
		ch$skewMax <- NA
		ch$kurtMin <- NA
		ch$kurtMean <- NA
		ch$kurtMax <- NA
		ch$etestMin <- NA
		ch$etestMean <- NA
		ch$etestMax <- NA
	}
	
	## Statlog: homogenität kovarianzmatrizen nötig?

	## variations in feature sd? sinnvoll, wenn daten skaliert werden?
		
	return(ch)
}



#' @S3method separability locClass
#' @method separability locClass
#' @rdname characterize

separability.locClass <- function(object, chBasics, ...) {
	ch <- list()

	# Bayes error
	ch$noise <- bayes(object)$noise				

	## information contained in the predictor variables	
	ch$EIC <- 1 - ch$noise/(1 - max(chBasics$Nk)/chBasics$N)
	
	## loo error of LDA - Bayes error
	require(MASS)
	require(nnet)
	ch$LDAE <- mean(lda(y ~ ., data = as.data.frame(object), CV = TRUE)$class != object$y)
	ch$LDAS <- ch$LDAE - ch$noise
	## logreg error
	predLogReg <- predict(multinom(y ~ ., data = as.data.frame(object), trace = FALSE))
	predLogReg <- factor(predLogReg, levels = levels(object$y))
	ch$LRE <- mean(predLogReg != object$y)
	ch$LRS <- ch$LRE - ch$noise
	## first canonical correlation
	cancors <- cancor(object$x, diag(chBasics$K)[as.numeric(object$y),])$cor
	ch$FCC <- max(cancors)
	## normalized first canonical correlation
	ch$NFCC <- ch$FCC + ch$noise
	# if fcc is large there cannot be much noise 
	# if fcc is small there are two causes: the decision boundary is not linear or there is much noise
	# if decision boundary is linear, but the noise level is high the first canonical correlation is small
	# in order to get a measure for the form of the decision boundary the noise is added
	## variation explained by the first canonical variable
	# lambda <- cancors^2/(1 - cancors^2) ## so in benchmark implementiert, warum?
	lambda <- cancors^2	# so stimmt es mit den Werten im statlog paper überein
	ch$frac1 <- max(lambda)/sum(lambda)	
	
	## loo error of QDA - Bayes error
	d <- as.data.frame(object)
	if (any(ind <- chBasics$Nk == 0)) {
		d$y <- factor(d$y, levels = levels(d$y)[!ind])
	}	
	ch$QDAE <- mean(qda(y ~ ., data = d, CV = TRUE)$class != d$y)
	ch$QS <- ch$QDAE - ch$noise

	return(ch)
}



#' @S3method characterize locClass.flexibleData
#' @method characterize locClass.flexibleData
#' @rdname characterize
#'
#' @param pars Logical. Should further data characteristics be calculated from the parameters used to generate the data?

characterize.locClass.flexibleData <- function(object, pars = TRUE, ...) {
	ch <- NextMethod(object, ...)
	if (pars) {
		## data sparseness ratio
		## known relationship between x and y, number of parameters -> relationship N and V
		# Nmin <- 5 ###???
		# ch$DSR <- ch$N/Nmin

		a <- attributes(object)
		ch$prior <- a$prior		# verschieden lang
		## information content of predictors
		ch$IC <- 1 - ch$noise/(1 - max(a$prior))
		## normalized Shannon entropy
		require(entropy)
		ch$NH <- entropy.empirical(a$prior)/log(ch$K)
		
		## proportion of irrelevant variables
		ch$PIV <- a$dUseless/ch$V

		ch$probMix <- a$probMix
		ch$NMix <- a$nMix	

		## # of mixture components
		centersMix <- sapply(a$muMix, nrow)
		ch$centersMix <- sum(centersMix)
		ch$cntersMixMin <- min(centersMix)
		ch$centersMixMean <- mean(centersMix)
		ch$centersMixMax <- max(centersMix)
## FIXME: mean(centersMix) gewichtetes arithmetisches Mittel?

		ch$sigmaMix <- a$sigmaMix	# verschieden lang
		
		ch$lambdaMix <- a$lambdaMix	# verschieden lang

		ch$NOther <- a$nOther
		
		if (a$probMix < 1) {# evtl. ist nOther trotzdem 0 und keine Beobachtung wurde aus dieser Verteilung erzeugt
			ch$centersOther <- nrow(a$muOther)
			ch$sigmaOther <- a$sigmaOther		
		} else {
			ch$centersOther <- NA
			ch$sigmaOther <- NA
		}
	}
	return(ch)
}
