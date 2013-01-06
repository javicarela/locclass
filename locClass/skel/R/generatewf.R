#  Copyright (C) 2011-2013 J. Schiffner
#  Copyright (C) R Development Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

# @param wf A string. The name of the window function. Default is \code{"biweight"}.
# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
# @param nn.only (Logical. Only required for window functions with infinite support.) Should only the k nearest neighbors or all observations receive positive weights? Defaults to \code{TRUE}.
# @param /dots Unused.
#
#' @noRd

generatewf <- function(wf = c("biweight", "cauchy", "cosine", "epanechnikov", "exponential", "gaussian",
	"optcosine", "rectangular", "triangular"), bw, k, nn.only = TRUE, n, ...) {
	wf <- match.arg(wf)
	if (missing(bw)) {		# bw missing
		if (missing(k))		# bw and k missing
			stop("either 'bw' or 'k' have to be specified")		
		else {				# only k given -> adaptive bandwidth
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
			if (k+1 > n)
				stop("'k' is larger than 'n - 1'")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
			# window functions with infinite support are cut depending on nn.only
			wfunc <- switch(wf,
        		biweight = function(x) {
            		ax <- abs(x)
            		sax <- sort(ax)
            		bw <- sax[k] + 1e-06
        			ifelse(ax < bw, 15/16 * (1 - (ax/bw)^2)^2/bw, 0)
            		# ax <- abs(x)
            		# sax <- unique(sort(ax))
            		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
        			# ifelse(ax < bw, 15/16 * (1 - (ax/bw)^2)^2/bw, 0)
            		# ax <- abs(x)
            		# bw <- sort(ax)[k+1] + .Machine$double.eps
        			# ifelse(ax < bw, 15/16 * (1 - (ax/bw)^2)^2/bw, 0)
        		},
        		cauchy = if (nn.only) {
        			function(x) {
	            		ax <- abs(x)
    	        		sax <- sort(ax)
        	    		bw <- sax[k] + 1e-06
						ind <- ax < bw
            			weights <- numeric(length(ax))
    	        		weights[ind] <- 1/(pi * (1 + (ax[ind]/bw)^2) * bw)
        	    		weights
	            		# ax <- abs(x)
	            		# sax <- unique(sort(ax))
    	        		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
						# ind <- ax < bw
            			# weights <- numeric(length(ax))
    	        		# weights[ind] <- 1/(pi * (1 + (ax[ind]/bw)^2) * bw)
        	    		# weights
	            		# ax <- abs(x)
            			# ord <- order(ax)
        	    		# bw <- ax[ord[k]] + min((ax[ord[k+1]] - ax[ord[k]])/2, 1e-06)
            			# weights <- numeric(length(ord))
    	        		# weights[ord[1:k]] <- 1/(pi * (1 + (ax[ord[1:k]]/bw)^2) * bw)
        	    		# weights
        				# ax <- abs(x)
            			# ord <- order(ax)
            			# bw <- ax[ord[k+1]] + .Machine$double.eps
            			# weights <- numeric(length(ord))
    	        		# weights[ord[1:k]] <- 1/(pi * (1 + (ax[ord[1:k]]/bw)^2) * bw)
        	    		# weights
        			}
        		} else {
        			function(x) {
	            		ax <- abs(x)
    	        		sax <- sort(ax)
        	    		bw <- sax[k] + 1e-06
        				1/(pi * (1 + (ax/bw)^2) * bw)        				
	            		# ax <- abs(x)
	            		# sax <- unique(sort(ax))
	            		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
        				# 1/(pi * (1 + (ax/bw)^2) * bw)        				
        				# bw <- sort(abs(x))[k+1] + .Machine$double.eps
        				# 1/(pi * (1 + (x/bw)^2) * bw)
        			}
        		},
      		  	cosine = function(x) {
            		ax <- abs(x)
   	        		sax <- sort(ax)
       	    		bw <- sax[k] + 1e-06
            		ifelse(ax < bw, (1 + cos(pi * ax/bw))/(2 * bw), 0)
      		  		# ax <- abs(x)
            		# sax <- unique(sort(ax))
            		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
            		# ifelse(ax < bw, (1 + cos(pi * ax/bw))/(2 * bw), 0)
      		  		# ax <- abs(x)
            		# bw <- sort(ax)[k+1] + .Machine$double.eps
            		# ifelse(ax < bw, (1 + cos(pi * ax/bw))/(2 * bw), 0)
            	},
       		 	epanechnikov = function(x) {
            		ax <- abs(x)
   	        		sax <- sort(ax)
       	    		bw <- sax[k] + 1e-06
            		ifelse(ax < bw, 3/4 * (1 - (ax/bw)^2)/bw, 0)
      		  		# ax <- abs(x)
            		# sax <- unique(sort(ax))
            		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
            		# ifelse(ax < bw, 3/4 * (1 - (ax/bw)^2)/bw, 0)
            		# ax <- abs(x)
            		# bw <- sort(ax)[k+1] + .Machine$double.eps
            		# ifelse(ax < bw, 3/4 * (1 - (ax/bw)^2)/bw, 0)
        		},
        		exponential = if (nn.only) {
        			function(x) {
	            		ax <- abs(x)
   		        		sax <- sort(ax)
       		    		bw <- sax[k] + 1e-06
						ind <- ax < bw
            			weights <- numeric(length(ax))
        				weights[ind] <- 0.5 * exp(-ax[ind]/bw)/bw
        				weights
	            		# ax <- abs(x)
	            		# sax <- unique(sort(ax))
    	        		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
						# ind <- ax < bw
            			# weights <- numeric(length(ax))
        				# weights[ind] <- 0.5 * exp(-ax[ind]/bw)/bw
        				# weights
        				# ax <- abs(x)
        				# ord <- order(ax)
	            		# bw <- ax[ord[k]] + min((ax[ord[k+1]] - ax[ord[k]])/2, 1e-06)
        				# weights <- numeric(length(ord))
        				# weights[ord[1:k]] <- 0.5 * exp(-ax[ord[1:k]]/bw)/bw
        				# weights
        				# ax <- abs(x)
        				# ord <- order(ax)
        				# bw <- ax[ord[k+1]] + .Machine$double.eps
        				# weights <- numeric(length(ord))
        				# weights[ord[1:k]] <- 0.5 * exp(-ax[ord[1:k]]/bw)/bw
        				# weights
        			}
        		} else {
        			function(x) {
	            		ax <- abs(x)
   		        		sax <- sort(ax)
       		    		bw <- sax[k] + 1e-06
        				0.5 * exp(-ax/bw)/bw
	      		  		# ax <- abs(x)
	            		# sax <- unique(sort(ax))
        	    		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
        				# 0.5 * exp(-ax/bw)/bw
            			# ax <- abs(x)
            			# bw <- sort(ax)[k+1] + .Machine$double.eps
        				# 0.5 * exp(-ax/bw)/bw
        			}
        		},
        		gaussian = if (nn.only) { ###
        			function(x) {
	            		ax <- abs(x)
   		        		sax <- sort(ax)
       		    		bw <- sax[k] + 1e-06
						ind <- ax < bw
            			weights <- numeric(length(ax))
    	        		weights[ind] <- dnorm(x[ind], sd = bw)
    	        		weights
	            		# ax <- abs(x)
	            		# sax <- unique(sort(ax))
    	        		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
						# ind <- ax < bw
            			# weights <- numeric(length(ax))
    	        		# weights[ind] <- dnorm(x[ind], sd = bw)
    	        		# weights
        				# ax <- abs(x)
        				# ord <- order(ax)
	            		# bw <- ax[ord[k]] + min((ax[ord[k+1]] - ax[ord[k]])/2, 1e-06)
            			# weights <- numeric(length(ord))
    	        		# weights[ord[1:k]] <- dnorm(x[ord[1:k]], sd = bw)
    	        		# weights
        				# ax <- abs(x)
        				# ord <- order(ax)
        				# bw <- ax[ord[k+1]] + .Machine$double.eps
            			# weights <- numeric(length(ord))
    	        		# weights[ord[1:(k+1)]] <- dnorm(x[ord[1:(k+1)]], sd = bw)
    	        		# weights
        			}
        		} else {
        			function(x) {
	            		ax <- abs(x)
   		        		sax <- sort(ax)
       		    		bw <- sax[k] + 1e-06
        				dnorm(x, sd = bw)
	      		  		# ax <- abs(x)
	            		# sax <- unique(sort(ax))
        	    		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
        				# dnorm(x, sd = bw)
        				# bw <- sort(abs(x))[k+1] + .Machine$double.eps
        				# dnorm(x, sd = bw)
        			}
        		},
        		optcosine = function(x) {
            		ax <- abs(x)
	        		sax <- sort(ax)
   		    		bw <- sax[k] + 1e-06
         	  	 	ifelse(ax < bw, pi/4 * cos(pi * x/(2 * bw))/bw, 0)
      		  		# ax <- abs(x)
            		# sax <- unique(sort(ax))
       	    		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
         	  	 	# ifelse(ax < bw, pi/4 * cos(pi * x/(2 * bw))/bw, 0)
        			# ax <- abs(x)
        			# bw <- sort(ax)[k+1] + .Machine$double.eps
         	  	 	# ifelse(ax < bw, pi/4 * cos(pi * x/(2 * bw))/bw, 0)
         	  	},
        		rectangular = function(x) {
            		ax <- abs(x)
	        		sax <- sort(ax)
   		    		bw <- sax[k] + 1e-06
            		ifelse(ax < bw, 0.5/bw, 0)
      		  		# ax <- abs(x)
            		# sax <- unique(sort(ax))
       	    		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
            		# ifelse(ax < bw, 0.5/bw, 0)
        			# ax <- abs(x)
        			# bw <- sort(ax)[k+1] + .Machine$double.eps
            		# ifelse(ax < bw, 0.5/bw, 0)
            	},
       		 	triangular = function(x) {
            		ax <- abs(x)
	        		sax <- sort(ax)
   		    		bw <- sax[k] + 1e-06
           		 	ifelse(ax < bw, (1 - ax/bw)/bw, 0)
      		  		# ax <- abs(x)
            		# sax <- unique(sort(ax))
       	    		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
           		 	# ifelse(ax < bw, (1 - ax/bw)/bw, 0)
            		# ax <- abs(x)
        			# bw <- sort(ax)[k+1] + .Machine$double.eps
           		 	# ifelse(ax < bw, (1 - ax/bw)/bw, 0)
        		})
        	if (wf %in% c("cauchy", "exponential", "gaussian"))
        		attributes(wfunc) <- list(name = wf, k = k, nn.only = nn.only, adaptive = TRUE)
			else {
        		if (!missing(nn.only))
        			warning("argument 'nn.only' is ignored")
        		attributes(wfunc) <- list(name = wf, k = k, nn.only = TRUE, adaptive = TRUE)
        	}
		}
	} else {				# bw given -> fixed bandwidth
		# checks on bw
		if ((!is.numeric(bw)) || !length(bw))
			stop("'bw' must be numeric of length > 0")
		if (length(bw) > 1) {
			bw <- bw[1]
			warning("only first element of 'bw' used")
		}
		if (bw <= 0)
    		stop("'bw' must be positive")
		if (missing(k)) {	# only bw given -> fixed bandwidth, ignore nn.only
			if (!missing(nn.only))
				warning("argument 'nn.only' is ignored")
			## window functions with fixed bandwidth
			wfunc <- switch(wf,
        		biweight = function(x)
            		ifelse(abs(x) < bw, 15/16 * (1 - (x/bw)^2)^2/bw, 0),
        		cauchy = function(x) 
        			1/(pi * (1 + (x/bw)^2) * bw),
      		  	cosine = function(x)
            		ifelse(abs(x) < bw, (1 + cos(pi * x/bw))/(2 * bw), 0),
       		 	epanechnikov = function(x)
            		ifelse(abs(x) < bw, 3/4 * (1 - (x/bw)^2)/bw, 0),
        		exponential = function(x)
        			0.5 * exp(-abs(x)/bw)/bw,
        		gaussian = function(x) 
        			dnorm(x, sd = bw),
        		optcosine = function(x)
         	  	 	ifelse(abs(x) < bw, pi/4 * cos(pi * x/(2 * bw))/bw, 0),
        		rectangular = function(x)
            		ifelse(abs(x) < bw, 0.5/bw, 0),
       		 	triangular = function(x) {
            		ax <- abs(x)
           		 	ifelse(ax < bw, (1 - ax/bw)/bw, 0)
        		})
        	attributes(wfunc) <- list(name = wf, bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			if (!missing(nn.only))
				if (!nn.only)
					stop("if 'bw' and 'k' are given argument 'nn.only' must be TRUE")
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
			if (k > n)
				stop("'k' is larger than 'n'")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
    		wfunc <- switch(wf,
        		biweight = function(x) {
            		ax <- abs(x)
	        		sax <- sort(ax)
   		    		knnbw <- sax[k] + 1e-06
					ind <- ax < knnbw
            		weights <- numeric(length(ax))
    	        	weights[ind] <- ifelse(ax[ind] < bw, 15/16 * (1 - (ax[ind]/bw)^2)^2/bw, 0)
        	    	weights
	            	# ax <- abs(x)
	            	# sax <- unique(sort(ax))
    	        	# knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- ifelse(ax[ind] < bw, 15/16 * (1 - (ax[ind]/bw)^2)^2/bw, 0)
        	    	# weights
	            	# ax <- abs(x)
            		# ord <- order(ax)
            		# weights <- numeric(length(ord))
    	        	# weights[ord[1:k]] <- ifelse(ax[ord[1:k]] < bw, 15/16 * (1 - (ax[ord[1:k]]/bw)^2)^2/bw, 0)
        	    	# weights
        		},
        		cauchy = function(x) {
            		ax <- x^2
	        		sax <- sort(ax)
   		    		knnbw <- sax[k] + 1e-06
					ind <- ax < knnbw
            		weights <- numeric(length(ax))
    	        	weights[ind] <- 1/(pi * (1 + ax[ind]/bw^2) * bw)
        	    	weights
	            	# ax <- x^2
	            	# sax <- unique(sort(ax))
    	        	# knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- 1/(pi * (1 + ax[ind]/bw^2) * bw)
        	    	# weights
        			# qx <- x^2
            		# ord <- order(qx)
            		# weights <- numeric(length(ord))
    	        	# weights[ord[1:k]] <- 1/(pi * (1 + qx[ord[1:k]]/bw^2) * bw)
        	    	# weights
        		},
      		  	cosine = function(x) {
            		ax <- abs(x)
	        		sax <- sort(ax)
   		    		knnbw <- sax[k] + 1e-06
					ind <- ax < knnbw
            		weights <- numeric(length(ax))
    	        	weights[ind] <- ifelse(ax[ind] < bw, (1 + cos(pi * x[ind]/bw))/(2 * bw), 0)
        	    	weights
	            	# ax <- abs(x)
	            	# sax <- unique(sort(ax))
    	        	# knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- ifelse(ax[ind] < bw, (1 + cos(pi * x[ind]/bw))/(2 * bw), 0)
        	    	# weights
      		  		# ax <- abs(x)
            		# ord <- order(ax)
            		# weights <- numeric(length(ord))
    	        	# weights[ord[1:k]] <- ifelse(ax[ord[1:k]] < bw, (1 + cos(pi * x[ord[1:k]]/bw))/(2 * bw), 0)
        	    	# weights
            	},
       		 	epanechnikov = function(x) {
            		ax <- abs(x)
	        		sax <- sort(ax)
   		    		knnbw <- sax[k] + 1e-06
					ind <- ax < knnbw
            		weights <- numeric(length(ax))
    	        	weights[ind] <- ifelse(ax[ind] < bw, 3/4 * (1 - (ax[ind]/bw)^2)/bw, 0)
        	    	weights
	            	# ax <- abs(x)
	            	# sax <- unique(sort(ax))
    	        	# knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- ifelse(ax[ind] < bw, 3/4 * (1 - (ax[ind]/bw)^2)/bw, 0)
        	    	# weights
            		# ax <- abs(x)
       		 		# ord <- order(ax)
            		# weights <- numeric(length(ord))
    	        	# weights[ord[1:k]] <- ifelse(ax[ord[1:k]] < bw, 3/4 * (1 - (ax[ord[1:k]]/bw)^2)/bw, 0)
        	    	# weights
        		},
        		exponential = function(x) {
            		ax <- abs(x)
	        		sax <- sort(ax)
   		    		knnbw <- sax[k] + 1e-06
					ind <- ax < knnbw
            		weights <- numeric(length(ax))
    	        	weights[ind] <- 0.5 * exp(-ax[ind]/bw)/bw
        	    	weights
	            	# ax <- abs(x)
	            	# sax <- unique(sort(ax))
    	        	# knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- 0.5 * exp(-ax[ind]/bw)/bw
        	    	# weights
        			# ax <- abs(x)
       		 		# ord <- order(ax)
            		# weights <- numeric(length(ord))
    	        	# weights[ord[1:k]] <- 0.5 * exp(-ax[ord[1:k]]/bw)/bw
        	    	# weights
        		},
        		gaussian = function(x) {
            		ax <- abs(x)
	        		sax <- sort(ax)
   		    		knnbw <- sax[k] + 1e-06
					ind <- ax < knnbw
            		weights <- numeric(length(ax))
    	        	weights[ind] <- dnorm(x[ind], sd = bw)
        	    	weights
	            	# ax <- abs(x)
	            	# sax <- unique(sort(ax))
    	        	# knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- dnorm(x[ind], sd = bw)
        	    	# weights
       		 		# ord <- order(abs(x))
            		# weights <- numeric(length(ord))
    	        	# weights[ord[1:k]] <- dnorm(x[ord[1:k]], sd = bw)
        	    	# weights
        		},
        		optcosine = function(x) {
            		ax <- abs(x)
	        		sax <- sort(ax)
   		    		knnbw <- sax[k] + 1e-06
					ind <- ax < knnbw
            		weights <- numeric(length(ax))
    	        	weights[ind] <- ifelse(ax[ind] < bw, pi/4 * cos(pi * x[ind]/(2 * bw))/bw, 0)
        	    	weights
	            	# ax <- abs(x)
	            	# sax <- unique(sort(ax))
    	        	# knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- ifelse(ax[ind] < bw, pi/4 * cos(pi * x[ind]/(2 * bw))/bw, 0)
        	    	# weights
        			# ax <- abs(x)
       		 		# ord <- order(ax)
            		# weights <- numeric(length(ord))
    	        	# weights[ord[1:k]] <- ifelse(ax[ord[1:k]] < bw, pi/4 * cos(pi * x[ord[1:k]]/(2 * bw))/bw, 0)
        	    	# weights
         	  	},
        		rectangular = function(x) {
            		ax <- abs(x)
	        		sax <- sort(ax)
   		    		knnbw <- sax[k] + 1e-06
					ind <- ax < knnbw
            		weights <- numeric(length(ax))
    	        	weights[ind] <- ifelse(ax[ind] < bw, 0.5/bw, 0)
        	    	weights
	            	# ax <- abs(x)
	            	# sax <- unique(sort(ax))
    	        	# knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- ifelse(ax[ind] < bw, 0.5/bw, 0)
        	    	# weights
        			# ax <- abs(x)
       		 		# ord <- order(ax)
            		# weights <- numeric(length(ord))
    	        	# weights[ord[1:k]] <- ifelse(ax[ord[1:k]] < bw, 0.5/bw, 0)
        	    	# weights
            	},
       		 	triangular = function(x) {
            		ax <- abs(x)
	        		sax <- sort(ax)
   		    		knnbw <- sax[k] + 1e-06
					ind <- ax < knnbw
            		weights <- numeric(length(ax))
    	        	weights[ind] <- ifelse(ax[ind] < bw, (1 - ax[ind]/bw)/bw, 0)
        	    	weights
	            	# ax <- abs(x)
	            	# sax <- unique(sort(ax))
    	        	# knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
					# ind <- ax < knnbw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- ifelse(ax[ind] < bw, (1 - ax[ind]/bw)/bw, 0)
        	    	# weights
            		# ax <- abs(x)
       		 		# ord <- order(ax)
            		# weights <- numeric(length(ord))
    	        	# weights[ord[1:k]] <- ifelse(ax[ord[1:k]] < bw, (1 - ax[ord[1:k]]/bw)/bw, 0)
        	    	# weights
        		})
        		attributes(wfunc) <- list(name = wf, bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(wfunc)
}

#' The window function generates functions that are used in various local classification methods.
#'
#' The window function generates functions are used to initialize a window function. These functions can be passed as
#' \code{wf} argument to various local classification methods.
#'
#' If only \code{bw} is given a window function with fixed bandwidth is returned.
#'
#' If only \code{k} is given a window function with \code{k} nearest neighbors bandwidth, i.e. adaptive to the local density of data points, is generated.
#' In case of window functions with infinite support, \code{"cauchy"}, \code{"exponential"} and \code{"gaussian"}, the argument \code{nn.only} is used to decide 
#' if only the \code{k} nearest neighbors or all observations receive positive weights.
#'
#' If \code{bw} and \code{k} are both specified, a window function with fixed bandwidth is generated and all weights are set to zero except for
#' the \code{k} nearest neighbors.
#' 
#' Parts of the source code are based on the function \link[stats]{density} in package \pkg{stats}. Concerning the \code{"cosine"} and \code{"optcosine"} windows, it applies
#' what is said in the documentation of \link[stats]{density}: \code{"cosine"} is smoother than \code{"optcosine"}, which is the usual 'cosine' kernel in the literature.
#' \code{"cosine"} is the version used by the S programming language.
#'
#' @title Generation of Window Functions
#'
#' @param bw The bandwidth parameter.
#' @param k The number of nearest neighbors.
#' @param nn.only (Logical. Only required for window functions with infinite support.) Should only the k nearest neighbors or all observations receive positive weights? Defaults to \code{TRUE}.
#'
#' @return Returns an object of class \code{"function"}. The resulting \code{function} implements the desired window function and depends on one
#' argument \code{x} that is usually some sort of distance.
#' The returned function has several attributes, depending on which arguments are specified.
#' \describe{
#'   \item{\code{"name"}}{The name of the window function.}
#'   \item{\code{"bw"}}{(If corresponding argument is given.) The chosen bandwidth.}
#' 	 \item{\code{"k"}}{(If corresponding argument is given.) The chosen number of nearest neighbors.}
#'   \item{\code{"nn.only"}}{(Logical. Only if \code{k} was specified.) \code{TRUE} if only the k nearest neighbors are used. 
#'     (\code{nn.only} is always \code{TRUE} except for window functions with infinite support.)}
#'   \item{\code{"adaptive"}}{(Logical.) \code{TRUE} in case of an adaptive bandwidth, \code{FALSE} if the bandwidth is fixed.}
#'	}
#'
#'
#' @seealso Documentation for various local classification methods, e.g. \code{\link{dalda}} or \code{\link{oslda}}, and \link[stats]{density}.
#'
#' @examples
#' ## fixed bandwidth
#' gwf <- gaussian(bw = 1)
#' gwf
#'
#' ## adaptive bandwidth, only the 100 nearest neighbors receive positive weights
#' gwf <- gaussian(k = 100)
#' gwf
#' gwf(1:150)
#'
#' ## adaptive bandwidth, all observations have positive weights
#' gwf <- gaussian(k = 100, nn.only = FALSE)
#' gwf
#' gwf(1:150)
#' 
#' ## fixed bandwidth, only the 100 nearest neighbors get positive weights
#' gwf <- gaussian(k = 100, bw = 1)
#' gwf
#' gwf(1:150)
#'
#' @rdname wfs
#'
#' @aliases wfs biweight cauchy cosine epanechnikov exponential gaussian optcosine rectangular triangular
#'
#' @export biweight cauchy cosine epanechnikov exponential gaussian optcosine rectangular triangular
#'

biweight <- function(bw, k) {
	if (missing(bw)) {		# bw missing
		if (missing(k))		# bw and k missing
			stop("either 'bw' or 'k' have to be specified")		
		else {				# only k given -> adaptive bandwidth
			# checks on k
			if ((!is.numeric(k)) || !length(k))
       			stop("'k' must be numeric of length > 0")
       		if (length(k) > 1) {
       			k <- k[1]
       			warning("only first element of 'k' used")
      	 	}
			if (k <= 0)
   		 		stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
      		bi <- function(x) {
            	ax <- abs(x)
            	sax <- sort(ax)
            	bw <- sax[k] + 1e-06
        		ifelse(ax < bw, 15/16 * (1 - (ax/bw)^2)^2/bw, 0)
           		# ax <- abs(x)
           		# sax <- unique(sort(ax))
           		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
       			# ifelse(ax < bw, 15/16 * (1 - (ax/bw)^2)^2/bw, 0)
       			# ax <- abs(x)
       			# bw <- sort(ax)[k+1] + .Machine$double.eps
   				# ifelse(ax < bw, 15/16 * (1 - (ax/bw)^2)^2/bw, 0)  
   			}
		    attributes(bi) <- list(name = "biweight", k = k, nn.only = TRUE, adaptive = TRUE)
		}
	} else {				# bw given -> fixed bandwidth
		# checks on bw
		if ((!is.numeric(bw)) || !length(bw))
			stop("'bw' must be numeric of length > 0")
		if (length(bw) > 1) {
			bw <- bw[1]
			warning("only first element of 'bw' used")
		}
		if (bw <= 0)
    		stop("'bw' must be positive")
		if (missing(k)) {	# only bw given -> fixed bandwidth, ignore nn.only
			## window functions with fixed bandwidth
			bi = function(x)
            	ifelse(abs(x) < bw, 15/16 * (1 - (x/bw)^2)^2/bw, 0)
		    attributes(bi) <- list(name = "biweight", bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
        	bi <- function(x) {
            	ax <- abs(x)
            	sax <- sort(ax)
            	knnbw <- sax[k] + 1e-06
				ind <- ax < knnbw
            	weights <- numeric(length(ax))
    	        weights[ind] <- ifelse(ax[ind] < bw, 15/16 * (1 - (ax[ind]/bw)^2)^2/bw, 0)
        	    weights
	            # ax <- abs(x)
	            # sax <- unique(sort(ax))
    	        # knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
				# ind <- ax < knnbw
            	# weights <- numeric(length(ax))
    	        # weights[ind] <- ifelse(ax[ind] < bw, 15/16 * (1 - (ax[ind]/bw)^2)^2/bw, 0)
        	    # weights
	            # ax <- abs(x)
            	# ord <- order(ax)
            	# weights <- numeric(length(ord))
    	        # weights[ord[1:k]] <- ifelse(ax[ord[1:k]] < bw, 15/16 * (1 - (ax[ord[1:k]]/bw)^2)^2/bw, 0)
        	    # weights
        	}
        	attributes(bi) <- list(name = "biweight", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(bi)
}


# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
# @param nn.only (Logical. Only required for window functions with infinite support.) Should only the k nearest neighbors or all observations receive positive weights? Defaults to \code{TRUE}.
#'
#' @rdname wfs

cauchy <- function(bw, k, nn.only = TRUE) {
	if (missing(bw)) {		# bw missing
		if (missing(k))		# bw and k missing
			stop("either 'bw' or 'k' have to be specified")		
		else {				# only k given -> adaptive bandwidth
			# checks on k
			if ((!is.numeric(k)) || !length(k))
       			stop("'k' must be numeric of length > 0")
       		if (length(k) > 1) {
       			k <- k[1]
       			warning("only first element of 'k' used")
      	 	}
			if (k <= 0)
   		 		stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
			# window functions with infinite support are cut depending on nn.only
        	if (nn.only) {
        		cau <- function(x) {
	            	ax <- abs(x)
    	        	sax <- sort(ax)
        	    	bw <- sax[k] + 1e-06
					ind <- ax < bw
        	    	weights <- numeric(length(ax))
   	        		weights[ind] <- 1/(pi * (1 + (ax[ind]/bw)^2) * bw)
       	    		weights
		            # ax <- abs(x)
		            # sax <- unique(sort(ax))
   		 	        # bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
					# ind <- ax < bw
        	    	# weights <- numeric(length(ax))
   	        		# weights[ind] <- 1/(pi * (1 + (ax[ind]/bw)^2) * bw)
       	    		# weights
            		# ax <- abs(x)
           			# ord <- order(ax)
       	    		# bw <- ax[ord[k]] + min((ax[ord[k+1]] - ax[ord[k]])/2, 1e-06)
           			# weights <- numeric(length(ord))
   	        		# weights[ord[1:k]] <- 1/(pi * (1 + (ax[ord[1:k]]/bw)^2) * bw)
       	    		# weights
        			# ax <- abs(x)
        			# ord <- order(ax)
        			# bw <- ax[ord[k+1]] + .Machine$double.eps
        			# weights <- numeric(length(ord))
        			# weights[ord[1:k]] <- 1/(pi * (1 + (ax[ord[1:k]]/bw)^2) * bw)
        			# weights
        		}
        	} else {
        		cau <- function(x) {
	            	ax <- abs(x)
    	        	sax <- sort(ax)
        	    	bw <- sax[k] + 1e-06
					1/(pi * (1 + (ax/bw)^2) * bw)
	            	# ax <- abs(x)
            		# sax <- unique(sort(ax))
	            	# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
					# 1/(pi * (1 + (ax/bw)^2) * bw)
  					# ax <- abs(x)
       				# bw <- sort(ax)[k+1] + .Machine$double.eps
					# 1/(pi * (1 + (ax/bw)^2) * bw)
        		}
        	}
			attributes(cau) <- list(name = "cauchy", k = k, nn.only = nn.only, adaptive = TRUE)
		}
	} else {				# bw given -> fixed bandwidth
		# checks on bw
		if ((!is.numeric(bw)) || !length(bw))
			stop("'bw' must be numeric of length > 0")
		if (length(bw) > 1) {
			bw <- bw[1]
			warning("only first element of 'bw' used")
		}
		if (bw <= 0)
    		stop("'bw' must be positive")
		if (missing(k)) {	# only bw given -> fixed bandwidth, ignore nn.only
			## window functions with fixed bandwidth
			cau <- function(x)
				1/(pi * (1 + (x/bw)^2) * bw)
			attributes(cau) <- list(name = "cauchy", bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
			cau <- function(x) { 
            	ax <- abs(x)
   	        	sax <- sort(ax)
       	    	knnbw <- sax[k] + 1e-06
				ind <- ax < knnbw
       	    	weights <- numeric(length(ax))
	       		weights[ind] <- 1/(pi * (1 + (ax[ind]/bw)^2) * bw)
        		weights
	            # ax <- abs(x)
	            # sax <- unique(sort(ax))
				# knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
				# ind <- ax < knnbw
       	    	# weights <- numeric(length(ax))
	       		# weights[ind] <- 1/(pi * (1 + (ax[ind]/bw)^2) * bw)
        		# weights
				# ax <- abs(x)
            	# ord <- order(ax)
            	# weights <- numeric(length(ord))
				# weights[ord[1:k]] <- 1/(pi * (1 + (ax[ord[1:k]]/bw)^2) * bw)
				# weights
			}
			attributes(cau) <- list(name = "cauchy", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(cau)
}


# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
#
#' @rdname wfs

cosine <- function(bw, k) {
	if (missing(bw)) {		# bw missing
		if (missing(k))		# bw and k missing
			stop("either 'bw' or 'k' have to be specified")	
		else {				# only k given -> adaptive bandwidth
			# checks on k
			if ((!is.numeric(k)) || !length(k))
       			stop("'k' must be numeric of length > 0")
       		if (length(k) > 1) {
       			k <- k[1]
       			warning("only first element of 'k' used")
      	 	}
			if (k <= 0)
   		 		stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
    		cosi <- function(x) {
            	ax <- abs(x)
   	        	sax <- sort(ax)
       	    	bw <- sax[k] + 1e-06
            	ifelse(ax < bw, (1 + cos(pi * ax/bw))/(2 * bw), 0)
				# ax <- abs(x)
           		# sax <- unique(sort(ax))
            	# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
            	# ifelse(ax < bw, (1 + cos(pi * ax/bw))/(2 * bw), 0)
				# ax <- abs(x)
       			# bw <- sort(ax)[k+1] + .Machine$double.eps
        		# ifelse(ax < bw, (1 + cos(pi * ax/bw))/(2 * bw), 0)
    		}    
    		attributes(cosi) <- list(name = "cosine", k = k, nn.only = TRUE, adaptive = TRUE)
		}
	} else {				# bw given -> fixed bandwidth
		# checks on bw
		if ((!is.numeric(bw)) || !length(bw))
			stop("'bw' must be numeric of length > 0")
		if (length(bw) > 1) {
			bw <- bw[1]
			warning("only first element of 'bw' used")
		}
		if (bw <= 0)
    		stop("'bw' must be positive")
		if (missing(k)) {	# only bw given -> fixed bandwidth, ignore nn.only
			## window functions with fixed bandwidth
    		cosi <- function(x)
        		ifelse(abs(x) < bw, (1 + cos(pi * x/bw))/(2 * bw), 0)
    		attributes(cosi) <- list(name = "cosine", bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
    		cosi <- function(x) {
            	ax <- abs(x)
   	        	sax <- sort(ax)
       	    	knnbw <- sax[k] + 1e-06
				ind <- ax < knnbw
       	    	weights <- numeric(length(ax))
        		weights[ind] <- ifelse(ax[ind] < bw, (1 + cos(pi * x[ind]/bw))/(2 * bw), 0)
        		weights
	            # ax <- abs(x)
	            # sax <- unique(sort(ax))
				# knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
				# ind <- ax < knnbw
       	    	# weights <- numeric(length(ax))
        		# weights[ind] <- ifelse(ax[ind] < bw, (1 + cos(pi * x[ind]/bw))/(2 * bw), 0)
        		# weights
				# ax <- abs(x)
            	# ord <- order(ax)
            	# weights <- numeric(length(ord))
        		# weights[ord[1:k]] <- ifelse(ax[ord[1:k]] < bw, (1 + cos(pi * x[ord[1:k]]/bw))/(2 * bw), 0)
        		# weights
    		}    
    		attributes(cosi) <- list(name = "cosine", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(cosi)
}


# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
#
#' @rdname wfs

epanechnikov <- function(bw, k) {
	if (missing(bw)) {		# bw missing
		if (missing(k))		# bw and k missing
			stop("either 'bw' or 'k' have to be specified")	
		else {				# only k given -> adaptive bandwidth
			# checks on k
			if ((!is.numeric(k)) || !length(k))
       			stop("'k' must be numeric of length > 0")
       		if (length(k) > 1) {
       			k <- k[1]
       			warning("only first element of 'k' used")
      	 	}
			if (k <= 0)
   		 		stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
    		epan <- function(x) {
            	ax <- abs(x)
   	        	sax <- sort(ax)
       	    	bw <- sax[k] + 1e-06
        		ifelse(ax < bw, 3/4 * (1 - (ax/bw)^2)/bw, 0)
  		  		# ax <- abs(x)
         		# sax <- unique(sort(ax))
        		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
        		# ifelse(ax < bw, 3/4 * (1 - (ax/bw)^2)/bw, 0)
        		# ax <- abs(x)
       			# bw <- sort(ax)[k+1] + .Machine$double.eps
        		# ifelse(ax < bw, 3/4 * (1 - (ax/bw)^2)/bw, 0)
    		}
    		attributes(epan) <- list(name = "epanechnikov", k = k, nn.only = TRUE, adaptive = TRUE)
		}
	} else {				# bw given -> fixed bandwidth
		# checks on bw
		if ((!is.numeric(bw)) || !length(bw))
			stop("'bw' must be numeric of length > 0")
		if (length(bw) > 1) {
			bw <- bw[1]
			warning("only first element of 'bw' used")
		}
		if (bw <= 0)
    		stop("'bw' must be positive")
		if (missing(k)) {	# only bw given -> fixed bandwidth, ignore nn.only
			## window functions with fixed bandwidth
    		epan <- function(x)
        		ifelse(abs(x) < bw, 3/4 * (1 - (x/bw)^2)/bw, 0)
    		attributes(epan) <- list(name = "epanechnikov", bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
    		epan <- function(x) {
            	ax <- abs(x)
   	        	sax <- sort(ax)
       	    	knnbw <- sax[k] + 1e-06
				ind <- ax < knnbw
       	    	weights <- numeric(length(ax))
        		weights[ind] <- ifelse(ax[ind] < bw, 3/4 * (1 - (ax[ind]/bw)^2)/bw, 0)
        		weights
	            # ax <- abs(x)
	            # sax <- unique(sort(ax))
				# knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
				# ind <- ax < knnbw
       	    	# weights <- numeric(length(ax))
        		# weights[ind] <- ifelse(ax[ind] < bw, 3/4 * (1 - (ax[ind]/bw)^2)/bw, 0)
        		# weights
        		# ax <- abs(x)
            	# ord <- order(ax)
            	# weights <- numeric(length(ord))
        		# weights[ord[1:k]] <- ifelse(ax[ord[1:k]] < bw, 3/4 * (1 - (ax[ord[1:k]]/bw)^2)/bw, 0)
        		# weights
    		}
    		attributes(epan) <- list(name = "epanechnikov", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(epan)
}


# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
# @param nn.only (Logical. Only required for window functions with infinite support.) Should only the k nearest neighbors or all observations receive positive weights? Defaults to \code{TRUE}.
#
#' @rdname wfs

exponential <- function(bw, k, nn.only = TRUE) {
	if (missing(bw)) {		# bw missing
		if (missing(k))		# bw and k missing
			stop("either 'bw' or 'k' have to be specified")	
		else {				# only k given -> adaptive bandwidth
			# checks on k
			if ((!is.numeric(k)) || !length(k))
       			stop("'k' must be numeric of length > 0")
       		if (length(k) > 1) {
       			k <- k[1]
       			warning("only first element of 'k' used")
      	 	}
			if (k <= 0)
   		 		stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
			# window functions with infinite support are cut depending on nn.only
        	if (nn.only) {
        		expo <- function(x) {
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	bw <- sax[k] + 1e-06
					ind <- ax < bw
            		weights <- numeric(length(ax))
        			weights[ind] <- 0.5 * exp(-ax[ind]/bw)/bw
        			weights
	            	# ax <- abs(x)
	            	# sax <- unique(sort(ax))
    	        	# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
					# ind <- ax < bw
            		# weights <- numeric(length(ax))
        			# weights[ind] <- 0.5 * exp(-ax[ind]/bw)/bw
        			# weights
       				# ax <- abs(x)
       				# ord <- order(ax)
            		# bw <- ax[ord[k]] + min((ax[ord[k+1]] - ax[ord[k]])/2, 1e-06)
       				# weights <- numeric(length(ord))
       				# weights[ord[1:k]] <- 0.5 * exp(-ax[ord[1:k]]/bw)/bw
       				# weights
        			# ax <- abs(x)
        			# ord <- order(ax)
        			# bw <- ax[ord[k+1]] + .Machine$double.eps
        			# weights <- numeric(length(ord))
        			# weights[ord[1:k]] <- 0.5 * exp(-ax[ord[1:k]]/bw)/bw
        			# weights
        		}
        	} else {
        		expo <- function(x) {
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	bw <- sax[k] + 1e-06
       				0.5 * exp(-ax/bw)/bw
      		  		# ax <- abs(x)
            		# sax <- unique(sort(ax))
       	    		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
       				# 0.5 * exp(-ax/bw)/bw
            		# ax <- abs(x)
            		# bw <- sort(ax)[k+1] + .Machine$double.eps
        			# 0.5 * exp(-ax/bw)/bw
        		}
        	}
			attributes(expo) <- list(name = "exponential", k = k, nn.only = nn.only, adaptive = TRUE)
		}
	} else {				# bw given -> fixed bandwidth
		# checks on bw
		if ((!is.numeric(bw)) || !length(bw))
			stop("'bw' must be numeric of length > 0")
		if (length(bw) > 1) {
			bw <- bw[1]
			warning("only first element of 'bw' used")
		}
		if (bw <= 0)
    		stop("'bw' must be positive")
		if (missing(k)) {	# only bw given -> fixed bandwidth, ignore nn.only
			if (!missing(nn.only))
				warning("argument 'nn.only' is ignored")
			## window functions with fixed bandwidth
			expo <- function(x)
        		0.5 * exp(-abs(x)/bw)/bw
			attributes(expo) <- list(name = "exponential", bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			if (!missing(nn.only))
				if (!nn.only)
					stop("if 'bw' and 'k' are given argument 'nn.only' must be TRUE")
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
			expo <- function(x) {
	            ax <- abs(x)
   		        sax <- sort(ax)
       		    knnbw <- sax[k] + 1e-06
				ind <- ax < knnbw
           		weights <- numeric(length(ax))
        		weights[ind] <- 0.5 * exp(-ax[ind]/bw)/bw
        		weights	
            	# ax <- abs(x)
            	# sax <- unique(sort(ax))
   	        	# knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
				# ind <- ax < knnbw
           		# weights <- numeric(length(ax))
        		# weights[ind] <- 0.5 * exp(-ax[ind]/bw)/bw
        		# weights	
        		# ax <- abs(x)
            	# ord <- order(ax)
            	# weights <- numeric(length(ord))
        		# weights[ord[1:k]] <- 0.5 * exp(-ax[ord[1:k]]/bw)/bw
        		# weights	
			}
			attributes(expo) <- list(name = "exponential", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(expo)
}


# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
# @param nn.only (Logical. Only required for window functions with infinite support.) Should only the k nearest neighbors or all observations receive positive weights? Defaults to \code{TRUE}.
#
#' @rdname wfs

gaussian <- function(bw, k, nn.only = TRUE) {
	if (missing(bw)) {		# bw missing
		if (missing(k))		# bw and k missing
			stop("either 'bw' or 'k' have to be specified")	
		else {				# only k given -> adaptive bandwidth
			# checks on k
			if ((!is.numeric(k)) || !length(k))
       			stop("'k' must be numeric of length > 0")
       		if (length(k) > 1) {
       			k <- k[1]
       			warning("only first element of 'k' used")
      	 	}
			if (k <= 0)
   		 		stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
			# window functions with infinite support are cut depending on nn.only
        	if (nn.only) {
        		gauss <- function(x) {###
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	bw <- sax[k] + 1e-06
					ind <- ax < bw
            		weights <- numeric(length(ax))
    	        	weights[ind] <- dnorm(x[ind], sd = bw)
    	        	weights
	            	# ax <- abs(x)
	            	# sax <- unique(sort(ax))
    	        	# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
					# ind <- ax < bw
            		# weights <- numeric(length(ax))
    	        	# weights[ind] <- dnorm(x[ind], sd = bw)
    	        	# weights
        			# ax <- abs(x)
        			# ord <- order(ax)
	            	# bw <- ax[ord[k]] + min((ax[ord[k+1]] - ax[ord[k]])/2, 1e-06)
            		# weights <- numeric(length(ord))
    	        	# weights[ord[1:k]] <- dnorm(x[ord[1:k]], sd = bw)
    	        	# weights
        			# ax <- abs(x)
        			# ord <- order(ax)
        			# bw <- ax[ord[k+1]] + .Machine$double.eps
            		# weights <- numeric(length(ord))
    	        	# weights[ord[1:k]] <- dnorm(x[ord[1:k]], sd = bw)
        	    	# weights
        		}
        	} else {
        		gauss <- function(x) {
	            	ax <- abs(x)
   		        	sax <- sort(ax)
       		    	bw <- sax[k] + 1e-06
       				dnorm(x, sd = bw)
      		  		# ax <- abs(x)
            		# sax <- unique(sort(ax))
       	    		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
       				# dnorm(x, sd = bw)
        			# bw <- sort(abs(x))[k+1] + .Machine$double.eps
        			# dnorm(x, sd = bw)
        		}
        	}
    		attributes(gauss) <- list(name = "gaussian", k = k, nn.only = nn.only, adaptive = TRUE)
		}
	} else {				# bw given -> fixed bandwidth
		# checks on bw
		if ((!is.numeric(bw)) || !length(bw))
			stop("'bw' must be numeric of length > 0")
		if (length(bw) > 1) {
			bw <- bw[1]
			warning("only first element of 'bw' used")
		}
		if (bw <= 0)
    		stop("'bw' must be positive")
		if (missing(k)) {	# only bw given -> fixed bandwidth, ignore nn.only
			if (!missing(nn.only))
				warning("argument 'nn.only' is ignored")
			## window functions with fixed bandwidth
    		gauss <- function(x)
    			dnorm(x, sd = bw)
			attributes(gauss) <- list(name = "gaussian", bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			if (!missing(nn.only))
				if (!nn.only)
					stop("if 'bw' and 'k' are given argument 'nn.only' must be TRUE")
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
        	gauss <- function(x) {
            	ax <- abs(x)
	        	sax <- sort(ax)
   		    	knnbw <- sax[k] + 1e-06
				ind <- ax < knnbw
            	weights <- numeric(length(ax))
    	        weights[ind] <- dnorm(x[ind], sd = bw)
        	    weights
	            # ax <- abs(x)
	            # sax <- unique(sort(ax))
    	        # knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
				# ind <- ax < knnbw
            	# weights <- numeric(length(ax))
    	        # weights[ind] <- dnorm(x[ind], sd = bw)
        	    # weights
       		 	# ord <- order(abs(x))
            	# weights <- numeric(length(ord))
    	        # weights[ord[1:k]] <- dnorm(x[ord[1:k]], sd = bw)
        	    # weights
        	}
			attributes(gauss) <- list(name = "gaussian", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(gauss)
}


# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
#
#' @rdname wfs

optcosine <- function(bw, k) {
	if (missing(bw)) {		# bw missing
		if (missing(k))		# bw and k missing
			stop("either 'bw' or 'k' have to be specified")	
		else {				# only k given -> adaptive bandwidth
			# checks on k
			if ((!is.numeric(k)) || !length(k))
       			stop("'k' must be numeric of length > 0")
       		if (length(k) > 1) {
       			k <- k[1]
       			warning("only first element of 'k' used")
      	 	}
			if (k <= 0)
   		 		stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
		    optcos <- function(x) {
            	ax <- abs(x)
	        	sax <- sort(ax)
   		    	bw <- sax[k] + 1e-06
          	 	ifelse(ax < bw, pi/4 * cos(pi * x/(2 * bw))/bw, 0)
  		  		# ax <- abs(x)
         		# sax <- unique(sort(ax))
       			# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
          	 	# ifelse(ax < bw, pi/4 * cos(pi * x/(2 * bw))/bw, 0)
		    	# ax <- abs(x)
		    	# bw <- sort(ax)[k+1] + .Machine$double.eps
        		# ifelse(ax < bw, pi/4 * cos(pi * ax/(2 * bw))/bw, 0)
    		}    
    		attributes(optcos) <- list(name = "optcosine", k = k, nn.only = TRUE, adaptive = TRUE)
		}
	} else {				# bw given -> fixed bandwidth
		# checks on bw
		if ((!is.numeric(bw)) || !length(bw))
			stop("'bw' must be numeric of length > 0")
		if (length(bw) > 1) {
			bw <- bw[1]
			warning("only first element of 'bw' used")
		}
		if (bw <= 0)
    		stop("'bw' must be positive")
		if (missing(k)) {	# only bw given -> fixed bandwidth, ignore nn.only
			## window functions with fixed bandwidth
		    optcos <- function(x)
        		ifelse(abs(x) < bw, pi/4 * cos(pi * x/(2 * bw))/bw, 0) 
    		attributes(optcos) <- list(name = "optcosine", bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
		    optcos <- function(x) {
            	ax <- abs(x)
	        	sax <- sort(ax)
   		    	knnbw <- sax[k] + 1e-06
				ind <- ax < knnbw
            	weights <- numeric(length(ax))
        		weights[ind] <- ifelse(ax[ind] < bw, pi/4 * cos(pi * x[ind]/(2 * bw))/bw, 0)
        		weights
	            # ax <- abs(x)
	            # sax <- unique(sort(ax))
    	        # knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
				# ind <- ax < knnbw
            	# weights <- numeric(length(ax))
        		# weights[ind] <- ifelse(ax[ind] < bw, pi/4 * cos(pi * x[ind]/(2 * bw))/bw, 0)
        		# weights
		    	# ax <- abs(x)
       		 	# ord <- order(ax)
            	# weights <- numeric(length(ord))
        		# weights[ord[1:k]] <- ifelse(ax[ord[1:k]] < bw, pi/4 * cos(pi * x[ord[1:k]]/(2 * bw))/bw, 0)
        		# weights
    		}    
    		attributes(optcos) <- list(name = "optcosine", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(optcos)
}


# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
#
#' @rdname wfs

rectangular <- function(bw, k) {
	if (missing(bw)) {		# bw missing
		if (missing(k))		# bw and k missing
			stop("either 'bw' or 'k' have to be specified")		
		else {				# only k given -> adaptive bandwidth
			# checks on k
			if ((!is.numeric(k)) || !length(k))
       			stop("'k' must be numeric of length > 0")
       		if (length(k) > 1) {
       			k <- k[1]
       			warning("only first element of 'k' used")
      	 	}
			if (k <= 0)
   		 		stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
    		rect <- function(x) {
            	ax <- abs(x)
	        	sax <- sort(ax)
   		    	bw <- sax[k] + 1e-06
        		ifelse(ax < bw, 0.5/bw, 0)
  		  		# ax <- abs(x)
         		# sax <- unique(sort(ax))
   	    		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
        		# ifelse(ax < bw, 0.5/bw, 0)
		    	# ax <- abs(x)
		    	# bw <- sort(ax)[k+1] + .Machine$double.eps
        		# ifelse(ax < bw, 0.5/bw, 0)
    		}    
    		attributes(rect) <- list(name = "rectangular", k = k, nn.only = TRUE, adaptive = TRUE)
		}
	} else {				# bw given -> fixed bandwidth
		# checks on bw
		if ((!is.numeric(bw)) || !length(bw))
			stop("'bw' must be numeric of length > 0")
		if (length(bw) > 1) {
			bw <- bw[1]
			warning("only first element of 'bw' used")
		}
		if (bw <= 0)
    		stop("'bw' must be positive")
		if (missing(k)) {	# only bw given -> fixed bandwidth, ignore nn.only
			## window functions with fixed bandwidth
    		rect <- function(x)
        		ifelse(abs(x) < bw, 0.5/bw, 0)
    		attributes(rect) <- list(name = "rectangular", bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
    		rect <- function(x) {
            	ax <- abs(x)
	        	sax <- sort(ax)
   		    	knnbw <- sax[k] + 1e-06
				ind <- ax < knnbw
            	weights <- numeric(length(ax))
        		weights[ind] <- ifelse(ax[ind] < bw, 0.5/bw, 0)
        		weights
	            # ax <- abs(x)
	            # sax <- unique(sort(ax))
    	        # knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
				# ind <- ax < knnbw
            	# weights <- numeric(length(ax))
        		# weights[ind] <- ifelse(ax[ind] < bw, 0.5/bw, 0)
        		# weights
		    	# ax <- abs(x)
       		 	# ord <- order(ax)
            	# weights <- numeric(length(ord))
        		# weights[ord[1:k]] <- ifelse(ax[ord[1:k]] < bw, 0.5/bw, 0)
        		# weights
    		}    
    		attributes(rect) <- list(name = "rectangular", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(rect)
}


# @param bw The bandwidth parameter.
# @param k The number of nearest neighbors.
#
#' @rdname wfs

triangular <- function(bw, k) {
	if (missing(bw)) {		# bw missing
		if (missing(k))		# bw and k missing
			stop("either 'bw' or 'k' have to be specified")		
		else {				# only k given -> adaptive bandwidth
			# checks on k
			if ((!is.numeric(k)) || !length(k))
       			stop("'k' must be numeric of length > 0")
       		if (length(k) > 1) {
       			k <- k[1]
       			warning("only first element of 'k' used")
      	 	}
			if (k <= 0)
   		 		stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with adaptive bandwidth
		    triangle <- function(x) {
            	ax <- abs(x)
	        	sax <- sort(ax)
   		    	bw <- sax[k] + 1e-06
       		 	ifelse(ax < bw, (1 - ax/bw)/bw, 0)
 		  		# ax <- abs(x)
         		# sax <- unique(sort(ax))
   	    		# bw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
       		 	# ifelse(ax < bw, (1 - ax/bw)/bw, 0)
       			# ax <- abs(x)
		    	# bw <- sort(ax)[k+1] + .Machine$double.eps
        		# ifelse(ax < bw, (1 - ax/bw)/bw, 0)
    		}
    		attributes(triangle) <- list(name = "triangular", k = k, nn.only = TRUE, adaptive = TRUE)
		}
	} else {				# bw given -> fixed bandwidth
		# checks on bw
		if ((!is.numeric(bw)) || !length(bw))
			stop("'bw' must be numeric of length > 0")
		if (length(bw) > 1) {
			bw <- bw[1]
			warning("only first element of 'bw' used")
		}
		if (bw <= 0)
    		stop("'bw' must be positive")
		if (missing(k)) {	# only bw given -> fixed bandwidth, ignore nn.only
			## window functions with fixed bandwidth
		    triangle <- function(x) {
       			ax <- abs(x)
        		ifelse(ax < bw, (1 - ax/bw)/bw, 0)
    		}
    		attributes(triangle) <- list(name = "triangular", bw = bw, adaptive = FALSE)
		} else {			# bw and k given -> fixed bandwidth with nn.only
			# checks on k
			if ((!is.numeric(k)) || !length(k))
        		stop("'k' must be numeric of length > 0")
        	if (length(k) > 1) {
        		k <- k[1]
        		warning("only first element of 'k' used")
        	}
			if (k <= 0)
    			stop("'k' must be positive")
    		if (abs(k - round(k)) > .Machine$double.eps^0.5)
				warning("'k' should be a natural number and is rounded off")
			## window functions with fixed bandwidth and nn.only
		    triangle <- function(x) {
            	ax <- abs(x)
	        	sax <- sort(ax)
   		    	knnbw <- sax[k] + 1e-06
				ind <- ax < knnbw
            	weights <- numeric(length(ax))
        		weights[ind] <- ifelse(ax[ind] < bw, (1 - ax[ind]/bw)/bw, 0)
        		weights
	            # ax <- abs(x)
	            # sax <- unique(sort(ax))
    	        # knnbw <- sax[k] + min((sax[k+1] - sax[k])/2, 1e-06)
				# ind <- ax < knnbw
            	# weights <- numeric(length(ax))
        		# weights[ind] <- ifelse(ax[ind] < bw, (1 - ax[ind]/bw)/bw, 0)
        		# weights
       			# ax <- abs(x)
       		 	# ord <- order(ax)
            	# weights <- numeric(length(ord))
        		# weights[ord[1:k]] <- ifelse(ax[ord[1:k]] < bw, (1 - ax[ord[1:k]]/bw)/bw, 0)
        		# weights
    		}
    		attributes(triangle) <- list(name = "triangular", bw = bw, k = k, nn.only = TRUE, adaptive = FALSE)
		}
	}
	return(triangle)
}
