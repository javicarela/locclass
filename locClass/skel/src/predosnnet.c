/*  Copyright (C) 2011 J. Schiffner
 *  nnet/src/nnet.c by W. N. Venables and B. D. Ripley  Copyright (C) 1992-2002
 *
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 or 3 of the License
 *  (at your option).
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  A copy of the GNU General Public License is available at
 *  http://www.r-project.org/Licenses/
 */

/* weights are stored in order of their destination unit.
 * the array Conn gives the source unit for the weight (0 = bias unit)
 * the array Nconn gives the number of first weight connecting to each unit,
 * so the weights connecting to unit i are Nconn[i] ... Nconn[i+1] - 1.
 *
 */


#include "wf.h"
#include "mynnet.h"
#include <R.h>
#include <R_ext/Applic.h>	// BFGS
#include <Rmath.h>			// random numbers
#include <math.h>

static int Epoch;
static double *Decay;
static double TotalError;

static int Nunits;
static int Ninputs;
static int FirstHidden;
static int FirstOutput;
static int Noutputs;
static int NSunits;		/* number of sigmoid units */
static int Nweights;
static int Entropy;
static int Linout;
static int Softmax;
static int Censored;

static double *Outputs;
static double *ErrorSums;
static double *Errors;
static int *Nconn;
static int *Conn;
static double *wts;
static double *Slopes;
static double *Probs;

static int NTrain;
static int NTest;
static Sdata *TrainIn;
static Sdata *TrainOut;
static Sdata *Weights;

static Sdata *toutputs;

static double **H, *h, *h1, **w;

static int p, q;


/* predosnnet
 * net:
 *  s_n:		as.integer(net$n)
 *  s_nconn:	as.integer(net$nconn)
 *  s_conn:		as.integer(net$conn) 
 *  s_decay:	as.double(decay) 
 *  s_nsunits:	as.integer(nsunits) 
 *  s_entropy:	as.integer(entropy) 
 *  s_softmax:	as.integer(softmax)
 *  s_censored:	as.integer(censored))
 *
 * train:
 *  s_ntr:		as.integer(ntr)
 *  s_train:	Z
 *  s_Nw:		as.integer(length(wts)) = nwts = length(object$net$conn)
 *  s_wts:		as.double(wts) 
 *	s_random:	TRUE/FALSE
 *	s_rang:		parameter gleichverteilung
 *  s_Fmin:		val = double(1)
 *  s_maxit:	as.integer(maxit)
 *  s_trace:	as.logical(trace)
 *  s_mask:		as.integer(mask)
 *  s_abstol:	as.double(abstol)
 *  s_reltol:	as.double(reltol)
 *  s_ifail:	integer(1L)
 *
 * test:
 *  s_ntest:	as.integer(ntr) # test observations
 *  s_nout:		# outputs
 *  s_test:		x
 *
 * weights:
 *  s_wf:		weight function, either R function or integer mapping to the name of the function
 *  s_bw:		bandwidth parameter of window function
 *  s_k:		number of nearest neighbors
 *  s_env:		environment for evaluation of R functions
 * 
 */

SEXP predosnnet (SEXP s_n, SEXP s_nconn, SEXP s_conn, SEXP s_decay, SEXP s_nsunits, 
				 SEXP s_entropy, SEXP s_softmax, SEXP s_censored,
				 SEXP s_ntr, SEXP s_train, SEXP s_Nw, SEXP s_wts, SEXP s_random, 
				 SEXP s_rang, SEXP s_Fmin, SEXP s_maxit, SEXP s_trace, SEXP s_mask, 
				 SEXP s_abstol, SEXP s_reltol, SEXP s_ifail, 
				 SEXP s_ntest, SEXP s_nout, SEXP s_test,
				 SEXP s_wf, SEXP s_bw, SEXP s_k, SEXP s_env
				 ) {

	int i, j, l;							// indices

	/* unpack R input */
	Sint *n = INTEGER(s_n);					// information about # units in net
	Sint *ntr = INTEGER(s_ntr);				// # of training observations
	Sdata *train = REAL(s_train);			// pointer to training data set

	/*for (i = 0; i < 3; i++) {
		Rprintf("n %u \n", n[i]);
	}
	Rprintf("ntr %u \n", *ntr);*/
	
	Sint *ntest = INTEGER(s_ntest);			// # of test observations
	Sint *nout = INTEGER(s_nout);			// # of output units
	Sdata *test = REAL(s_test);				// pointer to test data set
	R_len_t p = ncols(s_test);				// # of predictors

	/*Rprintf("ntest %u \n", *ntest);	
	Rprintf("nout %u \n", *nout);	
	Rprintf("p %u \n", p);*/

	double *Fmin = REAL(s_Fmin);			// 
	double *wts = REAL(s_wts);				// pointer to initial weights
	int random = LOGICAL(s_random)[0];		// initialize weights randomly?
	Sint *Nw = INTEGER(s_Nw);				// # of weights
	double iwts[*Nw];						// array for saving initial weights if passed
	double rang;							// parameter of uniform distribution for random initialization of weights
	if (random) {
		rang = REAL(s_rang)[0];				// parameter for uniform distribution
		//Rprintf("rang %f \n", rang);
	} else {								// save initial weights
		for (i = 0; i < *Nw; i++) {
			iwts[i] = wts[i];
		}
	}
	
	/*Rprintf("Fmin %f \n", *Fmin);
	Rprintf("random %u \n", random);
	Rprintf("Nw %u \n", *Nw);*/
	
	
	SEXP s_dist;							// initialize distances to test observation
	PROTECT(s_dist = allocVector(REALSXP, *ntr));
	double *dist = REAL(s_dist);
	
	SEXP s_weights;							// initialize case weight vector
	PROTECT(s_weights = allocVector(REALSXP, *ntr));
	Sdata *weights = REAL(s_weights);
	
	double sum_weights = 0.0;				// sum of observation weights for normalization of weights

	SEXP s_result;							// initialize matrix for raw values
	PROTECT(s_result = allocMatrix(REALSXP, *ntest, *nout));
	Sdata *result = REAL(s_result);	
	
	/* select weight function */
	typedef void (*wf_ptr_t) (double*, double*, int, double*, int);// *w, *dist, N, *bw, k
	wf_ptr_t wf = NULL;
	if (isInteger(s_wf)) {
		const int wf_nr = INTEGER(s_wf)[0];
		wf_ptr_t wfs[] = {biweight1, cauchy1, cosine1, epanechnikov1, exponential1, gaussian1,
			optcosine1, rectangular1, triangular1, biweight2, cauchy2, cosine2, epanechnikov2,
			exponential2, gaussian2, optcosine2, rectangular2, triangular2, biweight3, cauchy3,
			cosine3, epanechnikov3, exponential3, gaussian3, optcosine3, rectangular3, 
			triangular3, cauchy4, exponential4, gaussian4};
		wf = wfs[wf_nr - 1];
	}

	/* set net */
	VR_set_net(n, INTEGER(s_nconn), INTEGER(s_conn),
			   REAL(s_decay), INTEGER(s_nsunits), INTEGER(s_entropy),
			   INTEGER(s_softmax), INTEGER(s_censored));	
	/*VR_set_net(Sint *n, Sint *nconn, Sint *conn,
	 double *decay, Sint *nsunits, Sint *entropy,
	 Sint *softmax, Sint *censored);	*/
	/*for (i = 0; i <= 11; i++)
		Rprintf("s_nconn %u \n", INTEGER(s_nconn)[i]);
	Rprintf("Nunits %u \n", Nunits);*/

	GetRNGstate();

	/* loop over test observations */
	for (l = 0; l < *ntest; l++) {
		
		//Rprintf("Nweights %u\n", Nweights);
		/* initialize weights */
		if (random) {		// generate initial weights randomly
			for (i = 0; i < *Nw; i++) {
				wts[i] = runif(-rang, rang);
				//Rprintf("wts %f\n", wts[i]);
			}
		} else {			// restore initial weights
			for (i = 0; i < *Nw; i++) {
				wts[i] = iwts[i];
				//Rprintf("wts %f\n", wts[i]);
			}
		}
		/* reinitialize stuff */
		*Fmin = 0.0;
		Epoch = 0;
		TotalError = 0.0;
		sum_weights = 0.0;
		
		/* calculate distances to n-th test observation */
		for (i = 0; i < *ntr; i++) {
			dist[i] = 0;
			for (j = 0; j < p; j++) {
				dist[i] += pow(train[i + *ntr * j] - test[l + *ntest * j], 2);
			}
			dist[i] = sqrt(dist[i]);
			weights[i] = 0;
			//Rprintf("dist %f\n", dist[i]);
		}
	
		/* calculate observation weights */
		if (isInteger(s_wf)) {
			// case 1: wf is integer
			// calculate weights by reading number and calling corresponding C function
			wf (weights, dist, *ntr, REAL(s_bw), INTEGER(s_k)[0]);
		} else if (isFunction(s_wf)) {
			// case 2: wf is R function
			// calculate weights by calling R function
			SEXP R_fcall;
			PROTECT(R_fcall = lang2(s_wf, R_NilValue));
			SETCADR(R_fcall, s_dist);
			weights = REAL(eval(R_fcall, s_env));
			UNPROTECT(1); // R_fcall			
		}
		
		/* rescale weights such that they sum up to *ntr */
		for(i = 0; i < *ntr; i++) {
			sum_weights += weights[i];
			//Rprintf("weights %f\n", weights[i]);
		}
		for(i = 0; i < *ntr; i++) {
			weights[i] = weights[i]/sum_weights * *ntr; // ?numerical problems, 0/0?
			//Rprintf("weights %f\n", weights[i]);
		}
		
		/* train net */
		VR_dovm(ntr, train, weights,
				Nw, wts, Fmin,
				INTEGER(s_maxit), INTEGER(s_trace), INTEGER(s_mask),
				REAL(s_abstol), REAL(s_reltol), INTEGER(s_ifail));
		/*VR_dovm(Sint *ntr, Sdata *train, Sdata *weights,
			Sint *Nw, double *wts, double *Fmin,
			Sint *maxit, Sint *trace, Sint *mask,
			double *abstol, double *reltol, int *ifail);*/
		
		/*for (i = 0; i < *Nw; i++) {
			Rprintf("wts %f\n", wts[i]);
		}*/
		
		/* predict l-th test observation */
		VR_nntest(ntest, test, result, wts, l);
		/*VR_nntest(Sint *ntest, Sdata *test, Sdata *result, double *wts);*/
	
	}
	// end loop over test observations
	
	/* cleaning up */
	VR_unset_net();
	PutRNGstate();
	UNPROTECT(3);		// s_dist, s_w, s_result
	return(s_result);

}