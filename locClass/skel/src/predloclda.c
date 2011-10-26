/*  copyright (C) 2011 J. Schiffner
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
 *
 */

# include "wf.h"

/* predloclda:
 *
 *  s_test:		test data, N_test * p matrix
 *  s_learn:	training data, N_learn * p matrix
 *  s_grouping:	class labels, factor of length N_learn
 *  s_wf:		weight function, either R function or integer mapping to the name of the function
 *  s_bw:		bandwidth parameter of window function
 *  s_k:		number of nearest neighbors
 *  s_env:		environment for evaluation of R functions
 *
 */

SEXP predloclda(SEXP s_test, SEXP s_learn, SEXP s_grouping, SEXP s_wf, SEXP s_bw, SEXP s_k, SEXP s_env)
{
	const R_len_t p = ncols(s_test);		// dimensionality
	const R_len_t N_learn = nrows(s_learn);	// # training observations
	const R_len_t N_test = nrows(s_test);	// # test observations
	const R_len_t K = nlevels(s_grouping);	// # classes
	double *test = REAL(s_test);			// pointer to test data set
	double *learn = REAL(s_learn);			// pointer to training data set
	int *g = INTEGER(s_grouping);			// pointer to class labels
	const int k = INTEGER(s_k)[0];			// number of nearest neighbors
	
	SEXP s_posterior;						// initialize posteriors
	PROTECT(s_posterior = allocMatrix(REALSXP, N_test, K));
	double *posterior = REAL(s_posterior);
	
	SEXP s_dist;							// initialize distances to test observation
	PROTECT(s_dist = allocVector(REALSXP, N_learn));
	double *dist = REAL(s_dist);
	
	SEXP s_weights;							// initialize weight vector
	PROTECT(s_weights = allocVector(REALSXP, N_learn));
	double *weights = REAL(s_weights);
	
	double sum_weights;						// sum of weights
	double class_weights[K];				// class wise sum of weights
	double center[K][p];					// class means
	double covmatrix[p * p];				// pooled covariance matrix
	double z[p * K];						// difference between trial point and class center
	
	const char uplo = 'L', side = 'L';
	int info = 0;
	double onedouble = 1.0, zerodouble = 0.0;
	double C[p * K];
	double post[K];
	
	int i, j, l, m, n;						// indices
	
	// select weight function
	typedef void (*wf_ptr_t) (double*, double*, int, double*, int);// *weights, *dist, N, *bw, k
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
	
	// loop over all test observations
	for(n = 0; n < N_test; n++) {
			
		// 1. calculate distances to n-th test observation
		for (i = 0; i < N_learn; i++) {
			dist[i] = 0;
			for (j = 0; j < p; j++) {
				dist[i] += pow(learn[i + N_learn * j] - test[n + N_test * j], 2);
			}
			dist[i] = sqrt(dist[i]);
			weights[i] = 0;
			//Rprintf("dist %f\n", dist[i]);
		}
			
		// 2. calculate observation weights
		if (isInteger(s_wf)) {
			// case 1: wf is integer
			// calculate weights by reading number and calling corresponding C function
			wf (weights, dist, N_learn, REAL(s_bw), k);
		} else if (isFunction(s_wf)) {
			// case 2: wf is R function
			// calculate weights by calling R function
			SEXP R_fcall;
			PROTECT(R_fcall = lang2(s_wf, R_NilValue));
			SETCADR(R_fcall, s_dist);
			weights = REAL(eval(R_fcall, s_env));
			UNPROTECT(1); // R_fcall
		}
		/*for(i = 0; i < N_learn; i++) {
			Rprintf("weights %f\n", weights[i]);
		}*/
		
		// 3. calculate sum of weights, class wise sum of weights and unnormalized class means
		sum_weights = 0;
		for (m = 0; m < K; m++) {
			class_weights[m] = 0;
			for (j = 0; j < p; j++) {
				center[m][j] = 0;
			}
		}
		for (i = 0; i < N_learn; i++) {
			sum_weights += weights[i];
			for (m = 0; m < K; m++) {
				if (g[i] == m + 1) {
					class_weights[m] += weights[i];
					for (j = 0; j < p; j++) {
						center[m][j] += learn[i + N_learn * j] * weights[i];
					}
				}
			}
		}
		
		
		// 4. calculate covariance matrix, only lower triangle
		for (j = 0; j < p; j++) {
			for (l = 0; l <= j; l++) {
				covmatrix[j + p * l] = 0;
			}	
		}
		for (m = 0; m < K; m++) {
			if (class_weights[m] > 0) {	// only for classes with positive sum of weights
				for (i = 0; i < N_learn; i++) {
					if (g[i] == m + 1) {
						for (j = 0; j < p; j++) {
							for (l = 0; l <= j; l++) {
								covmatrix[j + p * l] += weights[i]/sum_weights * 
								(learn[i + N_learn * j] - center[m][j]/class_weights[m]) * 
								(learn[i + N_learn * l] - center[m][l]/class_weights[m]);
							}
						}
					}
				}
			}
		}

		// 5. calculate inverse of covmatrix
		F77_CALL(dpotrf)(&uplo, &p, covmatrix, &p, &info);
		F77_CALL(dpotri)(&uplo, &p, covmatrix, &p, &info);
		
		// 6. calculate difference between n-th test observation and all class centers
		for (m = 0; m < K; m++) {
			if (class_weights[m] > 0) {	// only for classes with positive sum of weights
				for (j = 0; j < p; j++) {
					z[j + p * m] = test[n + N_test * j] - center[m][j]/class_weights[m];
				}
			} else {
				for (j = 0; j < p; j++) {
					z[j + p * m] = 0;
				}
			}
		}

		// 7. calcualte C = covmatrix * z
		F77_CALL(dsymm)(&side, &uplo, &p, &K, &onedouble, covmatrix, &p, z, &p, &zerodouble, C, &p);
		
		// 8. calculate t(z) * C (mahalanobis distance) and unnormalized posterior probabilities
		for (m = 0; m < K; m++) {
			if (class_weights[m] > 0) {
				post[m] = 0;
				for (j = 0; j < p; j++) {
					post[m] += C[j + p * m] * z[j + p * m];
				}
				posterior[n + N_test * m] = class_weights[m]/sum_weights * exp(-0.5 * post[m]);
			} else {
				posterior[n + N_test * m] = 0;
			}
		}			
		
	}
	// end loop over test observations
		
	
	// 9. set dimnames of s_posterior
	SEXP dimnames;
	PROTECT(dimnames = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(getAttrib(s_test, R_DimNamesSymbol), 0));
	SET_VECTOR_ELT(dimnames, 1, getAttrib(s_grouping, R_LevelsSymbol));
	setAttrib(s_posterior, R_DimNamesSymbol, dimnames);
	
	UNPROTECT(4);	// dimnames, s_dist, s_weights, s_posterior
	return(s_posterior);
}