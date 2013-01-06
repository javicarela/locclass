/*
 *  Copyright (C) 2011-2013 J. Schiffner
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

#include "wf.h"

/* window functions
 *
 * *weights		pointer to weights vector, which is changed in these functions
 * *dist		pointer to dist
 * *N			length of weights and dist
 * *bw			pointer to bandwidth parameter
 * *k			number of nearest neighbors
 *
 * Note that not all arguments passed to the window functions are always used.
 *
 */
/*
 * FIXME: fabs()? check for positive dist?
 */

/***********************************************************************************************/
/* biweight 
 *	variant 1: fixed bandwidth
 */
void biweight1 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		weights[i] = adist < *bw ? 15/(double) 16 * pow(1 - pow(adist/ *bw, 2), 2)/ *bw : 0;		
	}
}


/* cauchy 
 *	variant1: fixed bandwidth
 */
void cauchy1 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	for (i = 0; i < *N; i++) {
		weights[i] = 1/(M_PI * (1 + pow(dist[i]/ *bw, 2)) * *bw);
	}
}


/* cosine 
 *	variant1: fixed bandwidth 
 */
void cosine1 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	for (i = 0; i < *N; i++) {
		weights[i] = fabs(dist[i]) < *bw ? (1 + cos(M_PI * dist[i]/ *bw))/(2 * *bw) : 0;
	}
}


/* epanechnikov 
 *	variant1: fixed bandwidth 
 */
void epanechnikov1 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		weights[i] = adist < *bw ? 0.75 * (1 - pow(adist/ *bw, 2))/ *bw : 0;		
	}
}


/* exponential 
 *	variant1: fixed bandwidth 
 */
void exponential1 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	for (i = 0; i < *N; i++) {
		weights[i] = 0.5 * exp(-fabs(dist[i])/ *bw)/ *bw;
	}
}


/* gaussian 
 *	variant1: fixed bandwidth 
 */
void gaussian1 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	for (i = 0; i < *N; i++) {
		weights[i] = dnorm(dist[i], 0, *bw, 0);
	}
}


/* optcosine 
 *	variant1: fixed bandwidth 
 */
void optcosine1 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	for (i = 0; i < *N; i++) {
		weights[i] = fabs(dist[i]) < *bw ? M_PI_4 * cos(M_PI * dist[i]/(2 * *bw))/ *bw : 0;		
	}
}


/* rectangular 
 *	variant1: fixed bandwidth 
 */
void rectangular1 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	for (i = 0; i < *N; i++) {
		weights[i] = fabs(dist[i]) < *bw ? 0.5/ *bw : 0;		
	}
}


/* triangular 
 *	variant1: fixed bandwidth 
 */
void triangular1 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		weights[i] = adist < *bw ? (1 - adist/ *bw)/ *bw : 0;
	}
}

/***********************************************************************************************/
/* biweight 
 *	variant 2: fixed bandwidth, k nearest neighbors only
 */
void biweight2 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		if (adist < a) {
			weights[i] = adist < *bw ? 15/(double) 16 * pow(1 - pow(adist/ *bw, 2), 2)/ *bw : 0;		
		} else {
			weights[i] = 0;
		}
	}
	// old version
/*	double adist;
	int i;
	int index[*N];
	for (i = 0; i < *N; i++) {
		index[i] = i;
		//Rprintf("index %u\n", index[i]);
	}	
	rsort_with_index (dist, index, *N);
	//for (i = 0; i < *N; i++) {
	// Rprintf("dist0 %f\n", dist[i]);
	// Rprintf("index0 %u\n", index[i]);				
	// }	
	for (i = 0; i < *k; i++) {
		adist = fabs(dist[i]);
		weights[index[i]] = adist < *bw ? 15/(double) 16 * pow(1 - pow(adist/ *bw, 2), 2)/ *bw : 0;		
	}
*/
}


/* cauchy 
 * variant 2: fixed bandwidth, k nearest neighbors only
 */
void cauchy2 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		if (adist < a) {
			weights[i] = 1/(M_PI * (1 + pow(dist[i]/ *bw, 2)) * *bw);
		} else {
			weights[i] = 0;
		}
	}
	// old version
/*	int i;
	int index[*N];
	for (i = 0; i < *N; i++) {
		index[i] = i;
		//Rprintf("index %u\n", index[i]);
	}	
	rsort_with_index (dist, index, *N);
	//for (i = 0; i < *N; i++) {
	// Rprintf("dist0 %f\n", dist[i]);
	// Rprintf("index0 %u\n", index[i]);				
	// }	
	for (i = 0; i < *k; i++) {
		weights[index[i]] = 1/(M_PI * (1 + pow(dist[i]/ *bw, 2)) * *bw);
	}
*/	
}


/* cosine 
 * variant 2: fixed bandwidth, k nearest neighbors only 
 */
void cosine2 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		if (adist < a) {
			weights[i] = adist < *bw ? (1 + cos(M_PI * dist[i]/ *bw))/(2 * *bw) : 0;		
		} else {
			weights[i] = 0;
		}
	}
	// old version
/*	int i;
	int index[*N];
	for (i = 0; i < *N; i++) {
		index[i] = i;
		//Rprintf("index %u\n", index[i]);
	}	
	rsort_with_index (dist, index, *N);
	//for (i = 0; i < *N; i++) {
	// Rprintf("dist0 %f\n", dist[i]);
	// Rprintf("index0 %u\n", index[i]);				
	// }	
	for (i = 0; i < *k; i++) {
		weights[index[i]] = fabs(dist[i]) < *bw ? (1 + cos(M_PI * dist[i]/ *bw))/(2 * *bw) : 0;
	}
*/
}


/* epanechnikov 
 * variant 2: fixed bandwidth, k nearest neighbors only 
 */
void epanechnikov2 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		if (adist < a) {
			weights[i] = adist < *bw ? 0.75 * (1 - pow(adist/ *bw, 2))/ *bw : 0;		
		} else {
			weights[i] = 0;
		}
	}
	// old version
/*	double adist;
	int i;
	int index[*N];
	for (i = 0; i < *N; i++) {
		index[i] = i;
		//Rprintf("index %u\n", index[i]);
	}	
	rsort_with_index (dist, index, *N);
	//for (i = 0; i < *N; i++) {
	// Rprintf("dist0 %f\n", dist[i]);
	// Rprintf("index0 %u\n", index[i]);				
	// }	
	for (i = 0; i < *k; i++) {
		adist = fabs(dist[i]);
		weights[index[i]] = adist < *bw ? 0.75 * (1 - pow(adist/ *bw, 2))/ *bw : 0;		
	}
*/
}


/* exponential 
 * variant 2: fixed bandwidth, k nearest neighbors only 
 */
void exponential2 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		if (adist < a) {
			weights[i] = 0.5 * exp(-fabs(dist[i])/ *bw)/ *bw;		
		} else {
			weights[i] = 0;
		}
	}
	// old version
/*	int i;
	int index[*N];
	for (i = 0; i < *N; i++) {
		index[i] = i;
		//Rprintf("index %u\n", index[i]);
	}	
	rsort_with_index (dist, index, *N);
	//for (i = 0; i < *N; i++) {
	// Rprintf("dist0 %f\n", dist[i]);
	// Rprintf("index0 %u\n", index[i]);				
	// }	
	for (i = 0; i < *k; i++) {
		weights[index[i]] = 0.5 * exp(-fabs(dist[i])/ *bw)/ *bw;
	}
*/
}


/* gaussian 
 * variant 2: fixed bandwidth, k nearest neighbors only 
 */
void gaussian2 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		if (adist < a) {
			weights[i] = dnorm(dist[i], 0, *bw, 0);		
		} else {
			weights[i] = 0;
		}
	}
	// old version
/*	int i;
	int index[*N];
	for (i = 0; i < *N; i++) {
		index[i] = i;
		//Rprintf("index %u\n", index[i]);
	}	
	rsort_with_index (dist, index, *N);
	//for (i = 0; i < *N; i++) {
	// Rprintf("dist0 %f\n", dist[i]);
	// Rprintf("index0 %u\n", index[i]);				
	// }	
	for (i = 0; i < *k; i++) {
		weights[index[i]] = dnorm(dist[i], 0, *bw, 0);
	}
*/
}

/* optcosine 
 * variant 2: fixed bandwidth, k nearest neighbors only 
 */
void optcosine2 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		if (adist < a) {
			weights[i] = adist < *bw ? M_PI_4 * cos(M_PI * dist[i]/(2 * *bw))/ *bw : 0;
		} else {
			weights[i] = 0;
		}
	}
	// old version
/*	int i;
	int index[*N];
	for (i = 0; i < *N; i++) {
		index[i] = i;
		//Rprintf("index %u\n", index[i]);
	}	
	rsort_with_index (dist, index, *N);
	//for (i = 0; i < *N; i++) {
	// Rprintf("dist0 %f\n", dist[i]);
	// Rprintf("index0 %u\n", index[i]);				
	// }	
	for (i = 0; i < *k; i++) {
		weights[index[i]] = fabs(dist[i]) < *bw ? M_PI_4 * cos(M_PI * dist[i]/(2 * *bw))/ *bw : 0;		
	}
*/
}


/* rectangular 
 * variant 2: fixed bandwidth, k nearest neighbors only 
 */
void rectangular2 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		if (adist < a) {
			weights[i] = adist < *bw ? 0.5/ *bw : 0;
		} else {
			weights[i] = 0;
		}
	}
	// old version
/*	int i;
	int index[*N];
	for (i = 0; i < *N; i++) {
		index[i] = i;
		//Rprintf("index %u\n", index[i]);
	}	
	rsort_with_index (dist, index, *N);
	//for (i = 0; i < *N; i++) {
	// Rprintf("dist0 %f\n", dist[i]);
	// Rprintf("index0 %u\n", index[i]);				
	// }	
	for (i = 0; i < *k; i++) {
		weights[index[i]] = fabs(dist[i]) < *bw ? 0.5/ *bw : 0;		
	}
*/
}


/* triangular 
 * variant 2: fixed bandwidth, k nearest neighbors only 
 */
void triangular2 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		if (adist < a) {
			weights[i] = adist < *bw ? (1 - adist/ *bw)/ *bw : 0;
		} else {
			weights[i] = 0;
		}
	}
	// old version
/*	double adist;
	int i;
	int index[*N];
	for (i = 0; i < *N; i++) {
		index[i] = i;
		//Rprintf("index %u\n", index[i]);
	}	
	rsort_with_index (dist, index, *N);
	//for (i = 0; i < *N; i++) {
	// Rprintf("dist0 %f\n", dist[i]);
	// Rprintf("index0 %u\n", index[i]);				
	// }
	for (i = 0; i < *k; i++) {
		adist = fabs(dist[i]);
		weights[index[i]] = adist < *bw ? (1 - adist/ *bw)/ *bw : 0;
	}
*/
}


/***********************************************************************************************/
/* biweight 
 *	variant 3: adaptive bandwidth, k nearest neighbors only
 */
void biweight3 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
//	double a = distcopy[*k] + DOUBLE_EPS;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		weights[i] = adist < a ? 15/(double) 16 * pow(1 - pow(adist/a, 2), 2)/a : 0;		
	}
}


/* cauchy 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void cauchy3 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		weights[i] = adist < a ? 1/(M_PI * (1 + pow(dist[i]/a, 2)) * a) : 0;
	}
	// old version
/*	int i;
	int index[*N];
	for (i = 0; i < *N; i++) {
		index[i] = i;
		//Rprintf("index %u\n", index[i]);
	}	
	rsort_with_index (dist, index, *N);
	double a = dist[*k] + DOUBLE_EPS;
	//for (i = 0; i < *N; i++) {
	// Rprintf("dist0 %f\n", dist[i]);
	// Rprintf("index0 %u\n", index[i]);				
	// }	
	for (i = 0; i < *k; i++) {
		weights[index[i]] = 1/(M_PI * (1 + pow(dist[i]/a, 2)) * a);
	}
*/
}


/* cosine 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void cosine3 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
//	double a = distcopy[*k] + DOUBLE_EPS;
	for (i = 0; i < *N; i++) {
		weights[i] = fabs(dist[i]) < a ? (1 + cos(M_PI * dist[i]/a))/(2 * a) : 0;
	}
}


/* epanechnikov 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void epanechnikov3 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
//	double a = distcopy[*k] + DOUBLE_EPS;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		weights[i] = adist < a ? 0.75 * (1 - pow(adist/a, 2))/a : 0;		
	}
}


/* exponential 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void exponential3 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		weights[i] = adist < a ? 0.5 * exp(-adist/a)/a : 0;
	}
	// old version
/*	int i;
	int index[*N];
	for (i = 0; i < *N; i++) {
		index[i] = i;
		//Rprintf("index %u\n", index[i]);
	}	
	rsort_with_index (dist, index, *N);
	double a = dist[*k] + DOUBLE_EPS;
	//for (i = 0; i < *N; i++) {
	// Rprintf("dist0 %f\n", dist[i]);
	// Rprintf("index0 %u\n", index[i]);				
	// }	
	for (i = 0; i < *k; i++) {
		weights[index[i]] = 0.5 * exp(-fabs(dist[i])/a)/a;
	}
*/
}


/* gaussian 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void gaussian3 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		weights[i] = adist < a ? dnorm(dist[i], 0, a, 0) : 0;
	}
	// old version
/*	int i;
	int index[*N];
	for (i = 0; i < *N; i++) {
		index[i] = i;
		//Rprintf("index %u\n", index[i]);
	}	
	rsort_with_index (dist, index, *N);
	double a = dist[*k] + DOUBLE_EPS;
	//for (i = 0; i < *N; i++) {
	// Rprintf("dist0 %f\n", dist[i]);
	// Rprintf("index0 %u\n", index[i]);				
	// }	
	for (i = 0; i < *k; i++) {
		weights[index[i]] = dnorm(dist[i], 0, a, 0);
	}
*/
}


/* optcosine 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void optcosine3 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
//	double a = distcopy[*k] + DOUBLE_EPS;
	for (i = 0; i < *N; i++) {
		weights[i] = fabs(dist[i]) < a ? M_PI_4 * cos(M_PI * dist[i]/(2 * a))/a : 0;		
	}
}


/* rectangular 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void rectangular3 (double *weights, double *dist, int *N, double *bw, int *k) {
	//double a = *bw * sqrt(3);
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
//	double a = distcopy[*k] + DOUBLE_EPS;
	for (i = 0; i < *N; i++) {
		weights[i] = fabs(dist[i]) < a ? 0.5/a : 0;		
	}
}


/* triangular 
 * variant 3: adaptive bandwidth, k nearest neighbors only
 */
void triangular3 (double *weights, double *dist, int *N, double *bw, int *k) {
	double adist;
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
//	double a = distcopy[*k] + DOUBLE_EPS;
	for (i = 0; i < *N; i++) {
		adist = fabs(dist[i]);
		weights[i] = adist < a ? (1 - adist/a)/a : 0;
	}
}


/***********************************************************************************************/
/* cauchy 
 * variant 4: adaptive bandwidth, all observations 
 */
void cauchy4 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
//	double a = distcopy[*k] + DOUBLE_EPS;
	for (i = 0; i < *N; i++) {
		weights[i] = 1/(M_PI * (1 + pow(dist[i]/a, 2)) * a);
	}
	
}


/* exponential 
 * variant 4: adaptive bandwidth, all observations 
 */
void exponential4 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
//	double a = distcopy[*k] + DOUBLE_EPS;
	for (i = 0; i < *N; i++) {
		weights[i] = 0.5 * exp(-fabs(dist[i])/a)/a;
	}
}


/* gaussian 
 * variant 4: adaptive bandwidth, all observations 
 */
void gaussian4 (double *weights, double *dist, int *N, double *bw, int *k) {
	int i;
	double distcopy[*N];
	for (i = 0; i < *N; i++) {
		distcopy[i] = dist[i];
	}
	rPsort(distcopy, *N, *k-1);
	double a = fabs(distcopy[*k-1]) + 1e-06;
//	double a = distcopy[*k] + DOUBLE_EPS;
	for (i = 0; i < *N; i++) {
		weights[i] = dnorm(dist[i], 0, a, 0);
	}
}

/***********************************************************************************************/


/*
 void biweight (double *weights, double *dist, int N, double *bw) {
	//double a = *bw * sqrt(7);
	double adist;
	int i;
	for (i = 0; i < N; i++) {
		adist = fabs(dist[i]);
		weights[i] = adist < *bw ? 15/(double) 16 * pow(1 - pow(adist/ *bw, 2), 2)/a : 0;		
	}
}

void cauchy (double *weights, double *dist, int N, double *bw) {
	int i;
	for (i = 0; i < N; i++) {
		weights[i] = 1/(M_PI * (1 + pow(dist[i]/ *bw, 2)) * *bw);
	}
}

void cosine (double *weights, double *dist, int N, double *bw) {
	//double a = *bw/sqrt(1/ (double) 3 - 2/pow(M_PI, 2));
	int i;
	for (i = 0; i < N; i++) {
		weights[i] = fabs(dist[i]) < *bw ? (1 + cos(M_PI * dist[i]/ *bw))/(2 * *bw) : 0;
	}
}

void epanechnikov (double *weights, double *dist, int N, double *bw) {
	//double a = *bw * sqrt(5);
	double adist;
	int i;
	for (i = 0; i < N; i++) {
		adist = fabs(dist[i]);
		weights[i] = adist < *bw ? 0.75 * (1 - pow(adist/ *bw, 2))/ *bw : 0;		
	}
}

void exponential (double *weights, double *dist, int N, double *bw) {
	//double a = *bw * sqrt(2);
	int i;
	for (i = 0; i < N; i++) {
		weights[i] = 0.5 * exp(-fabs(dist[i])/ *bw)/ *bw;
	}
}

void gaussian (double *weights, double *dist, int N, double *bw) {
	int i;
	for (i = 0; i < N; i++) {
		weights[i] = dnorm(dist[i], 0, *bw, 0);
	}
}

void optcosine (double *weights, double *dist, int N, double *bw) {
	//double a = *bw/sqrt(1 - 8/pow(M_PI, 2));
	int i;
	for (i = 0; i < N; i++) {
		weights[i] = fabs(dist[i]) < *bw ? M_PI_4 * cos(M_PI * dist[i]/(2 * *bw))/ *bw : 0;		
	}
}

void rectangular (double *weights, double *dist, int N, double *bw) {
	//double a = *bw * sqrt(3);
	int i;
	for (i = 0; i < N; i++) {
		weights[i] = fabs(dist[i]) < *bw ? 0.5/ *bw : 0;		
	}
}

void triangular (double *weights, double *dist, int N, double *bw) {
	//double a = *bw * sqrt(6);
	double adist;
	int i;
	for (i = 0; i < N; i++) {
		adist = fabs(dist[i]);
		weights[i] = adist < *bw ? (1 - adist/ *bw)/ *bw : 0;
	}
}
*/