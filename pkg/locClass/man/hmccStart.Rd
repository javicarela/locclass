\name{hmccStart}
\alias{hmccStart}
\title{Initialization of the Hierarchical Mixture Classifier and the Common Components Classifier}
\usage{hmccStart(x, grouping, J, method, n, n.k, d, K, lev1, lev, tries, iter, eps, thr)
}
\description{Calculate initial cluster membership probabilities for the function \code{\link{hmcc}} that fits the hierarchical mixture model 
or the common components model.}
\details{The EM algorithm requires starting values for the cluster membership probabilities.
These are obtained by applying the K-means algorithm with \eqn{J} centers.
The K-means algorithm again requires starting centers, which are \eqn{J} randomly selected 
training observations.

In order to avoid the whole procedure suffering from bad choices of starting 
values the common components and hierarchical mixture models are fitted multiple
times (specified by \code{tries}) with different sets of random starting values. The
class posteriors of the training observations are calculated
and the fit with the smallest posterior deviance
\deqn{-2 \cdot \sum_{i=1}^n \ln P(y_i\,|\,x_i)}{%
-2 * \sum_{i=1}^n ln P(y_i | x_i)}
is chosen. Here, \eqn{n} is the number of training observations and \eqn{y_i} denotes
the class label of \eqn{x_i}.}
\value{A matrix containing estimated cluster membership probabilities \eqn{P(j\,|\,x_i)}{P(j|x_i)} of the training observations.}
\references{Dempster, A. P., Laird, N. M., Rubin, D. B. (1977), Maximum likelihood from incomplete
data via the EM algorithm. \emph{Journal of the Royal Statistical Society B}, \bold{39(1)}, 
1--38.

Titsias, M. K., Likas, A. C. (2001), Shared kernel models for class conditional density estimation.
\emph{IEEE Transactions on Neural Networks}, \bold{12(5)}, 987--997.

Titsias, M. K., Likas, A. C. (2002), Mixture of experts classification using a hierarchical mixture model.
\emph{Neural Computation}, \bold{14}, 2221--2244.}
\seealso{\code{\link{hmcc}}, \code{\link{predict.hmcc}}.}
\keyword{classif}
\keyword{cluster}
\keyword{multivariate}
\arguments{\item{x}{A \code{matrix} or \code{data.frame} or \code{Matrix} containing the explanatory variables.}
\item{grouping}{A \code{factor} specifying the class membership for each observation.}
\item{J}{The number of mixture components. See \code{\link{hmcc}}.}
\item{method}{\code{"hm1"} or \code{"hm2"} for the hierarchical mixture classifier, or \code{"cc"} for the common components classifier. See \code{\link{hmcc}}.}
\item{n}{The number of training observations.}
\item{n.k}{The number of training observations in the \eqn{k}-th class (\eqn{k = 1,\ldots,K}{k = 1,...,K}).}
\item{d}{The number of explanatory variables.}
\item{K}{The number of classes.}
\item{lev1}{The class labels.}
\item{lev}{The class labels.}
\item{tries}{The number of random starts. See Details below.}
\item{iter}{A limit on the total number of iterations in the EM algorithm.}
\item{eps}{Stop criterion for the EM algorithm. See \code{\link{hmcc}}.}
\item{thr}{(Required for \code{method} \code{"hm1"} and \code{"hm2"}.) Threshold for pruning of mixture components. See \code{\link{hmcc}}.}
}

