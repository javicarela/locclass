\name{hmcc}
\alias{hmcc}
\title{Hierarchical Mixture and Common Components Classifier}
\usage{hmcc(x, ...)
\method{hmcc}{formula}(formula, data, ..., subset, na.action)
\method{hmcc}{data.frame}(x, ...)
\method{hmcc}{matrix}(x, grouping, ..., subset, na.action=na.fail)
\method{hmcc}{default}(x, grouping, J, method=c("hm1", "hm2", "cc"), ..., tries=10, iter=15,
    eps=10^(-5), thr)
}
\description{Fit the hierarchical mixture model or the common components model for Gaussian mixture-based classification.}
\details{\code{hmcc}: This function fits the hierarchical mixture model (Titsias and Likas, 2002) or the common components model
(Titsias and Likas, 2001) for Gaussian mixture-based classification, that is the class conditional distributions 
are modeled as Gaussian mixtures.

If \code{method = "cc"} the common components model is fitted. It is given as 
\deqn{f(x\,|\,\theta) = \sum_{k=1}^K p_k \sum_{j=1}^J \pi_{jk} \phi(x\,|\,\mu_j, \Sigma_j)}{%
f(x | theta) = sum_{k=1}^K P.k sum_{j=1}^J Pi.jk  phi(x | mu.j, Sigma.j)}
where      
\describe{
\item{\eqn{K}:}{the number of classes,}
\item{\eqn{J:}}{the number of mixture components,}
\item{\eqn{p_k:}{P.k}}{the class priors,}
\item{\eqn{\pi_{jk}}{Pi.jk}:}{the mixture weights.}  
}
The maximum likelihood parameter estimates are computed by means of the EM algorithm (Dempster et al., 1977).

If \code{method = "hm1"} or \code{"hm2"} the hierarchical mixture model
\deqn{f(x\,|\,\theta) = \sum_{j=1}^J \pi_j \sum_{k=1}^K p_{kj} \phi(x\,|\,\mu_{kj}, \Sigma_{kj})}{%
f(x | theta) = sum_{j=1}^J Pi.j sum_{k=1}^K P.kj  phi(x | mu.kj, Sigma.kj)}
is fitted where 
\describe{
\item{\eqn{J}:}{the number of mixture components,}  
\item{\eqn{K}:}{the number of classes,}
\item{\eqn{\pi_j}{Pi.j}:}{the mixture weights,}  
\item{\eqn{p_{kj}}{P.kj}:}{the conditional class priors.}
}
The hierarchical structure of the model is reflected when calculating the parameter estimates. 
In a first step, a model-based clustering is carried out. For this purpose two different models can be applied:
\describe{
\item{\code{"hm1"}}{A simple Gaussian mixture model:
\deqn{f(x\,|\,\psi) = \sum_{j=1}^J \pi_j \phi(x\,|\,\mu_j, \Sigma_j).}{%
f(x | psi) = sum_{j=1}^J Pi.j phi(x | mu.j, Sigma.j).}}
\item{\code{"hm2"}}{The common components model:
\deqn{f(x\,|\,\psi) = \sum_{k=1}^K p_k \sum_{j=1}^J \pi_{jk} \phi(x\,|\,\mu_j, \Sigma_j).}{%
f(x | psi) = sum_{k=1}^K P.k sum_{j=1}^J Pi.jk  phi(x | mu.j, Sigma.j).}}
}
Based on the estimated cluster membership probabilities obtained in the first step
maximum likelihood estimates of \eqn{\pi_j}{Pi.j}, \eqn{p_{kj}}{P.kj}, \eqn{\mu_{kj}}{mu.kj}, and \eqn{\Sigma_{kj}}{Sigma.kj} 
are calculated in a second step.

If the estimate of \eqn{p_{kj}}{P.kj} is close to zero it is likely that the \eqn{j}-th mixture component 
does not represent data of the \eqn{k}-th class. Therefore, if the estimate of \eqn{p_{kj}}{P.kj} is below 
the chosen \code{thr} it is set to zero, i.e. the \eqn{j}-th mixture component of the \eqn{k}-th 
class is pruned.
By default \code{thr} is set to 
\deqn{\frac{\textrm{the number of predictors} + 1}{\textrm{the number of training observations}}.}{%
(the number of predictors + 1)/(the number of training observations).
}

stopping criterion, \code{eps} ???

}
\value{\code{hmcc}: An object of class \code{"hmcc"} containing the following components:
\item{K}{The number of classes.}
\item{J}{The number of mixture components.}
\item{P.k}{(Only for \code{method} \code{"cc"}.) A vector of class priors.}
\item{Pi.jk}{(Only for \code{method} \code{"cc"}.) A matrix of conditional mixture weights.}
\item{Pi.j}{(Only for \code{method}s \code{"hm1"} and \code{hm2}.) A vector of mixture weights.}
\item{P.kj}{(Only for \code{method}s \code{"hm1"} and \code{hm2}.) A matrix of conditional class priors.}
\item{mu.kj}{(Only for \code{method}s \code{"hm1"} and \code{hm2}.) Array of mean vectors. If a mixture component was pruned the correponding entries in \code{mu.kj} are set to \code{NA}.}
\item{Sigma.kj}{(Only for \code{method}s \code{"hm1"} and \code{hm2}.) Array of covariance matrices. If a mixture component was pruned the correponding entries in \code{Sigma.kj} are set to \code{NA}.}
\item{mu.j}{Array of mean vectors either of the simple mixture model (for \code{method} \code{"hm1"}) or of the common components model (for \code{method}s \code{"cc"} and \code{hm2}).}
\item{Sigma.j}{Array of covariance matrices either of the simple mixture model (for \code{method} \code{"hm1"}) or of the common components model (for \code{method}s \code{"cc"} and \code{"hm2"}).}
\item{ll}{The maximized log-likelihood.}
\item{method}{The \code{method} used (\code{"cc"}, \code{"hm1"}, or \code{"hm2"})}
\item{call}{The (matched) function call.}

}
\references{Dempster, A. P., Laird, N. M., Rubin, D. B. (1977), Maximum likelihood from incomplete
data via the EM algorithm. \emph{Journal of the Royal Statistical Society B}, \bold{39(1)}, 
1--38.

Titsias, M. K., Likas, A. C. (2001), Shared kernel models for class conditional density estimation.
\emph{IEEE Transactions on Neural Networks}, \bold{12(5)}, 987--997.

Titsias, M. K., Likas, A. C. (2002), Mixture of experts classification using a hierarchical mixture model.
\emph{Neural Computation}, \bold{14}, 2221--2244.}
\seealso{\code{\link{hmccStart}}, \code{\link{predict.hmcc}}. See also \pkg{\link[mda]{mda}} for Gaussian mixture-based classification.}
\keyword{classif}
\keyword{cluster}
\keyword{multivariate}
\alias{hmcc}
\alias{hmcc.data.frame}
\alias{hmcc.default}
\alias{hmcc.formula}
\alias{hmcc.matrix}
\arguments{\item{formula}{A \code{formula} of the form \code{groups ~ x1 + x2 + ...}, that is, the response is the grouping \code{factor} 
and the right hand side specifies the (non-\code{factor}) discriminators.}
\item{data}{A \code{data.frame} from which variables specified in \code{formula} are to be taken.}
\item{x}{(Required if no \code{formula} is given as principal argument.) A \code{matrix} or \code{data.frame} or \code{Matrix} containing the explanatory variables.}
\item{grouping}{(Required if no \code{formula} is given as principal argument.) A \code{factor} specifying the class membership for each observation.}
\item{J}{The number of mixture components. See Details below.}
\item{method}{\code{"hm1"} or \code{"hm2"} for the hierarchical mixture classifier, or \code{"cc"} for the common components classifier. See Details below.}
\item{\dots}{Further arguments.}
\item{tries}{The number of random starts. See \code{\link{hmccStart}}.}
\item{iter}{A limit on the total number of iterations in the EM algorithm.}
\item{eps}{Stop criterion for the EM algorithm. See Details.}
\item{thr}{(Required for \code{method}s \code{"hm1"} and \code{"hm2"}.) Threshold for pruning of mixture components. See Details below.}
\item{subset}{An index vector specifying the cases to be used in the training sample. (NOTE: If given, this argument must be named.)}
\item{na.action}{A function to specify the action to be taken if \code{NA}s are found.}
}
\examples{library(mlbench)
data.train <- as.data.frame(mlbench.waveform(300))
data.test <- as.data.frame(mlbench.waveform(200))

## "cc"
fit <- hmcc(classes ~ ., data = data.train, J = 5, method = "cc")
pred <- predict(fit, data.test)
mean(pred$class != data.test$classes)

## "hm1"
fit <- hmcc(classes ~ ., data = data.train, J = 3, method = "hm1")
pred <- predict(fit, data.test)
mean(pred$class != data.test$classes)

## "hm2"
fit <- hmcc(classes ~ ., data = data.train, J = 3, method = "hm2")
pred <- predict(fit, data.test)
mean(pred$class != data.test$classes)}
\alias{hmcc.formula}
\alias{hmcc.data.frame}
\alias{hmcc.matrix}
\alias{hmcc.default}

