\name{bivar}
\alias{bivar}
\title{Bias-Variance Decomposition of the Misclassification Rate}
\usage{bivar(y, grouping, ybayes, posterior)
}
\description{Compute the bias-variance decomposition of the misclassification rate according to the approaches of James (2003) and Domingos (2000).}
\details{If \code{posterior} is specified, \code{ybayes} is calculated from the posterior probabilities and the posteriors are used to estimate noise, 
error, systematic effect and variance effect.
If \code{ybayes} is specified it is ignored if the posteriors are given. Otherwise the empirical distribution of \code{ybayes} is inferred and used
to calculate the quantities of interest.
If neither \code{posterior} nor \code{ybayes} are specified is assumed that the noise level is zero and 
the remaining quantities are calculated based on this assumption.}
\value{A \code{data.frame} containing the following columns:
\item{error}{Estimated misclassification probability.}
\item{noise}{(Only if \code{ybayes} or \code{posterior} was specified.) Noise.}
\item{bias}{Bias.}
\item{variance}{Variance.}
\item{unbiased.variance}{Unbiased variance.}
\item{biased.variance}{Biased variance.}
\item{net.variance}{Pointwise net variance.}
\item{systematic.effect}{Systematic effect.}
\item{variance.effect}{Variance effect.}
\item{ymain}{Main prediction.}
\item{ybayes}{(Only if \code{ybayes} or \code{posterior} was specified.) The optimal prediction.}
\item{size}{Numeric vector of the same length as the number of test observations. The number of predictions made for each test observation.}}
\references{James, G. M. (2003). Variance and bias for general loss functions. \emph{Machine Learning}, \bold{51(2)} 115--135.

Domingos, P. (2000). A unified bias-variance decomposition for zero-one and squared loss. In
Proceedings of the Seventeenth National Conference on Artificial Intelligence and Twelfth Conference on 
Innovative Applications of Artificial Intelligence, pages 564--569. AAAI Press / The MIT Press.}
\arguments{\item{y}{Predicted class labels on a test data set based on multiple training data sets. \code{y} is supposed to be a list where each element contains 
the predictions for one single test observation. #factor???}
\item{grouping}{Vector of true class labels (a \code{factor}).}
\item{ybayes}{(Optional.) Bayes prediction. Not used if \code{posterior} is specified as \code{ybayes} can be easily calculated from the posterior probabilities.}
\item{posterior}{(Optional.) Matrix of posterior probabilities, either known or estimated. It is assumed that the columns are ordered according
to the factor levels of \code{grouping}.}
}

