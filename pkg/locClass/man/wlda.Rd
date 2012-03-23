\name{wlda}
\alias{wlda}
\alias{wlda.data.frame}
\alias{wlda.default}
\alias{wlda.formula}
\alias{wlda.matrix}
\title{Weighted Linear Discriminant Analysis}
\usage{
  wlda(x, ...)

  \method{wlda}{formula} (formula, data,
    weights = rep(1, nrow(data)), ..., subset, na.action)

  \method{wlda}{data.frame} (x, ...)

  \method{wlda}{matrix} (x, grouping,
    weights = rep(1, nrow(x)), ..., subset,
    na.action = na.fail)

  \method{wlda}{default} (x, grouping,
    weights = rep(1, nrow(x)),
    method = c("unbiased", "ML"), ...)
}
\arguments{
  \item{formula}{A \code{formula} of the form \code{groups
  ~ x1 + x2 + \dots}, that is, the response is the grouping
  \code{factor} and the right hand side specifies the
  (non-\code{factor}) discriminators.}

  \item{data}{A \code{data.frame} from which variables
  specified in \code{formula} are to be taken.}

  \item{x}{(Required if no \code{formula} is given as
  principal argument.) A \code{matrix} or \code{data.frame}
  or \code{Matrix} containing the explanatory variables.}

  \item{grouping}{(Required if no \code{formula} is given
  as principal argument.) A \code{factor} specifying the
  class membership for each observation.}

  \item{weights}{Observation weights to be used in the
  fitting process, must be larger or equal to zero.}

  \item{method}{Method for scaling the pooled weighted
  covariance matrix, either \code{"unbiased"} or
  maximum-likelihood (\code{"ML"}). Defaults to
  \code{"unbiased"}.}

  \item{...}{Further arguments.}

  \item{subset}{An index vector specifying the cases to be
  used in the training sample. (NOTE: If given, this
  argument must be named.)}

  \item{na.action}{A function to specify the action to be
  taken if NAs are found. The default action is first the
  \code{na.action} setting of \code{\link{options}} and
  second \code{\link{na.fail}} if that is unset. An
  alternative is \code{\link{na.omit}}, which leads to
  rejection of cases with missing values on any required
  variable. (NOTE: If given, this argument must be named.)}
}
\value{
  An object of class \code{"wlda"}, a \code{list}
  containing the following components:
  \item{prior}{Weighted class prior probabilities.}
  \item{counts}{The number of observations per class.}
  \item{means}{Weighted estimates of class means.}
  \item{cov}{Weighted estimate of the pooled class
  covariance matrix.} \item{lev}{The class labels (levels
  of \code{grouping}).} \item{N}{The number of
  observations.} \item{weights}{The observation weights
  used in the fitting process.} \item{method}{The method
  used for scaling the pooled weighted covariance matrix.}
  \item{call}{The (matched) function call.}
}
\description{
  A version of Linear Discriminant Analysis that can deal
  with observation weights.
}
\details{
  The formulas for the weighted estimates of the class
  means, the covariance matrix and the class priors are as
  follows:

  Normalized weights: if \eqn{x_n} is in class g, i. e.
  \eqn{y_n = g} \deqn{w_n^* = \frac{w_n}{\sum_{n:y_n=g}
  w_n}}{w_n* = w_n/sum_{n:y_n=g} w_n} Weighted class means:
  \deqn{\bar x_g = \sum_{n:y_n=g} w_n^* x_n}{bar x_g =
  sum_{n:y_n=g} w_n* x_i} Pooled weighted class covariance
  matrix: \deqn{S_g = \sum_{n:y_n=g} w_n^* (x_n - \bar
  x_g)(x_n - \bar x_g)'}{S_g = sum_{n:y_n=g} w_n* (x_n -
  bar x_g)(x_n - bar x_g)'} \code{method = "ML"}: \deqn{S =
  \sum_g p_g S_g}{S = sum_g p_g S_g} \code{method =
  "unbiased"}: \deqn{S = \frac{\sum_g p_g S_g}{1 - \sum_g
  p_g \sum_{n:y_n=g} w_n^{*2}}}{S = sum_g p_g S_g/(1 -
  sum_g p_g sum_{n:y_n=g} w_n*^2)} Weighted prior
  probabilities: \deqn{p_g = \frac{\sum_{n:y_n=g}
  w_n}{\sum_n w_n}}{p_g = \sum_{n:y_n=g} w_n/\sum_n w_n}

  If the predictor variables include factors, the formula
  interface must be used in order to get a correct model
  matrix.
}
\examples{
library(mlbench)
data(PimaIndiansDiabetes)

train <- sample(nrow(PimaIndiansDiabetes), 500)

# weighting observations from classes pos and neg according to their
# frequency in the data set:
ws <- as.numeric(1/table(PimaIndiansDiabetes$diabetes)
    [PimaIndiansDiabetes$diabetes])

fit <- wlda(diabetes ~ ., data = PimaIndiansDiabetes, weights = ws,
    subset = train)
pred <- predict(fit, newdata = PimaIndiansDiabetes[-train,])
mean(pred$class != PimaIndiansDiabetes$diabetes[-train])
}
\seealso{
  \code{\link{predict.wlda}} and \code{\link{dalda}} which
  is based on \code{\link{wlda}}.
}
\keyword{classif}
\keyword{multivariate}

