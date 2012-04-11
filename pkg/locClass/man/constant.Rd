\name{constant}
\alias{constant}
\alias{constant.data.frame}
\alias{constant.default}
\alias{constant.formula}
\alias{constant.matrix}
\title{Constant Classifier}
\usage{
  constant(x, ...)

  \method{constant}{formula} (formula, data,
    weights = rep(1, nrow(data)), ..., subset, na.action)

  \method{constant}{data.frame} (x, ...)

  \method{constant}{matrix} (x, grouping,
    weights = rep(1, nrow(x)), ..., subset,
    na.action = na.fail)

  \method{constant}{default} (x, grouping,
    weights = rep(1, nrow(x)), ...)
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
  An object of class \code{"constant"}, a \code{list}
  containing the following components:
  \item{prior}{Weighted class prior probabilities.}
  \item{counts}{The number of observations per class.}
  \item{lev}{The class labels (levels of \code{grouping}).}
  \item{N}{The number of observations.} \item{weights}{The
  observation weights used in the fitting process.}
  \item{predictors}{The names of the predictor variables.}
  \item{call}{The (matched) function call.}
}
\description{
  A classifier that always predicts the class with the
  highest weighted prior probability.
}
\details{
  This function is rather a helper function needed to
  combine mixture models and recursive partitioning with a
  constant classifier. The weighted prior probabilities are
  calculated as: \deqn{p_g = \frac{\sum_{n:y_n=g}
  w_n}{\sum_n w_n}}{p_g = \sum_{n:y_n=g} w_n/\sum_n w_n}
}
\examples{
library(mlbench)
data(PimaIndiansDiabetes)

train <- sample(nrow(PimaIndiansDiabetes), 500)

# weighting observations from classes pos and neg according to their
# frequency in the data set:
ws <- as.numeric(1/table(PimaIndiansDiabetes$diabetes)
    [PimaIndiansDiabetes$diabetes])

fit <- constant(diabetes ~ ., data = PimaIndiansDiabetes, weights = ws,
    subset = train)
pred <- predict(fit, newdata = PimaIndiansDiabetes[-train,])
mean(pred$class != PimaIndiansDiabetes$diabetes[-train])
}
\seealso{
  \code{\link{predict.constant}}.
}
\keyword{classif}

