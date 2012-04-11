\name{predict.constant}
\alias{predict.constant}
\title{Classify Multivariate Observations Based on the Constant Classifier}
\usage{
  \method{predict}{constant} (object, newdata, ...)
}
\arguments{
  \item{object}{Object of class \code{"constant"}.}

  \item{newdata}{A \code{data.frame} of cases to be
  classified or, if \code{object} has a \code{formula}, a
  \code{data.frame} with columns of the same names as the
  variables used.  A vector will be interpreted as a row
  vector.  If \code{newdata} is missing, an attempt will be
  made to retrieve the data used to fit the \code{constant}
  object.}

  \item{\dots}{Further arguments.}
}
\value{
  A list with components: \item{class}{The predicted class
  labels (a \code{factor}).} \item{posterior}{Matrix of
  class posterior probabilities.}
}
\description{
  Classify multivariate observations in conjunction with
  \code{\link{constant}}.
}
\details{
  This function is a method for the generic function
  \code{predict()} for class \code{"constant"}.  It can be
  invoked by calling \code{predict(x)} for an object
  \code{x} of the appropriate class, or directly by calling
  \code{predict.constant(x)} regardless of the class of the
  object.
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
  \code{\link{constant}}.
}
\keyword{classif}

