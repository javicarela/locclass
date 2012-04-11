\name{bayes}
\alias{bayes}
\alias{bayes.locClass.flashData}
\alias{bayes.locClass.hvData}
\alias{bayes.locClass.hvQuadraticData}
\alias{bayes.locClass.mixtureData}
\alias{bayes.locClass.outlierCorrectData}
\alias{bayes.locClass.outlierWrongData}
\alias{bayes.locClass.ringData}
\alias{bayes.locClass.spiralData}
\alias{bayes.locClass.twonormLinearData}
\alias{bayes.locClass.twonormQuadraticData}
\alias{bayes.locClass.vData}
\alias{bayes.locClass.vNormalData}
\alias{bayes.locClass.vNormalLinearData}
\alias{bayes.locClass.vNormalQuadraticData}
\alias{bayes.locClass.wData}
\alias{bayes.locClass.xor3Data}
\alias{bayes.locClass.xorData}
\title{Calculate Posterior Probabilities and Bayes Predictions}
\usage{
  bayes(object, ...)

  \method{bayes}{locClass.flashData} (object, ...)

  \method{bayes}{locClass.hvData} (object, ...)

  \method{bayes}{locClass.hvQuadraticData} (object, ...)

  \method{bayes}{locClass.mixtureData} (object, ...)

  \method{bayes}{locClass.outlierCorrectData} (object, ...)

  \method{bayes}{locClass.outlierWrongData} (object, ...)

  \method{bayes}{locClass.ringData} (object, ...)

  \method{bayes}{locClass.spiralData} (object, ...)

  \method{bayes}{locClass.twonormLinearData} (object, ...)

  \method{bayes}{locClass.twonormQuadraticData} (object,
    ...)

  \method{bayes}{locClass.vData} (object, ...)

  \method{bayes}{locClass.vNormalLinearData} (object, ...)

  \method{bayes}{locClass.vNormalQuadraticData} (object,
    ...)

  \method{bayes}{locClass.wData} (object, ...)

  \method{bayes}{locClass.xor3Data} (object, ...)

  \method{bayes}{locClass.xorData} (object, ...)
}
\arguments{
  \item{object}{An object of class \code{"locClass"}.}

  \item{\dots}{Currently unused.}
}
\value{
  A list with components: \item{posterior}{A matrix of
  posterior probabilities.} \item{ybayes}{A vector of Bayes
  predictions.}
}
\description{
  Calculate posterior probabilities and Bayes predictions.
}

