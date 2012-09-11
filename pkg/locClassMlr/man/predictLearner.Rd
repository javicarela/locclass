\name{predictLearner}
\alias{predictLearner}
\alias{predictLearner.classif.constant}
\alias{predictLearner.classif.dalda}
\alias{predictLearner.classif.dalr}
\alias{predictLearner.classif.damultinom}
\alias{predictLearner.classif.dannet}
\alias{predictLearner.classif.daqda}
\alias{predictLearner.classif.dasvm}
\alias{predictLearner.classif.FLXMCLconstant}
\alias{predictLearner.classif.FLXMCLlda}
\alias{predictLearner.classif.FLXMCLmultinom}
\alias{predictLearner.classif.FLXMCLnnet}
\alias{predictLearner.classif.FLXMCLqda}
\alias{predictLearner.classif.FLXMCLsvm}
\alias{predictLearner.classif.kda}
\alias{predictLearner.classif.kmc}
\alias{predictLearner.classif.mobConstantModel}
\alias{predictLearner.classif.mobLdaModel}
\alias{predictLearner.classif.mobMultinomModel}
\alias{predictLearner.classif.mobNnetModel}
\alias{predictLearner.classif.mobQdaModel}
\alias{predictLearner.classif.mobSvmModel}
\alias{predictLearner.classif.oslda}
\alias{predictLearner.classif.osmultinom}
\alias{predictLearner.classif.osnnet}
\alias{predictLearner.classif.osqda}
\alias{predictLearner.classif.ossvm}
\alias{predictLearner.classif.wlda}
\alias{predictLearner.classif.wqda}
\alias{predictLearner.classif.wsvm}
\title{Methods for Predicting a \code{\link[mlr]{Learner}} Object}
\usage{
  \method{predictLearner}{classif.constant} (.learner,
    .model, .newdata, ...)

  \method{predictLearner}{classif.dalda} (.learner, .model,
    .newdata, ...)

  \method{predictLearner}{classif.dalr} (.learner, .model,
    .newdata, ...)

  \method{predictLearner}{classif.damultinom} (.learner,
    .model, .newdata, ...)

  \method{predictLearner}{classif.dannet} (.learner,
    .model, .newdata, ...)

  \method{predictLearner}{classif.daqda} (.learner, .model,
    .newdata, ...)

  \method{predictLearner}{classif.dasvm} (.learner, .model,
    .newdata, ...)

  \method{predictLearner}{classif.FLXMCLconstant}
    (.learner, .model, .newdata, ...)

  \method{predictLearner}{classif.FLXMCLlda} (.learner,
    .model, .newdata, ...)

  \method{predictLearner}{classif.FLXMCLmultinom}
    (.learner, .model, .newdata, ...)

  \method{predictLearner}{classif.FLXMCLnnet} (.learner,
    .model, .newdata, ...)

  \method{predictLearner}{classif.FLXMCLqda} (.learner,
    .model, .newdata, ...)

  \method{predictLearner}{classif.FLXMCLsvm} (.learner,
    .model, .newdata, ...)

  \method{predictLearner}{classif.kda} (.learner, .model,
    .newdata, ...)

  \method{predictLearner}{classif.kmc} (.learner, .model,
    .newdata, ...)

  \method{predictLearner}{classif.mobConstantModel}
    (.learner, .model, .newdata, ...)

  \method{predictLearner}{classif.mobLdaModel} (.learner,
    .model, .newdata, ...)

  \method{predictLearner}{classif.mobMultinomModel}
    (.learner, .model, .newdata, ...)

  \method{predictLearner}{classif.mobNnetModel} (.learner,
    .model, .newdata, ...)

  \method{predictLearner}{classif.mobQdaModel} (.learner,
    .model, .newdata, ...)

  \method{predictLearner}{classif.mobSvmModel} (.learner,
    .model, .newdata, ...)

  \method{predictLearner}{classif.oslda} (.learner, .model,
    .newdata, ...)

  \method{predictLearner}{classif.osmultinom} (.learner,
    .model, .newdata, ...)

  \method{predictLearner}{classif.osnnet} (.learner,
    .model, .newdata, ...)

  \method{predictLearner}{classif.osqda} (.learner, .model,
    .newdata, ...)

  \method{predictLearner}{classif.ossvm} (.learner, .model,
    .newdata, ...)

  \method{predictLearner}{classif.wlda} (.learner, .model,
    .newdata, ...)

  \method{predictLearner}{classif.wqda} (.learner, .model,
    .newdata, ...)

  \method{predictLearner}{classif.wsvm} (.learner, .model,
    .newdata, ...)
}
\arguments{
  \item{.learner}{[\code{\link[mlr]{RLearner}}]\cr Wrapped
  learner.}

  \item{.model}{[\code{\link[mlr]{WrappedModel}}]\cr Model
  produced by training.}

  \item{.newdata}{[\code{data.frame}]\cr New data to
  predict. Does not include target column.}

  \item{\dots}{[any]\cr Additional parameters, which need
  to be passed to the underlying predict function.}
}
\value{
  Either a factor for type \dQuote{response} or a matrix
  for type \dQuote{prob}. In the latter case the columns
  must be named with the class labels.
}
\description{
  Methods for predicting a \code{\link[mlr]{Learner}}
  object.
}

