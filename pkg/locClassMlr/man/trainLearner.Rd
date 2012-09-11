\name{trainLearner}
\alias{trainLearner}
\alias{trainLearner.classif.constant}
\alias{trainLearner.classif.dalda}
\alias{trainLearner.classif.dalr}
\alias{trainLearner.classif.damultinom}
\alias{trainLearner.classif.dannet}
\alias{trainLearner.classif.daqda}
\alias{trainLearner.classif.dasvm}
\alias{trainLearner.classif.FLXMCLconstant}
\alias{trainLearner.classif.FLXMCLlda}
\alias{trainLearner.classif.FLXMCLmultinom}
\alias{trainLearner.classif.FLXMCLnnet}
\alias{trainLearner.classif.FLXMCLqda}
\alias{trainLearner.classif.FLXMCLsvm}
\alias{trainLearner.classif.kda}
\alias{trainLearner.classif.kmc}
\alias{trainLearner.classif.mobConstantModel}
\alias{trainLearner.classif.mobLdaModel}
\alias{trainLearner.classif.mobMultinomModel}
\alias{trainLearner.classif.mobNnetModel}
\alias{trainLearner.classif.mobQdaModel}
\alias{trainLearner.classif.mobSvmModel}
\alias{trainLearner.classif.oslda}
\alias{trainLearner.classif.osmultinom}
\alias{trainLearner.classif.osnnet}
\alias{trainLearner.classif.osqda}
\alias{trainLearner.classif.ossvm}
\alias{trainLearner.classif.wlda}
\alias{trainLearner.classif.wqda}
\alias{trainLearner.classif.wsvm}
\title{Methods for Training a \code{\link[mlr]{Learner}} Object}
\usage{
  \method{trainLearner}{classif.constant} (.learner, .task,
    .subset, ...)

  \method{trainLearner}{classif.dalda} (.learner, .task,
    .subset, ...)

  \method{trainLearner}{classif.dalr} (.learner, .task,
    .subset, ...)

  \method{trainLearner}{classif.damultinom} (.learner,
    .task, .subset, ...)

  \method{trainLearner}{classif.dannet} (.learner, .task,
    .subset, ...)

  \method{trainLearner}{classif.daqda} (.learner, .task,
    .subset, ...)

  \method{trainLearner}{classif.dasvm} (.learner, .task,
    .subset, ...)

  \method{trainLearner}{classif.FLXMCLconstant} (.learner,
    .task, .subset, ...)

  \method{trainLearner}{classif.classif.FLXMCLlda}
    (.learner, .task, .subset, ...)

  \method{trainLearner}{classif.FLXMCLmultinom} (.learner,
    .task, .subset, ...)

  \method{trainLearner}{classif.FLXMCLnnet} (.learner,
    .task, .subset, ...)

  \method{trainLearner}{classif.FLXMCLqda} (.learner,
    .task, .subset, ...)

  \method{trainLearner}{classif.FLXMCLsvm} (.learner,
    .task, .subset, ...)

  \method{trainLearner}{classif.kda} (.learner, .task,
    .subset, ...)

  \method{trainLearner}{classif.kmc} (.learner, .task,
    .subset, ...)

  \method{trainLearner}{classif.mobConstantModel}
    (.learner, .task, .subset, ...)

  \method{trainLearner}{classif.mobLdaModel} (.learner,
    .task, .subset, ...)

  \method{trainLearner}{classif.mobMultinomModel}
    (.learner, .task, .subset, ...)

  \method{trainLearner}{classif.mobNnetModel} (.learner,
    .task, .subset, ...)

  \method{trainLearner}{classif.mobQdaModel} (.learner,
    .task, .subset, ...)

  \method{trainLearner}{classif.mobSvmModel} (.learner,
    .task, .subset, ...)

  \method{trainLearner}{classif.oslda} (.learner, .task,
    .subset, ...)

  \method{trainLearner}{classif.osmultinom} (.learner,
    .task, .subset, ...)

  \method{trainLearner}{classif.osnnet} (.learner, .task,
    .subset, ...)

  \method{trainLearner}{classif.osqda} (.learner, .task,
    .subset, ...)

  \method{trainLearner}{classif.ossvm} (.learner, .task,
    .subset, ...)

  \method{trainLearner}{classif.wlda} (.learner, .task,
    .subset, ...)

  \method{trainLearner}{classif.wqda} (.learner, .task,
    .subset, ...)

  \method{trainLearner}{classif.wsvm} (.learner, .task,
    .subset, ...)
}
\arguments{
  \item{.learner}{[\code{\link[mlr]{RLearner}}]\cr Wrapped
  learner.}

  \item{.task}{[\code{\link[mlr]{SupervisedTask}}]\cr Task
  to train learner on.}

  \item{.subset}{[\code{integer}]\cr Subset of cases for
  training set, index the task with this.  You probably
  want to use \code{\link[mlr]{getTaskData}} for this
  purpose.}

  \item{\dots}{[any]\cr Additional (hyper)parameters, which
  need to be passed to the underlying train function.}
}
\value{
  [any] Model of the underlying learner.
}
\description{
  Methods for training a \code{\link[mlr]{Learner}} object.
}

