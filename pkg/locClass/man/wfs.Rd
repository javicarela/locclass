\name{wfs}
\alias{biweight}
\title{Generation of Window Functions}
\usage{biweight(bw, k)
cauchy(bw, k, nn.only=TRUE)
cosine(bw, k)
epanechnikov(bw, k)
exponential(bw, k, nn.only=TRUE)
gaussian(bw, k, nn.only=TRUE)
optcosine(bw, k)
rectangular(bw, k)
triangular(bw, k)
}
\description{The window function generating functions that can are used in various local classification methods.}
\details{\code{biweight}: The window function generating functions are used to initialize a window function. These functions can be passed as
\code{wf} argument to \code{\link{dalda}}, \code{\link{dalr}}, \code{\link{kmc}} or \code{\link{oslda}}.

If only \code{bw} is given a window function with fixed bandwidth is returned.

If only \code{k} is given a window function with \code{k} nearest neighbors bandwidth, i.e. adaptive to the local density of data points, is generated.
In case of window functions with infinite support, \code{"cauchy"}, \code{"exponential"} and \code{"gaussian"}, the argument \code{nn.only} is used to decide 
if only the \code{k} nearest neighbors or all observations receive positive weights.

If \code{bw} and \code{k} are both specified, a window function with fixed bandwidth is generated and all weights are set to zero except for
the \code{k} nearest neighbors.

Parts of the source code are based on the function \link[stats]{density} in package stats. Concerning the \code{"cosine"} and \code{"optcosine"} windows, it applies
what is said in the documentation of \link[stats]{density}: \code{"cosine"} is smoother than \code{"optcosine"}, which is the usual 'cosine' kernel in the literature.
\code{"cosine"} is the version used by S.

}
\value{\code{biweight}: Returns an object of class \code{"function"}. The resulting \code{function} implements the desired window function and depends on one
argument \code{x} that is usually some sort of distance.
The returned function has several attributes, depending on which arguments are specified.
\describe{
\item{\code{"name"}}{The name of the window function.}
\item{\code{"bw"}}{(If corresponding argument is given.) The chosen bandwidth.}
\item{\code{"k"}}{(If corresponding argument is given.) The chosen number of nearest neighbors.}
\item{\code{"nn.only"}}{(Logical. Only if \code{k} was specified.) \code{TRUE} if only the k nearest neighbors are used. 
(\code{nn.only} is always \code{TRUE} except for window functions with infinite support.)}
\item{\code{"adaptive"}}{(Logical.) \code{TRUE} in case of an adaptive bandwidth, \code{FALSE} if the bandwidth is fixed.}
}

}
\seealso{\code{\link{dalda}}, \code{\link{dalr}}, \code{\link{kmc}}, \code{\link{oslda}}, \link[stats]{density}.}
\alias{wfs}
\alias{biweight}
\alias{cauchy}
\alias{cosine}
\alias{epanechnikov}
\alias{exponential}
\alias{gaussian}
\alias{optcosine}
\alias{rectangular}
\alias{triangular}
\arguments{\item{bw}{The bandwidth parameter.}
\item{k}{The number of nearest neighbors.}
\item{nn.only}{(Logical. Only required for window functions with infinite support.) Should only the k nearest neighbors or all observations receive positive weights? Defaults to \code{TRUE}.}
}
\examples{## fixed bandwidth
gwf <- gaussian(bw = 1)
gwf

## adaptive bandwidth, only the 100 nearest neighbors receive positive weights
gwf <- gaussian(k = 100)
gwf
gwf(1:150)

## adaptive bandwidth, all observations receive positive weights
gwf <- gaussian(k = 100, nn.only = FALSE)
gwf
gwf(1:150)

## fixed bandwidth, only the 100 nearest neighbors receive positive weights
gwf <- gaussian(k = 100, bw = 1)
gwf
gwf(1:150)}
\alias{cauchy}
\alias{cosine}
\alias{epanechnikov}
\alias{exponential}
\alias{gaussian}
\alias{optcosine}
\alias{rectangular}
\alias{triangular}

