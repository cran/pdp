% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdp-package.R
\docType{package}
\name{pdp-package}
\alias{pdp}
\alias{pdp-package}
\title{pdp: A general framework for constructing partial dependence (i.e., marginal
effect) plots from various types machine learning models in R.}
\description{
Partial dependence plots (PDPs) help visualize the relationship between a
subset of the features (typically 1-3) and the response while accounting for
the average effect of the other predictors in the model. They are
particularly effective with black box models like random forests and support
vector machines.
}
\details{
The development version can be found on GitHub: https://github.com/bgreenwell/pdp.
As of right now, \code{pdp} exports four functions:
\itemize{
  \item{\code{partial}} - construct partial dependence functions (i.e., objects of class \code{"partial"}) from various fitted model objects;
  \item{\code{plotPartial}} - plot partial dependence functions (i.e., objects of class \code{"partial"}) using \code{\link[lattice]{lattice}} graphics;
  \item{\code{autoplot}} - plot partial dependence functions (i.e., objects of class \code{"partial"}) using \code{\link[ggplot2]{ggplot2}} graphics;
  \item{\code{topPredictors}} - extract most "important" predictors from various types of fitted models.
}
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/bgreenwell/pdp}
  \item \url{http://bgreenwell.github.io/pdp/}
  \item Report bugs at \url{https://github.com/bgreenwell/pdp/issues}
}

}
\author{
\strong{Maintainer}: Brandon M. Greenwell \email{greenwell.brandon@gmail.com} (\href{https://orcid.org/0000-0002-8120-0084}{ORCID})

}
\keyword{internal}
