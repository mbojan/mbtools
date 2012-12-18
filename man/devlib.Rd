\name{devlib}
\alias{devlib}
\title{Load package from developer's library}
\description{
  Load package from developer's library.
}
\usage{
devlib(..., libloc=getOption("devlib"))
}
\arguments{
  \item{libloc}{Path to developer's library}
  \item{\dots}{other arguments passed to \code{\link{library}}}
}
\value{
  Nothing of interest, but the package is loaded.
}
\seealso{
  \code{\link{library}}, \code{\link{.libPaths}}
}
