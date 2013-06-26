\name{mbrepo}
\alias{mbrepo}
\title{Get current URL to my R packages repository}
\usage{
  mbrepo(loc = c("3e", "icm"))
}
\arguments{
  \item{loc}{character, name of location}
}
\value{
  Character scalar with the URL to the top dir of my R
  package repository. Can be used with
  \code{\link{download.packages}},
  \code{\link{install.packages}} and alike.
}
\description{
  Returns a valid URL to my package repository.
}
\details{
  Because I was migrating quite a bit recently this
  function, when called with the default value of
  \code{loc}, should give the current and valid address to
  my R package repository. If the default value is not
  working alternative \code{loc}ations can be tried.
}
\examples{
\dontrun{
available.packages(contrib.url(mbrepo()))
}
}

