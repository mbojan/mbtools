\name{ncinfo}
\docType{methods}
\alias{ncinfo}
\alias{ncinfo-methods}
\alias{ncinfo,ANY-method}
\alias{ncinfo,character-method}

\title{Summary information about NetCDF file}
\description{
This generic function returns a summary information about a NetCDF file like
the list of dimensions, defined variables etc.
}
\usage{
ncinfo(object)
}
\arguments{
  \item{object}{object of class "RNetCDF", or a name of the file}
}
\details{
This function relies on package \pkg{RNetCDF}.

Currently there are two methods supported. 

The \code{object} can be a file handle (object of class "RNetCDF")
or a file name. See examples.
}
\value{
A list of components \code{dimensions} and \code{variables}:
  \item{dimensions}{a matrix with columns "id", "name", "length", and "unlim";
  and rows for all the dimensions defined in the file.}
  \item{variables}{a matrix with columns "id", "name", "type", "ndims",
  "dimids", and "natts" and rows for all variables defined in the file.}
}
\seealso{Accessing NetCDF files with \pkg{RNetCDF}}
\examples{
## This is based on examples from the help page on 'var.put.nc' from
## package RNetCDF
if(require(RNetCDF))
{
  # temporary file
  tfile <- tempfile()

  nc <- create.nc(tfile)

  # define dimensions
  dim.def.nc(nc, "station", 5)
  dim.def.nc(nc, "time", unlim=TRUE)
  dim.def.nc(nc, "max_string_length", 32)

  #  Create three variables, one as coordinate variable
  var.def.nc(nc, "time", "NC_INT", "time")
  var.def.nc(nc, "temperature", "NC_DOUBLE", c(0,1))
  var.def.nc(nc, "name", "NC_CHAR", c("max_string_length", "station"))

  #  Put some missing_value attribute for temperature
  att.put.nc(nc, "temperature", "missing_value", "NC_DOUBLE", -99999.9)

  #  Define variable values
  mytime        <- c(1:2)
  mytemperature <- c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, NA, NA, 9.9)
  myname        <- c("alfa", "bravo", "charlie", "delta", "echo")
  dim(mytemperature) <- c(5,2)

  #  Put the data with indicated start/count
  var.put.nc(nc, "time", mytime, 1, length(mytime))
  var.put.nc(nc, "temperature", mytemperature, c(1,1), c(5,2))
  var.put.nc(nc, "name", myname, c(1,1), c(32,5))

  sync.nc(nc)

  #  Put the data with default start/count
  var.put.nc(nc, "time", mytime)
  var.put.nc(nc, "temperature", mytemperature)
  var.put.nc(nc, "name", myname)

  # close the file
  close.nc(nc)
  rm(nc)

  ### Now use the 'ncinfo' function to get the information
  # from netcdf handle object
  nc <- open.nc(tfile)
  ncinfo(nc)
  close.nc(nc)
  rm(nc)

  # directly from file
  ncinfo(tfile)

  ### Clean-up
  unlink(tfile)
  rm(tfile)
}
}
\keyword{file}
