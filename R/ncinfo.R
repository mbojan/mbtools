# Gather some information on NetCDF file

setGeneric("ncinfo", function(object) standardGeneric("ncinfo"))


setMethod("ncinfo", "ANY",
function(object)
{
    # require(RNetCDF)
    if(data.class(object) != "NetCDF")
	stop("'object' must be of class 'NetCDF', got", data.class(object))
    nc <- object
    finfo <- file.inq.nc(nc)
    # dimension table
    dimtable <- sapply( seq(0, length=finfo$ndims),
	function(x) dim.inq.nc(nc, x))
    dimnames(dimtable)[[2]] <- dimtable["name",]
    vartable <- sapply( seq(0, length=finfo$nvars),
	function(x) 
	{
	    val <- var.inq.nc(nc, x)
	    if(length(val$dimids) > 1)
		val$dimids <- paste(val$dimids, collapse=", ")
	    val
	} )
    dimnames(vartable)[[2]] <- vartable["name",]
    list(dimensions=t(dimtable), variables=t(vartable) )
} )


setMethod("ncinfo", "character",
function(object)
{
    # require(RNetCDF)
    nc <- open.nc(object, write=FALSE)
    rval <- ncinfo(nc)
    close.nc(nc)
    rval
} )

