#' Test if a degree sequence isGraphical
#' 
#' Given a vector of integer values check whether it is \emph{graphical}, i.e.
#' whether there exists an undirected graph with a corresponding degree
#' sequence.
#' 
#' A sequence of non-negative integers \eqn{a_1, a_2, ..., a_p}{a[1], a[2],
#' ..., a[p]}. is \emph{graphical} if there exists a graph with the
#' corresponding degree sequence.
#' 
#' This function takes such a sequence as an argument \code{x} and performes
#' the check.
#' 
#' Two algorithms are implemented. The first one due too Erdos and Gallai
#' (1960), the second, a bit more efficient and the default, is due too
#' Tripathi and Vijay (2003). The method can be selected with the \code{method}
#' argument.
#' 
#' @param x integer vector, degree sequence to be tested
#' @param method character, type of algorithm to be used, defaults to "tv", see
#' Details
#'
#' @return Logical, whether the sequence is graphical or not.
#'
#' @references
#' P. Erdos, T. Gallai (1960) Graphs with prescribed degree of vertices
#' (Hungarian), Mat. Lapok 11 264--274.
#' 
#' A. Tripathi and Sujith Vijay (2003) A note on a theorem of Erdos and Gallai,
#' Discrete Mathematics 265 417--420
#'
#' @export
#'
#' @example examples/isGraphical.R
#'
isGraphical <- function(x, method=c("tv", "eg"))
{
    m <- match.arg(method)
    switch(m,  eg = graphicalEG(x),   tv = graphicalTV(x) )
}

# check the inequality
# \sum a_k \leq n(n-1) \sum_{k=n+1}^p \min( n, a_k )
# a=vector, n=scalar
graphicalIneq <- function(n, a)
{
    # left-hand side
    left.s <- sum( a[seq(1,n)] )
    # vector of minimums from right-hand side
    m <- pmin( a[seq(n+1,length(a))], rep(n,length(a)-n) )
    # check inequality
    left.s <= n*(n-1) + sum(m)
}

# based on Erdos-Gallai
graphicalEG <- function(a)
{
    if(!is.numeric(a))
	stop("'a' must be numeric, got", data.class(a) )
    a <- sort(a, decreasing=TRUE)
    # sum of degrees must be even
    sm <- sum(a)
    if( sm %% 2 )
	return(FALSE)
    # vector of n's
    n <- seq(1, length(a)-1 )
    # for every 'n' check the inequality
    rval <- sapply(n, function(i) graphicalIneq(n=i, a=a))
    if(all(rval))
	return(TRUE)
    else
	return(FALSE)
}


# version based on Tripati and Vijay
graphicalTV <- function(a)
{
    if(!is.numeric(a))
	stop("'a' must be numeric, got", data.class(a) )
    a <- sort(a, decreasing=TRUE)
    # sum of degrees divisible by 2
    sm <- sum(a)
    if( sm %% 2 )
	return(FALSE)
    s <- a - seq(along=a) + 1
    s <- max(which(s>=0))
    n <- seq(1, min(s, length(a)-1 ) )
    # for every 'n' check the inequality
    rval <- sapply(n, function(i) graphicalIneq(n=i, a=a))
    if(all(rval))
	return(TRUE)
    else
	return(FALSE)
}







 
 
# some testing
# TODO move to tests
if(FALSE)
{
    testit <- function(d)
    {
	rval <- c( eg = isGraphical(d, "eg"),
	    tv = isGraphical(d, "tv") )
	if(!all(rval))
	{
	    print(rval)
	    stop("failed")
	}
	rval
    }
    # graphs of size 3
    testit( c(0,0,0) )
    testit( c(0,1,1) )
    testit( c(1,2,1) )
    testit( c(2,2,2) )



    # compare speeds for random graphs of sizes from 20 to 200
    if(require(sna))
    {
	n <- 20
	s <- seq( 20, 500, by=20 )
	r <- matrix(NA, nrow=length(s), ncol=2)
	for( i in seq(along=s) )
	{

	    g <- rgraph(s[i])
	    d <- degree(g) / 2
	    library(bojan)
	    o <- speedcomp( e1=expression(isGraphical(d, "eg")),
		e2=expression(isGraphical(d, "tv")) )
	    r[i,] <- o@timings[, "elapsed"]
	}
	plot( range(s), range(r), type="n", main="Speed comparison of E-G and T-V",
	    xlab="Network size", ylab="Time [10ms]")
	lines(s, r[,1], pch="EG", type="b", cex=.6, col="red")
	lines(s, r[,2], pch="TV", type="b", cex=.6)
	# E-G a little-bit slower
	summary( r[1,] - r[2,] )
    }
}
