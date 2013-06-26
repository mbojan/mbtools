# Given a vector (with probably a lot of unique values) create a set of vectors
# that can be used as plotting attributes. For example, return a vector of
# colors, vector of shapes etc.



#' Map a vector to several plotting attributes
#' 
#' Given a vector with likely high number of unique values create a list of
#' vectors which combinations cover the unique values of the input vector.
#' 
#' 
#' @aliases attributize attributize.default attributize.data.frame
#' @param x,vec input vector
#' @param na.last logical, passed to \code{sort}
#' @param vcol numeric vector, column id with input vector values
#' @param \dots other arguments passed to/from other methods
#' @return data frame
#' @export attributize
attributize <- function(x, ...) UseMethod("attributize")




#' @method attributize data.frame
#' @export
#' @rdname attributize
# given data frame that assignes unique values (last column) to attributes and
# a vector
attributize.data.frame <- function(x, vec, vcol=ncol(x), ...)
{
  stopifnot(is.numeric(vcol))
  r <- lapply(x[,-vcol], function(a) a[match(vec, x[[vcol]])])
  structure( list(a=r, d=x, v=vec), class="attributize")
}



#' @method attributize default
#' @export
#' @rdname attributize
# given a vector
attributize.default <- function(x, ..., na.last=FALSE, vcol=ncol(d))
{
  u <- sort(unique(x), na.last=na.last)
  dots <- list(...)
  d <- do.call("expand.grid", c(dots, list(stringsAsFactors=FALSE)))
  if( length(u) > nrow(d) )
    stop(paste("not enough attribute combinations (", nrow(d),
               ") to cover unique values (", length(u), ")", sep=""))
  v <- rep(NA, nrow(d))
  v[1:length(u)] <- u
  a <- lapply(d, function(x) x[seq(along=v)])
  d <- cbind( as.data.frame(a, stringsAsFactors=FALSE), v, stringsAsFactors=FALSE)
  attributize.data.frame(d, vec=x, vcol=vcol)
}





if(FALSE)
{

library(RColorBrewer)
library(igraph)

palette(brewer.pal(8, "Accent"))

set.seed(1)
g <- barabasi.game(80, power=3/4)
g$layout <- layout.fruchterman.reingold(g)
V(g)$type <- letters[sample(c(1:15, NA), vcount(g), replace=TRUE)]
plot(g, vertex.size=7, edge.arrow.size=.3, vertex.label.cex=.7)

a <- attributize(V(g)$type, col=1:8, shape=c("circle", "square"))

labs <- V(g)$type
labs[is.na(labs)] <- "BD"

plot(g, vertex.shape=a$a$shape, vertex.color=a$a$col, vertex.size=7,
     edge.arrow.size=.3, vertex.label=labs)
legend("top", ncol=4, pch=c(22,21)[match(a$d$shape, a$a$shape)],
       pt.bg=a$d$col, pt.cex=3, legend=replace(a$d$v, 1, "BD"))

z <- a$d
z$col[1] <- 0

aa <- attributize(z, vec=V(g)$type)

plot(g, vertex.shape=aa$a$shape, vertex.color=aa$a$col, vertex.size=7,
     edge.arrow.size=.3, vertex.label=labs)
legend("top", ncol=4, pch=c(22,21)[match(aa$d$shape, aa$a$shape)],
       pt.bg=aa$d$col, pt.cex=3, legend=replace(aa$d$v, 1, "BD"))

}
