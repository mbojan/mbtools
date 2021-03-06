% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Inflacja.R
\docType{data}
\name{Inflacja}
\alias{Inflacja}
\title{Monthly inflation rates in Poland}
\format{
A data frame with 195 observations on the following 6 variables.
\describe{
\item{year}{Year}
\item{month}{Month}
\item{infl1}{a time-series, inflation as compared to the same month
of the previous year}
\item{infl2}{a time-series, inflation as
compared to the previous month}
\item{infl3}{a time-series,
inflation as compared to December of the previous year}
\item{infl4}{a time-series, average inflation of the previous 12
months}
}
}
\source{
(Polish) Central Statistical Office, http://www.stat.gov.pl
}
\description{
Data on monthly inflation rates in Poland since January 1989 up to March
2005.
}
\examples{
data(Inflacja)

### transform inflX variables into time-series objects
Inflacja$infl1ts <- ts(Inflacja$infl1, start=c(1989,1), end=c(2005,3),
	freq=12)
Inflacja$infl2ts <- ts(Inflacja$infl2, start=c(1989,1), end=c(2005,3),
	freq=12)
Inflacja$infl3ts <- ts(Inflacja$infl3, start=c(1989,1), end=c(2005,3),
	freq=12)
Inflacja$infl4ts <- ts(Inflacja$infl4, start=c(1989,1), end=c(2005,3),
	freq=12)

### plot some...
plot( Inflacja$infl1ts, main="Inflation rates in Poland",
	ylab="Inflation")
lines( Inflacja$infl4ts, lty=2 )
legend( 1995, 1200,
	legend=c("compared to year ago","average of last 12 months"),
	lty=1:2 )

}
\keyword{datasets}
