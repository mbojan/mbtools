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

