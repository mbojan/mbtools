pcmodel <- princomp( USArrests, cor=TRUE )
pcmodel
anova(pcmodel) # variance prc.
