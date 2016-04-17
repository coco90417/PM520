# Size-biased sampling via MCMC

Exponential<-function(dRate,x){
	return (dRate*exp(-dRate*x))
}

TruncatedNormal<-function(dMean,x){
  y<- -1
  while (y<0){y<-dnorm(x,dMean,1)}
  return (y)   # assume an SD of 1, and truncation at 0 for now
}

SizedBiasedMCMC<-function(Density, Param1,NoOfIts,StepSize,LowRange,HighRange){
	 #start somewhere
	 x<-runif(1,LowRange,HighRange)
	 Accepteds<-mat.or.vec(1,NoOfIts)
	 
	 # do the MCMC
	 for (i in 1:NoOfIts){
	 	
	 	# propose new x
	 	xprime<-x+runif(1,-StepSize,StepSize)
		if (xprime> HighRange)
			xprime<-HighRange-(xprime-HighRange) # this treats the edge of the range as a 'reflecting boundary'.
		if (xprime< LowRange)
			xprime<-LowRange-(xprime-LowRange) # this treats the edge of the range as a 'reflecting boundary'.

		# Calculate Hastings Ratio - the Q term will disappear
		Paccept<-min(1,(xprime*Density(Param1,xprime))/(x*Density(Param1,x)))
		#Paccept<-min(1,(Density(Param1,xprime))/(Density(Param1,x)))
		#cat("\n",paste(xprime,"  ",x,"  ",Density(Param1,xprime),"  ",Density(Param1,x)),"   ",Paccept)
		#cat(Paccept)
		#cat("\n")
		# move or not? 
		p<-runif(1)
		if (p<Paccept)
		{
			x<-xprime
		}
		# update the vector of states
		Accepteds[i]<-x
	 }
	hist(Accepteds,breaks=50)
	return (Accepteds)
}

# try the following
SB<-SizedBiasedMCMC(Exponential,1,100000,1,0,10)
HSB<-hist(SB[1,],breaks=500)
plot(HSB$mids,HSB$density,pch='.',cex=3)
curve(x*exp(-x),add=TRUE,col="blue")

SB2<-SizedBiasedMCMC(Exponential,1,100000,1,0,10)
HSB2<-hist(SB2[1,],breaks=500)
plot(HSB2$mids,HSB2$density,pch='.',cex=3)
curve(x*exp(-x),add=TRUE,col="blue")


# Gelman code
library("coda")

# convert to mcmc objects, with a burn-in - the Gelman routine needs them as column vectors (each variable in a column)
# so we need to transpose them
MCMC1<-mcmc(t(SB),start=1000)
MCMC2<-mcmc(t(SB2),start=1000)

# combine different mcmc chain objects to a mcmc list.
Combined<-mcmc.list(list(MCMC1,MCMC2))

# gelman functions are 
gelman.plot(Combined) # for plots
gelman.diag(Combined) # for diagnostic values
