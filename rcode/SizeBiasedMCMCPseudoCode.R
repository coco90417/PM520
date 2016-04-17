# Size-biased sampling via MCMC sampling

Exponential<-function(dRate,x){
	return (dRate*exp(-dRate*x))
}

SizedBiasedMCMC<-function(Density, Param1,NoOfIts,StepSize,LowRange,HighRange){
	 #start somewhere
	 x<-runif(1,LowRange,HighRange)
	 Accepteds<-mat.or.vec(1,NoOfIts)
	 
	 # do Metropolis Algorithm
	 for (i in 1:NoOfIts){
	 	
	 	# propose new x
	 	xprime<-x+runif(1,-StepSize,StepSize)
		if (xprime> HighRange)
			xprime<-HighRange-(xprime-HighRange) # this treats the edge of the range as a 'reflecting boundary'.
		if (xprime< LowRange)
			xprime<-LowRange-(xprime-LowRange) # this treats the edge of the range as a 'reflecting boundary'.

		# Calculate acceptance prob min{1,q(x'->x)f(x')/q(x->x')f(x)} ...... here f(y)=y*Density(param1,y) 

    # Decide whether to move or not? 
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
SB<-SizedBiasedMCMC(Exponential,1,500000,0.9,0,10)
HSB<-hist(SB[1,],breaks=500)
plot(HSB$mids,HSB$density,pch='.',cex=3)
curve(x*exp(-x),add=TRUE,col="blue")