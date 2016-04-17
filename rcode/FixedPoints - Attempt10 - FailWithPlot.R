# Now we put a domain check in the function

# write the function sqrt(10*x+1)
F1<-function(x){
	if (x< -1){
	cat("Function not defined for x<-1. Exit.")
	return("NaN") }
		return(sqrt(10*x+1))
}

F2<-function(x){
			return(x+log(x)-exp(-x))
}

pdf("Fig3.pdf")
curve(sqrt(10*x+1),0,20,main="y=sqrt(10*x+1)")
abline(coef=c(0,1),col=4)
dev.off()

# function declaration
FixedPointFinder<-function(func,StartingValue,Tolerance,MaxNumberOfIterations,DrawLines){
	# set initial parameters (maximum number of steps; accuracy required; start 	point)
	#StartingXValue<- StartValue
	#MaxNumberOfIterations<-100
	#Tolerance<-1e-5
	Deviation<-1000   # an arbitrary big number
	i<-0
	Xprime<-StartingValue 
	#browser()
	#Set up a while loop until we hit the required target accuracy or the max. 	number of steps
	while ((i<MaxNumberOfIterations)&&(Deviation>Tolerance))
	{
		# Set x'=f(x)
		X<-Xprime # x is our current v-value
		Xprime<-func(X)
		if (Xprime=="NaN"){
			cat("Function not defined error.\n")
			break
		}
		if (DrawLines){
			segments(X,X,X,Xprime,col="blue",lty=2)
			segments(X,Xprime,Xprime,Xprime,col="blue",lty=2)
			}
		
		# calculate accuracy<- |f(x)-x|
		Deviation<-abs(Xprime-X)
		X<-Xprime
		i<-i+1
	}

	# output the result
	if (Deviation<Tolerance){
		cat(paste("Found the fixed point: ",X, "after ", i, "iterations"))
		}else{
		cat(paste("Convergence failure. Deviation: ",Deviation, "after ", i, 	"iterations"))}
}

pdf("Fig8.pdf")
curve(x+log(x)-exp(-x),0,20,main="y=x+log(x)-exp(-x)")
abline(0,1)
FixedPointFinder(F2,8,1e-5,25,1)
dev.off()
	

