# a function we will work with
F1<-function(x){
		return(c(x^2,2*x)) # note that the function returns two numbers. The first is f(x); the second is the derivative, f'(x)
}

#define a function F2(x)=sin(x)
F2<-function(x){
    return(c(sin(x),cos(x)))
}

#define F3(x)=(x-2)^3-6*x
F3<-function(x){
    return(c((x-2)^3-6*x,2*(x-2)^2-6))
}

#define F4(x)=cos(x)-x# (All functions need to return f(x) and f'(x)
F4<-function(x){
    return(c(cos(x)-x, -sin(x)-1))
}

# Define your Newton-Raphson  function
NewtonRaphson<-function(func,StartingValue,Tolerance,MaxNumberOfIterations){
  #initialize a variable, Deviation (say), to record |f(x)| so that you know how far away you are from 0.
  #(So initialze it to some arbitrary large number)
  Deviation = 10^5
  #Set up a counter, i, to record how many iterations you have performed. Set it equal to 0
  i <- 0
  MyX = rep(0,MaxNumberOfIterations)
  # Initialize the values of x and f(x)
  X = StartingValue
  Z = rep(0,2)
  Z[1] = func(X)[1]
  Z[2] = func(X)[2]

	#Set up a while loop until we hit the required target accuracy or the max. number of steps
	while ((i<MaxNumberOfIterations)&&(Deviation>Tolerance))
	{
    # Record the value of f(x) and f'(x), for the current x value.
    # I put them in a variable Z. Z[1]=f(x); Z[2]=f'(x)
	  # To be safe, check that the function and it's derivative are defined at X (either could be NaN if you are unlucky)
      
	  if ((Z[1]=="NaN")||(Z[2]=="NaN")){
	    cat("Function or derivative not defined error.\n")
	    break
	  }
	  
    #Find the next X-value using Newton-Raphson's formula. Let's call that value X
		X = X - Z[1]/Z[2]
		# calculate Deviation<- |f(x)-0|
        Z[1] = func(X)[1]
        Z[2] = func(X)[2]
        Deviation = abs(Z[1])
        Y = Z[1]
    # increase the value of your iteration counter
		i<-i+1
        
    # if you like, have the program write out how it is getting on
		cat(paste("\nIteration ",i,":   X=",X,"  Y=",Y))
        MyX[i] = X
    }
    
	# output the result
	if (Deviation<Tolerance){
		cat(paste("\nFound the root point: ",X, "after ", i, "iterations"))
		}else{
		cat(paste("\nConvergence failure. Deviation: ",Deviation, "after ", i, 	"iterations"))}
        return(MyX[1:i])
}
	
# Here's how you would call your function (once it is finished)  and get it to save a plot
curve(x^2,-10,10) # supposing our function is f(x)=x^2 and we want to plot the function between -10 and 10
X=NewtonRaphson(F1,-100,1e-3,40)# this starts it at x=8, with a desired maximum deviation of 0.001 and a maximum of 40 iterations
Y=rep(0,length(X))
for(i in seq_along(X)){
    Y[i] = F1(X[i])[1]
    segments(X[i], 0, X[i], Y[i], col="red", lwd=2)
}
abline(h=0) # this is the root we should find

NewtonRaphson(F2,-8,1e-3,40)
NewtonRaphson(F3,-8,1e-3,40)
NewtonRaphson(F4,-8,1e-3,40)
