# Here's a function to practise minimizing. The mininum is at (0,0)
# TSP

X = c(9.3027950, 6.6956783, 6.9927559, 4.8220995, 9.3115035, 0.5580354, 5.8925933, 9.2219042, 8.3391724, 9.7156158)
Y = c(9.7416671, 5.8780066, 1.4676554, 7.0652635 1.2684691, 5.3369829, 3.3594382, 5.8392295, 1.1435415, 2.3761776)

XYSquared<-function(x,y)
{
  return (-1*(x^2+y^2))
}

Distance <- function(x, y, index){
    distance = 0
    for(i in seq_along(index)){
        if (i == length(index)){
            distance = distance + sqrt((x[i] - x[1])^2 + (y[i]-y[1])^2)
                return(distance)
        }
        distance = distance + sqrt((x[i] - x[i+1])^2 + (y[i]-y[i+1])^2)
    }
}


SimAnneal2D<-function(Fn,StartPoint,InitialTemp,FinalTemp,TempDecreaseRate){
	StepSize<-0.5   # how much are we going to move by for each step?
	X<-StartPoint[1]
	Y<-StartPoint[2]
	# record current X and Y values (Old X, Old Y), & the value the function takes at this point (FnVal, say)
    OldX = X
    OldY = Y
    FnVal = Fn(OldX,OldY)
    # Set the temperature (Temp) to = InitialTemp,
    Temp = InitialTemp
	plot(X,Y,xlim=c(-2*X,2*X),ylim=c(-2*Y,2*Y)) # set up a plot
	points(0,0,pch=19,col="red")  # plot the Minimum - this is where you are hoping to end up
	while (Temp>FinalTemp){  # we keep going until things cool down
		#propose new point
		NewX<-X+rnorm(1,mean=0,sd=StepSize)  # you don’t have to do it this way, but it seems reasonable to me
		NewY<-Y+rnorm(1,mean=0,sd=StepSize)	
		#decide whether to move
		NewVal<- Fn(NewX,NewY)  # the value the function takes at the new point
		h<-min(1,exp(1*(NewVal-FnVal)/Temp))
		p<-runif(1)  # the random number that is going to help us decide whether to move
		if (p<h){ #move
			# update your records of where we are:...OldX<-X     OldY<-Y
            OldX = X
            OldY = Y
			# update your records again (I’m guessing we don’t need both of these) X<-NewX   Y<-NewY
            X = NewX
            Y = NewY
			# update the record of the function value:     FnVal<-NewVal
            FnVal = NewVal
			# add an arrow to the plot showing where we moved:
            arrows(OldX,OldY,X,Y,length=0.05)
        }
        #reduce temperature
        Temp<-Temp*(1-TempDecreaseRate)
        # pause for a bit - otherwise the plots flash by too quickly to see properly
        Start.Time<-Sys.time()
        while (Sys.time()<Start.Time+0.02){
            Sys.sleep(0.01)
        }   # there is probably a better way of doing this
	}
	points(X,Y,pch=19,col="blue")   # a big blue point to show where we ended up
	return (c(X,Y,Fn(X,Y)))  # return the coordinates of our final resting place, and the function value
}