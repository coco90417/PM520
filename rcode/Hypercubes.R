set.seed(329878)  # set the seed for the random number generator to any number you like
NumberOfTrials<-50000
TotalCountNearSurface<-numeric()
MaxNumberOfDimensions<-10  # we will go up to this many dimensions
HowManyCloseToSurface<-0 # A variable to record how often the point is close to the surface
for (j in 1:MaxNumberOfDimensions) {
  HowManyCloseToSurface<-0   # reset your counter
  for (i in 1:NumberOfTrials) {
    # pick a point at random in the cube
    MyPoint<-runif(j,0,1)
    #cat(MyPoint)
    #cat("     Max/min: ",max(MyPoint),min(MyPoint<0.1),(max(MyPoint)>0.9)||(min(MyPoint<0.1)))
    #cat("\n")
    
    # check whether it is within 0.1 of the surface (how do you check this?)
   if ( (max(MyPoint)>0.9)||(min(MyPoint)<0.1)){
      # if it is, increase the value of HowManyCloseToSurface by 1
      HowManyCloseToSurface <- HowManyCloseToSurface+1
   }
  }  
  
  # record the value of HowManyCloseToSurface/NumberOfTrials, this will be your estimate of 
  TotalCountNearSurface[j]<-HowManyCloseToSurface
  # the proportion of the volume of the cube that is within 0.1 of the surface
  cat("\nEstimatedProb.:", TotalCountNearSurface[j]/NumberOfTrials)
}


# plot your estimates of the proportion of the volume that is within 01 of the surface (y-axis)
# against the number of dimensions the hypercube has (x-axis)
plot(TotalCountNearSurface/NumberOfTrials)

# what do we think the answer should be
ExpectedAnswer<-numeric()
for (i in 1:MaxNumberOfDimensions){
  ExpectedAnswer[i]<-1-(0.8^i)
}
# super-impose this on our plot
lines(ExpectedAnswer)

# what do you notice about this proportion as N increases?
# what value do you think it takes for very large N?
