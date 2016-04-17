TimeToSimulate<-50   # the number of time periods we simulate
NumberOfCars<- 35   # How many cars are on the road
NumberOfSpacesInRoad <- 100   # The road is a loop with this many spaces in it. Each space can be occupied by at most one car
vMax <- 5  # The maximum velocity of cars
PDecelerate <- 0.3 # the probability that a car randomly decelerates
CarPositions<-rep(-9,NumberOfCars)   # This will store the locations of the cars - I put -9s in there for 9, to represent missing data
CarVs<-rep(0,NumberOfCars)   # This will store the velocities of the cars
# since updates are simultaneous, we will make a second copy of the position vector to store the new values
NewCarPositions<-rep(-9,NumberOfCars)  
AverageVelocity<-rep(-9,TimeToSimulate) # we will record average velocity at each time step as well

# set up the initial positions of the cars. We might as well keep them in order
CarPositions<-sample(1:NumberOfSpacesInRoad,NumberOfCars)
CarPositions<-sort(CarPositions)
cat(CarPositions)

# Define a huge matrix to store the position of each car at each time point. This won't work if we have too many time points or cars
PositionRecorder <- mat.or.vec(TimeToSimulate,NumberOfCars)

# for debug, set up a vector to record the distances to the next car
DistanceRecorder<-rep(-9,NumberOfCars)


# loop through the time periods
for (t in 1:TimeToSimulate){
  # loop through the cars
  for (CarIndex in 1:NumberOfCars)
  {
    # go through the 4 update steps for this car
    # step 1 - if my velocity<vMax increase my velocity by 1
    if (CarVs[CarIndex]<vMax){ 
      # ...
     } 
    #step 2 - how close is the car in front? (Be careful about this if this car is further along the road.) If v>=d, then set v=d-1    
    #step 3 - decelerate with probability PDecelerate
    #step 4 - update my position (put it in NewCarPosition). If my new position is > NumberOfSpacesInRoad, wrap this car around to the start of the road
  }
  
  # Now copy the new position vector into the current position vector
  CarPositions<-NewCarPositions
  #Store the positions
  PositionRecorder[t,]<-CarPositions
  
  # record average velocity
  AverageVelocity[t]<-mean(CarVs)
  # let's output some information to see if everything looks ok
  cat(CarVs)     # this is just a debug: if you see any negative car velocities you know you have a problem
  cat(CarPositions) # again, just for debug. The numbers shoud all be between 1 and NumberOfSpacesInRoad
  cat("\n")
  cat(DistanceRecorder)
  cat("\n\n")
}

# plot the positions of the cars through time
Times<- -1*(1:TimeToSimulate)
plot(PositionRecorder[,1],Times,pch='.',xlim=c(0,100))
  for (b in 2:NumberOfCars){
    points(PositionRecorder[,b],Times,pch='.',cex=3)    
}

plot(AverageVelocity,type='l')

