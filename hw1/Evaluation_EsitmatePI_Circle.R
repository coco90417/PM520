# Pseudocode for estimating pi using the circle method
set.seed(2053)   # remember to set the random number seed so that you can reproduce your output if you end up needing to hunt bugs

# Let's assume the square has length 2, and is centered at (0,0)
# Set circle radius


CircleRadius<-1
NumberOfTrials<-10000    # how many random points to generate
NumberOfPointsInsideCircle<-0   # this will count how often the point lands inside the circle

# do the expteriment - here's one way to do it
# Start the clock
ptm <- proc.time()
for (i in 1:NumberOfTrials){
  # generate a random point in the square
  XCoordinate<-runif(1,0-CircleRadius,CircleRadius)
  YCoordinate<-runif(1,-CircleRadius,CircleRadius)
  # check whether it falls within the circle (i.e. the distance to the origina is less than Circle Radius). If so, set NumberOfPointsInsideCircle<-NumberOfPointsInsideCircle+1
  if (sqrt(XCoordinate^2 + YCoordinate^2) <= CircleRadius)
  {
      NumberOfPointsInsideCircle<-NumberOfPointsInsideCircle+1
  }
}
ProportionOfPointsInCircle<-NumberOfPointsInsideCircle/NumberOfTrials
# and then do the math to produce you estimate of pi - yours to write
MyPi <-4*ProportionOfPointsInCircle
# Stop the clock
proc.time() - ptm






# load library
library(gplots)
palette(rev(rich.colors(32)))

MyTrails = c(10, 100, 1000, 10000)
MyPi = rbind(rep(0,100), rep(0,100), rep(0,100), rep(0,100))
for (j in 1:100){
for (k in 1:4){
# Here's another way -is it quicker than the other version?
# Start the clock
CircleRadius<-j
NumberOfTrials<- MyTrails[k]    # how many random points to generate
NumberOfPointsInsideCircle<-0   # this will count how often the point lands inside the circle
ptm <- proc.time()
AllXCoordinate<-runif(NumberOfTrials,-CircleRadius,CircleRadius)
AllYCoordinate<-runif(NumberOfTrials,-CircleRadius,CircleRadius)
# Now check what proportion of these points fall within the circle - yours to write
# and then do the math to produce you estimate of pi - yours to write
# create a vector of TRUE and FALSE variables of whether each (X,Y) coordinate falls within the circle
MyTruthTable<-sqrt(AllXCoordinate^2 + AllYCoordinate^2) <= CircleRadius
# calculate the proportion
ProportionOfPointsInCircle<-length(MyTruthTable[MyTruthTable==TRUE])/NumberOfTrials
MyPi[k,j]<-4*ProportionOfPointsInCircle
# Stop the clock
proc.time() - ptm
}
}


MyTrails = c(10, 100, 1000, 10000)
# Here's another way -is it quicker than the other version?
# Start the clock
CircleRadius<-1
NumberOfTrials<- MyTrails[k]    # how many random points to generate
NumberOfPointsInsideCircle<-0   # this will count how often the point lands inside the circle
ptm <- proc.time()
AllXCoordinate<-runif(NumberOfTrials,-CircleRadius,CircleRadius)
AllYCoordinate<-runif(NumberOfTrials,-CircleRadius,CircleRadius)
# Now check what proportion of these points fall within the circle - yours to write
# and then do the math to produce you estimate of pi - yours to write
# create a vector of TRUE and FALSE variables of whether each (X,Y) coordinate falls within the circle
MyTruthTable<-sqrt(AllXCoordinate^2 + AllYCoordinate^2) <= CircleRadius
# calculate the proportion
ProportionOfPointsInCircle<-length(MyTruthTable[MyTruthTable==TRUE])/NumberOfTrials
MySecondPi[k,j]<-4*ProportionOfPointsInCircle
# Stop the clock
proc.time() - ptm


CircleRadius<-1
NumberOfTrials<-10000   # how many random points to generate
NumberOfPointsInsideCircle<-0   # this will count how often the point lands inside the circle
ptm <- proc.time()
AllXCoordinate<-runif(NumberOfTrials,-CircleRadius,CircleRadius)
AllYCoordinate<-runif(NumberOfTrials,-CircleRadius,CircleRadius)
# Now check what proportion of these points fall within the circle - yours to write
# and then do the math to produce you estimate of pi - yours to write
# create a vector of TRUE and FALSE variables of whether each (X,Y) coordinate falls within the circle
MyTruthTable<-sqrt(AllXCoordinate^2 + AllYCoordinate^2) <= CircleRadius
# calculate the proportion
ProportionOfPointsInCircle<-length(MyTruthTable[MyTruthTable==TRUE])/NumberOfTrials
MyPi[j]<-4*ProportionOfPointsInCircle
# Stop the clock
proc.time() - ptm



plot(1:3000, MyPi, main="Value of pi at different seed value", xlim=c(0,3000), ylim=c(1.5,4), xlab="seed", ylab="My estimated pi", type = "l", lty=1, col=6)
y = c(0,1,2,30,3000)
x = c(pi, pi, pi, pi, pi)
lines(y, x, col=1, lwd=1)
legend("bottomright", c("actual pi"), lty = 1, col=1, cex=0.7)



par(mfrow=c(2,2))
hist(MySecondPi[1,], main = "pi estimated with 10 trails", xlab = "My pi")
y = c(0,1,2, 100,10000)
x = c(pi, pi, pi, pi, pi)
lines(x, y, col="red")
legend("topleft", "actual pi", lty = 1, col="red", cex=0.7)

hist(MySecondPi[2,], main = "pi estimated with 100 trails",  xlab = "My pi")
y = c(0,1,2, 2,2000)
x = c(pi, pi, pi, pi, pi)
lines(x, y, col="red")
legend("topright", "actual pi", lty = 1, col="red", cex=0.7)

hist(MySecondPi[3,], main = "pi estimated with 1000 trails",  xlab = "My pi")
y = c(0,1,2, 2,2000)
x = c(pi, pi, pi, pi, pi)
lines(x, y, col="red")
legend("topright", "actual pi", lty = 1, col="red", cex=0.7)

hist(MySecondPi[4,], main = "pi estimated with 10000 trails",  xlab = "My pi")
y = c(0,1,2, 2,20000)
x = c(pi, pi, pi, pi, pi)
lines(x, y, col="red")
legend("topright", "actual pi", lty = 1, col="red", cex=0.7)



plot(1:100, MyPi[1,], main="Value of pi at different radius", xlim=c(0,101), ylim=c(1.5,4), xlab="radius", ylab="My estimated pi", type = "l", lty=1, col=6)
lines(1:100, MyPi[2,], col=11)
lines(1:100, MyPi[3,], col=16)
lines(1:100, MyPi[4,], col=21)
y = c(0,1,2,30,100)
x = c(pi, pi, pi, pi, pi)
lines(y, x, col=1, lwd=1)

legend("bottomright", c("actual pi", "10 trails", "100 trails", "1000 trails", "100000 trails"), lty = 1, col=c(1, 6, 11, 16, 21), cex=0.7)





# if you are feeling fancy, write some code to plot your points and the circle and square, or your estimate of pi as you perofrm the iterations

# Now you will need to write some code to repeat the above for different sizes of circle (i.e. different values of CircleRadius)
# this will be easiest if you turn the above into functions






