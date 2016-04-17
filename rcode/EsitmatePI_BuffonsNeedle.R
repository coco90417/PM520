# Pseudocode for estimating pi using Buffon's needle
set.seed(2053)   # remember to set the random number seed so that you can reproduce your output if you end up needing to hunt bugs

# set the distance between the lines
LineDistance<-1
# Set the needle length
NeedleLength<-1

NumberOfTrials<-10000    # how many needles to throw
NumberOfNeedlesThatCrossALine<-0   # this will count how often the needle crosses a line


# do the expteriment - here's one way to do it
# Start the clock
ptm <- proc.time()
for (i in 1:NumberOfTrials){
  # generate a random needle position - first we choose where the end of the needle falls
  # for convenience, we will say that it falls somewhere within a 10x10 table
  XCoordinate<-runif(1,0,10) 
  YCoordinate<-runif(1,0,10)
  # now generate the angle at which the needle falls - R measure angles in radian, so for the code we need to use pi (a pre-defined variable in R)!
  Angle<-runif(1,0,2*pi)
  
  # check whether it falls within the circle (you have to think about how to do this) and increase value of NumberOfNeedlesThatCrossALine if so
  
}
ProportionOfPointsThatCrossLine<-NumberOfNeedlesThatCrossALine/NumberOfTrials
# and then do the math to produce you estimate of pi - yours to write
# Stop the clock
proc.time() - ptm


# Here's another way -is it quicker than the other version?
# Start the clock
ptm <- proc.time()
AllXCoordinate<-runif(NumberOfTrials,-1,1) 
AllYCoordinate<-runif(NumberOfTrials,-1,1)
AllAngles<-runif(NumberOfTrials,0,2*pi)

# Now check what proportion of these points fall within the circle - yours to write
# and then do the math to produce you estimate of pi - yours to write
# Stop the clock
proc.time() - ptm


# if you are feeling fancy, write some code to plot your needles and the table, or your estimate of pi as you perofrm the iterations

# Now you will need to write some code to repeat the above for different needle lengths
# this will be easiest if you turn the above into functions


