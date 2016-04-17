# Define a function Urn(m,n) that simulates an Urn that produces n non-black balls,                           # assuming the black/mutation ball has weight m, and returns the number of non-black 
# colors among the final n balls.  Make sure it returns the number of (non-black) colors in the final urn. Then....

NoReps<-10000   # how many samples to generate
HowManyColorsNeeded<-1   # the ‘target’ we have to hit (what we saw in an observed dataset)
MaxWeight<-10    # the maximum weight we will consider for the black ball 
NoOfBalls<-5    # the number of non-black balls we want in the urn at the end
AcceptedWeights<-rep(-9,NoReps)

for (i in 1:NoReps){
    HowManyColorsObserved<- -9
    while (HowManyColorsObserved != HowManyColorsNeeded) {
           # Sample a weight, ThisWeight, for the mutation ball, from Unif(0,MaxWeight). Then...
           HowManyColorsObserved<-Urn(ThisWeight,NoOfBalls)  # simulate the urn using this weight
    }
   AcceptedWeights[i]<-ThisWeight  
}

# you will now have NoReps accepted weights, so plot a histogram to see what the 
# posterior distribution f(weight|HowManyColorsNeeded) looks like