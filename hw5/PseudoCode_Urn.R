# part I

#####################################################################
############################ subroutines ############################
#####################################################################

# Define a function Urn(m,n) that simulates an Urn that produces n non-black balls,
# assuming the black/mutation ball has weight m, and returns the number of non-black
# colors among the final n balls.  Make sure it returns the number of (non-black) colors in the final urn. Then....

setwd("/Users/chengliangdong/Desktop/PM520/hw5")

# write a function to simulate the Urn
Urn<-function(MutationBallWeight,TotalNumberOfBallsNeeded){
  # put your code here
  Urn<-mat.or.vec(1,TotalNumberOfBallsNeeded)
  # 1= black
  Urn[1] = 1
  Urn[2] = 2
  Urn[3] = 2
  # set up a counter (NumberOfBalls) to keep track of how many balls we have
  NumberOfBalls<-sum(Urn==1)+sum(Urn==2)
  while (NumberOfBalls<(TotalNumberOfBallsNeeded+1)){
      # draw a ball with weight MutationBallWeight for black ball
      p = runif(1)
      # black ball
      if(p <= MutationBallWeight/(MutationBallWeight + length(Urn[Urn!=0]) -1)){
          MyIndex = sample(2:length(Urn[Urn!=0]),1)
          MyOtherBall = Urn[MyIndex]
          # pick another ball
          # change color of ball to new color (i.e. new number)
          Urn[MyIndex] = max(unique(Urn) + 1)
          # Set color of ball equal to  NumberOfColorsUsed
          # note that we don’t increase the count of the number of balls
      }else{
          MyIndex = sample(2:length(Urn[Urn!=0]),1)
          MyBall = Urn[MyIndex]
          Urn[length(Urn[Urn!=0]) + 1] = MyBall
          # the ball is some other color
          # return the ball and add another one like it
          # increase the counter of how many balls we have in the urn:
          NumberOfBalls<-NumberOfBalls+1
      }
  }
  # output summaries of what is in the urn when we are done
  Urn
}

# write a function to count how many colors there are in the urn
NumberColors<-function(ThisUrn){
  # put your code here
  return(length(unique(ThisUrn))-1)
}

GetExpRV<-function(lambda){
    u<-runif(1,0,1)
    ExpRV <- (-1/lambda)*log(u)
    return(ExpRV)
}

PiTheta <- function(theta){
    return(1/20)
}

CasiTheta <- function(lambda, theta){
    return(lambda * exp(-lambda*theta)/(1-exp(-20*lambda)))
}


#####################################################################
############################# real thing ############################
#####################################################################

NoReps<-10000   # how many samples to generate
HowManyColorsNeeded<-1   # the ‘target’ we have to hit (what we saw in an observed dataset)
MaxWeight<-20    # the maximum weight we will consider for the black ball
NoOfBalls<-5    # the number of non-black balls we want in the urn at the end
AcceptedWeights<-rep(-9,NoReps)
Posterior<-rep(-9,NoReps)
CountNeeded = rep(-9, NoReps)

for (i in 1:NoReps){
    HowManyColorsObserved<- -9
    Index = 0
    while (HowManyColorsObserved != HowManyColorsNeeded) {
        # Sample a weight, ThisWeight, for the mutation ball, from Unif(0,MaxWeight). Then...
        ThisWeight <- runif(1,0,20)
        MyUrn<-Urn(ThisWeight,NoOfBalls)  # simulate the urn using this weight
        HowManyColorsObserved<-NumberColors(MyUrn)  # count how many colors there were at the end of this urn simulation
        Index = Index + 1
    }
    # you will leave the loop when you simulate an Urn that ends up with the target number of colors. So save that value of the weight...
    AcceptedWeights[i]<-ThisWeight
    CountNeeded[i]<-Index
}
pdf("hw5_rejection_count.pdf")
hist(CountNeeded, main = "Counts")
abline(v=mean(CountNeeded), col = "red")
legend("topleft", lty = 1, col="red", legend = paste("mean=", mean(CountNeeded), "sd=", sd(CountNeeded)))
dev.off()

pdf("hw5_rejection_density.pdf")
kernel = density(AcceptedWeights,weights = rep(1/NoReps, NoReps), kernel = "gaussian")
show(kernel)
plot(kernel, main = "Poterior distribution")
dev.off()

result = "Call:

Call:
density.default(x = AcceptedWeights, kernel = "gaussian", weights = rep(1/NoReps,     NoReps))

Data: AcceptedWeights (10000 obs.);	Bandwidth 'bw' = 0.2452

x                 y
Min.   :-0.7355   Min.   :0.0000044
1st Qu.: 4.5774   1st Qu.:0.0024242
Median : 9.8902   Median :0.0082855
Mean   : 9.8902   Mean   :0.0470072
3rd Qu.:15.2031   3rd Qu.:0.0387564
Max.   :20.5160   Max.   :0.4665314
"


lambda = c(0.0000001, 0.000001, 0.00001, 0.0001, 0.001, 0.01 ,0.1,1,10)
for(mylambda in 1:length(lambda)){
for (i in 1:NoReps){
    HowManyColorsObserved<- -9
    Index = 0
    while (HowManyColorsObserved != HowManyColorsNeeded) {
           # Sample a weight, ThisWeight, for the mutation ball, from Unif(0,MaxWeight). Then...
           ThisWeight <- GetExpRV(mylambda)
           MyUrn<-Urn(ThisWeight,NoOfBalls)  # simulate the urn using this weight
           HowManyColorsObserved<-NumberColors(MyUrn)  # count how many colors there were at the end of this urn simulation
           Index = Index + 1
    }
    # you will leave the loop when you simulate an Urn that ends up with the target number of colors. So save that value of the weight...
    Posterior[i]<-PiTheta(ThisWeight)/CasiTheta(mylambda, ThisWeight)
    AcceptedWeights[i]<-ThisWeight
    CountNeeded[i]<-Index
}

# you will now have NoReps accepted weights, so plot a histogram to see what the 
# posterior distribution f(weight|HowManyColorsNeeded) looks like

pdf(paste("hw5_importance_", mylambda, "_count.pdf", sep=""))
hist(CountNeeded, main = "Counts")
abline(v=mean(CountNeeded), col = "red")
legend("topleft", lty = 1, col="red", legend = paste("mean=", mean(CountNeeded), "sd=", sd(CountNeeded)))
dev.off()
pdf(paste("hw5_importance_", mylambda, "_density.pdf", sep=""))
kernel = density(AcceptedWeights,weights = Posterior/sum(Posterior), kernel = "gaussian")
show(kernel)
plot(kernel, main = "Poterior distribution")
dev.off()
}


result = "

Call:
density.default(x = AcceptedWeights, kernel = "gaussian", weights = Posterior/sum(Posterior))

Data: AcceptedWeights (10000 obs.);	Bandwidth 'bw' = 0.0652

x                 y
Min.   :-0.1956   Min.   :0.0009257
1st Qu.: 1.3430   1st Qu.:0.0441544
Median : 2.8816   Median :0.0913248
Mean   : 2.8816   Mean   :0.1623221
3rd Qu.: 4.4202   3rd Qu.:0.2289521
Max.   : 5.9588   Max.   :0.6856574

Call:
density.default(x = AcceptedWeights, kernel = "gaussian", weights = Posterior/sum(Posterior))

Data: AcceptedWeights (10000 obs.);	Bandwidth 'bw' = 0.04

x                 y
Min.   :-0.1200   Min.   :0.001171
1st Qu.: 0.8679   1st Qu.:0.119195
Median : 1.8558   Median :0.220379
Mean   : 1.8558   Mean   :0.252795
3rd Qu.: 2.8437   3rd Qu.:0.342971
Max.   : 3.8316   Max.   :0.731938

Call:
density.default(x = AcceptedWeights, kernel = "gaussian", weights = Posterior/sum(Posterior))

Data: AcceptedWeights (10000 obs.);	Bandwidth 'bw' = 0.0297

x                  y
Min.   :-0.08908   Min.   :0.001251
1st Qu.: 0.57487   1st Qu.:0.188719
Median : 1.23882   Median :0.331187
Mean   : 1.23882   Mean   :0.376134
3rd Qu.: 1.90277   3rd Qu.:0.522079
Max.   : 2.56672   Max.   :1.024505

Call:
density.default(x = AcceptedWeights, kernel = "gaussian", weights = Posterior/sum(Posterior))

Data: AcceptedWeights (10000 obs.);	Bandwidth 'bw' = 0.02392

x                  y
Min.   :-0.07174   Min.   : 0.00000
1st Qu.: 0.70984   1st Qu.: 0.00000
Median : 1.49141   Median : 0.08893
Mean   : 1.49141   Mean   : 0.31989
3rd Qu.: 2.27299   3rd Qu.: 0.14608
Max.   : 3.05456   Max.   :12.72993

Call:
density.default(x = AcceptedWeights, kernel = "gaussian", weights = Posterior/sum(Posterior))

Data: AcceptedWeights (10000 obs.);	Bandwidth 'bw' = 0.01981

x                  y
Min.   :-0.05942   Min.   :0.0000
1st Qu.: 0.45076   1st Qu.:0.2084
Median : 0.96094   Median :0.4154
Mean   : 0.96094   Mean   :0.4894
3rd Qu.: 1.47112   3rd Qu.:0.6156
Max.   : 1.98130   Max.   :4.5633

Call:
density.default(x = AcceptedWeights, kernel = "gaussian", weights = Posterior/sum(Posterior))

Data: AcceptedWeights (10000 obs.);	Bandwidth 'bw' = 0.01679

x                  y
Min.   :-0.05035   Min.   :0.002002
1st Qu.: 0.27728   1st Qu.:0.524914
Median : 0.60490   Median :0.779876
Mean   : 0.60490   Mean   :0.762294
3rd Qu.: 0.93253   3rd Qu.:1.024939
Max.   : 1.26015   Max.   :1.291202

Call:
density.default(x = AcceptedWeights, kernel = "gaussian", weights = Posterior/sum(Posterior))

Data: AcceptedWeights (10000 obs.);	Bandwidth 'bw' = 0.01465

x                  y
Min.   :-0.04395   Min.   :0.001873
1st Qu.: 0.27366   1st Qu.:0.578223
Median : 0.59126   Median :0.797104
Mean   : 0.59126   Mean   :0.786324
3rd Qu.: 0.90886   3rd Qu.:1.076065
Max.   : 1.22646   Max.   :1.838081

Call:
density.default(x = AcceptedWeights, kernel = "gaussian", weights = Posterior/sum(Posterior))

Data: AcceptedWeights (10000 obs.);	Bandwidth 'bw' = 0.01314

x                  y
Min.   :-0.03942   Min.   :0.002138
1st Qu.: 0.23116   1st Qu.:0.737937
Median : 0.50173   Median :0.945722
Mean   : 0.50173   Mean   :0.923015
3rd Qu.: 0.77230   3rd Qu.:1.173377
Max.   : 1.04287   Max.   :1.812830

Call:
density.default(x = AcceptedWeights, kernel = "gaussian", weights = Posterior/sum(Posterior))

Data: AcceptedWeights (10000 obs.);	Bandwidth 'bw' = 0.01163

x                  y
Min.   :-0.03487   Min.   :0.002205
1st Qu.: 0.21683   1st Qu.:0.637087
Median : 0.46854   Median :1.147460
Mean   : 0.46854   Mean   :0.992170
3rd Qu.: 0.72025   3rd Qu.:1.385814
Max.   : 0.97196   Max.   :2.565069

"


# part II

#####################################################################
############################ subroutines ############################
#####################################################################

getDNA <- function(length){
    barcode = rep(-9, length)
    barcode = sample(4,length, rep=T)
    return(barcode)
}

sameSample <- function(length, case){
    samples = rep(-9, 96)
    for(i in 1:96){
        samples[i] = paste(getDNA(length), sep="", collapse="")
    }
    if(case == 2){
        samples = gsub("3", "2", samples)
    }
    if(length(unique(samples)) == 96){
        return(0)
    }else{
        return(1)
    }
}

#####################################################################
############################# real thing ############################
#####################################################################


# case 1
# l = 1
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(1,1)
}
P=sum(Results)/length(Results)
# p = 1

# l = 2
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(2,1)
}
P=sum(Results)/length(Results)
# p = 1

# l = 3
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(3,1)
}
P=sum(Results)/length(Results)
# p = 1

# l = 4
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(4,1)
}
P=sum(Results)/length(Results)
# p = 1

# l = 5
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(5,1)
}
P=sum(Results)/length(Results)
# p = 0.9901

# l = 6
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(6,1)
}
P=sum(Results)/length(Results)
# p = 0.6749


# l = 7
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(7,1)
}
P=sum(Results)/length(Results)
# p = 0.2497


# l = 8
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(8,1)
}
P=sum(Results)/length(Results)
# p = 0.0643


# l = 9
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(9,1)
}
P=sum(Results)/length(Results)
# p = 0.0164


# l = 10
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(10,1)
}
P=sum(Results)/length(Results)
# p = 0.0038




# l = 11
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(11,1)
}
P=sum(Results)/length(Results)
show(P)
# p = 6e-04



# l = 12
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(12,1)
}
P=sum(Results)/length(Results)
show(P)
# p = 5e-04


# l = 13
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(13,1)
}
P=sum(Results)/length(Results)
show(P)
# p = 1e-04










# case 1
# l = 1
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(1,2)
}
P=sum(Results)/length(Results)
show(P)
# p = 1

# l = 2
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(2,2)
}
P=sum(Results)/length(Results)
show(P)
# p = 1

# l = 3
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(3,2)
}
P=sum(Results)/length(Results)
show(P)
# p = 1

# l = 4
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(4,2)
}
P=sum(Results)/length(Results)
show(P)
# p = 1

# l = 5
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(5,2)
}
P=sum(Results)/length(Results)
show(P)
# p = 1

# l = 6
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(6,2)
}
P=sum(Results)/length(Results)
show(P)
# p = 1


# l = 7
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(7,2)
}
P=sum(Results)/length(Results)
show(P)
# p = 0.9891


# l = 8
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(8,2)
}
P=sum(Results)/length(Results)
show(P)
# p = 0.8283


# l = 9
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(9,2)
}
P=sum(Results)/length(Results)
show(P)
# p = 0.4714


# l = 10
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(10,2)
}
P=sum(Results)/length(Results)
show(P)
# p = 0.2183



# l = 11
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(11,2)
}
P=sum(Results)/length(Results)
show(P)
# p = 0.0893



# l = 12
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(12,2)
}
P=sum(Results)/length(Results)
show(P)
# p = 0.0349


# l = 13
NumberOfSimulation = 10000
Results = rep(-9, NumberOfSimulation)
for(i in 1:NumberOfSimulation){
    Results[i] = sameSample(13,2)
}
P=sum(Results)/length(Results)
show(P)
# p = 0.0148






