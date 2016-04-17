NTrialsPerExpt<-5000   # how long should the sequence of 0s and 1s be
BinarySequence<-rep(0,NTrialsPerExpt)  # a variable to hold the sequence
set.seed(234)  # this sets the 'seed' of the random number generator, so that we get the same sequence of random numbers each time we run the code

SlowGenerator<-function(Seq){
  for (i in 1:length(Seq)){
    p<-runif(1,0,1)   # a random number between 0 and 1
    if (p<0.5){
      Seq[i]<-0
    }else{       # Note that in R the closing } of the if loop must go on the same line as the 'else' statement
      Seq[i]<-1
    }
  } 
  return (Seq)
}

FastGenerator<-function(Seq){
  p<-runif(length(Seq),0,1)
  Seq<-as.integer((p<0.5))
  return (Seq)
}

SlowHowManyRuns<-function(Seq){
  RunCount<-1
  for (i in 2:length(Seq)){
    if (Seq[i]!=Seq[i-1]){
      RunCount<-RunCount+1
    }
  }
  return (RunCount)
}

FastHowManyRuns<-function(Seq){
  Count<-diff(Seq,lag=1)
  Count<-abs(Count)
  return (1+sum(Count))
}

##########################################
##########################################
# Here's where the main code body starts #
##########################################
##########################################


MySeq<-rep(0,NTrialsPerExpt)
# If you run this for NTrialsPerExpt<-100000 you will see that the Fast way is 100 times ftaster than the SLow way. R hates loops!
# Start the clock
ptm <- proc.time()
MySeq1<-SlowGenerator(MySeq)
# Stop the clock
proc.time() - ptm
# Start the clock
ptm <- proc.time()
MySeq2<-FastGenerator(MySeq)
# Stop the clock
proc.time() - ptm

# Start the clock
ptm <- proc.time()
SlowHowManyRuns(MySeq2)
# Stop the clock
proc.time() - ptm
# Start the clock
ptm <- proc.time()
FastHowManyRuns(MySeq2)
# Stop the clock
proc.time() - ptm


# Now we do the whole experiment 2500 times to find the distribution of the number of runs
NExpts<-2500
MultipleExpts<-rep(0,NExpts)
for (i in 1:NExpts){
  MySeq1<-FastGenerator(MySeq)
  MultipleExpts[i]<-FastHowManyRuns(MySeq1)
}
MyHist<-hist(MultipleExpts,main="Empirical Distribution of Number of Runs",xlab="Number of runs")
multiplier <- MyHist$counts / MyHist$density
NormalMean<-1+(NTrialsPerExpt-1)/2   # the expected number of 'runs'
# It should look like a Binomial (which itself looks like a Normal for large values of NTrialsPerExpt)
NormalSD<-sqrt((NTrialsPerExpt-1)*0.5*0.5)
myx <- seq(min(MultipleExpts), max(MultipleExpts), length.out= 100)
normal <- dnorm(x = myx, mean = NormalMean, sd = NormalSD)
#plot(MyHist$density)
lines(myx, normal * multiplier[1], col = "blue", lwd = 2)

#Formal test of normality
shapiro.test(MultipleExpts)

# Let's compare it to a set of randomly generated binomial random variables
MyBinomials<-rbinom(NExpts,NTrialsPerExpt,0.5)    # to find this command I Googled "Binomial Random Variables R"
# compare the plots
par(mfrow=c(1,2))
MyHist<-hist(MultipleExpts,main="Empirical Distribution of Number of Runs",xlab="Number of runs")
MyHist2<-hist(MyBinomials)
par(mfrow=c(1,1))

# let's look at qq plots -  if the distributions are the same, we should see straight lines
par(mfrow=c(1,2))
qqplot(MultipleExpts,MyBinomials,pch='.')
qqnorm(MultipleExpts,pch='.')
par(mfrow=c(1,1))


# What is the distribution of the length of the run starting at the first toss?
NExpts<-10000
MultipleExpts<-rep(0,NExpts)
for (i in 1:NExpts){
    MySeq1<-FastGenerator(MySeq)
    MyLength=1
    if(MySeq1[1] == 1){
        for (j in 2:length(MySeq1)){
            if(MySeq1[j] == 1){
                MyLength=MyLength+1
            }else{
                break
            }
        }
    }else{
        for (j in 2:length(MySeq1)){
            if(MySeq1[j] == 0){
                MyLength=MyLength+1
            }else{
                break
            }
        }
    }
    MultipleExpts[i]<-MyLength
}
MyHist<-hist(MultipleExpts,main="Empirical Distribution of Number of Runs of first toss",xlab="Number of runs")


# What is the expected number and distribution of the number of runs in total?
NExpts<-2500
MultipleExpts<-rep(0,NExpts)
for (i in 1:NExpts){
    MySeq1<-FastGenerator(MySeq)
    MyCount=0
    MyLast=MySeq1[1]
    for (j in 2:length(MySeq1)){
        if(MySeq1[j] == MyLast){
            
        }else{
            MyCount=MyCount+1
        }
        MyLast=MySeq1[j]
    }
    MultipleExpts[i]<-MyCount
}
MyHist<-hist(MultipleExpts,main="Empirical distribution of the number of runs in total",xlab="Number of runs")


# What is the expected value and distribution of the length of the longest run?
NExpts<-2500
MultipleExpts<-rep(0,NExpts)
for (i in 1:NExpts){
    MySeq1<-FastGenerator(MySeq)
    MyCount=0
    MyLongest=0
    MyTemplength=1
    MyLast=MySeq1[1]
    for (j in 2:length(MySeq1)){
        if(MySeq1[j] == MyLast){
            MyTemplength=MyTemplength+1
        }else{
            MyLongest=max(MyTemplength,MyLongest)
            MyCount=MyCount+1
            MyTemplength=1
        }
        MyLast=MySeq1[j]
    }
    MultipleExpts[i]<-MyLongest
}
MyHist<-hist(MultipleExpts,main="Empirical distribution of length of the longest run",xlab="Number of runs")

# What is the distribution of the number of balls in bucket i (for any i)?
NExpts<-2500
NumberOfBucket=6
MyBucket = 1
NumberOfBall=1000
MultipleExpts<-rep(0,NExpts)
for(i in 1:NExpts){
    MyNumberOfBall = sample( NumberOfBucket, NumberOfBall,replace = T)
    MultipleExpts[i] = length(MyNumberOfBall[MyNumberOfBall==MyBucket])
}
MyHist<-hist(MultipleExpts,main="Empirical distribution of number of balls in bucket i (for any i)",xlab="Number of runs")



# What is the distribution of the number of empty buckets?
NExpts<-2500
NumberOfBucket=6
NumberOfBall=6
MultipleExpts<-rep(0,NExpts)
for(i in 1:NExpts){
    MyNumberOfBall = sample( NumberOfBucket, NumberOfBall,replace = T)
    MultipleExpts[i] = 0
    for(j in 1:NumberOfBucket){
        if (length(MyNumberOfBall[MyNumberOfBall==j]) == 0){
            MultipleExpts[i] = MultipleExpts[i] + 1
        }
    }
}
MyHist<-hist(MultipleExpts,main="Empirical distribution of number of balls in each bucket at the end",xlab="Number of runs",breaks=5)



# What is the distribution of the number of balls in the bucket with the most balls?
NExpts<-2500
NumberOfBucket=6
NumberOfBall=10000
MultipleExpts<-rep(0,NExpts)
for(i in 1:NExpts){
    MyNumberOfBall = sample( NumberOfBucket, NumberOfBall,replace = T)
    MyMaxNumber = 0
    for(j in 1:NumberOfBucket){
        MyMaxNumber = max(length(MyNumberOfBall[MyNumberOfBall==j]), MyMaxNumber)
    }
    MultipleExpts[i] =MyMaxNumber
}
MyHist<-hist(MultipleExpts,main="Empirical distribution of the number of balls in the bucket with the most balls",xlab="Number of runs")







