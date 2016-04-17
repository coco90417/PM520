set.seed(99999)

#repeat the followoing until you have 1000 conditioned exponential rvs.
NTrials = 1000
MyResult = rep(0,NTrials)
for(i in 1:NTrials) {
    u<-runif(1,0,1)
    y<-1
    lambda<-2
    ConditionedExpRV<- (-1/lambda)*log(u)
    while (ConditionedExpRV < y){
      u<-runif(1,0,1)
      ConditionedExpRV<- (-1/lambda)*log(u)
    }
    MyResult[i] = ConditionedExpRV
}
#Store the value of ConditionedExpRV

hist(MyResult, main="Empirical Distribution of Conditional Exponential RV",xlab="Number of runs")
curve(lambda*exp(-lambda*x))



#### bus schedule
NumberOfBus = 100
NumberOfPerson = 50
NumberOfTrial = 2500
lambda<-0.5
MyResult = rep(0,NumberOfTrial)
for(k in 1:NumberOfTrial){
    MyBus = rep(0,NumberOfBus)
    MyPerson = sample(1:24,NumberOfPerson,replace=T)
    MyWaitingTime = rep(0,NumberOfPerson)
    u<-runif(1,0,1)
    MyBus[1] = (-1/lambda)*log(u)
    for(i in 2:NumberOfBus){
       u<-runif(1,0,1)
       MyBus[i] = (-1/lambda)*log(u) + MyBus[i-1]
    }
    MyDayBus = MyBus[MyBus<=24]
    for(j in 1:NumberOfPerson){
        if(length(MyDayBus[MyDayBus>MyPerson[j]])==0){
            MyValue = 24
        }else{
            MyValue = MyDayBus[MyDayBus>MyPerson[j]][1]
        }
        MyWaitingTime[j] = MyValue - MyPerson[j]
    }
    MyResult[k] = mean(MyWaitingTime)
}
MyHist=hist(MyResult, main="Empirical Distribution of Waiting Time",xlab="Number of runs")
