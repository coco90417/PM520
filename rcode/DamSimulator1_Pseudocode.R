# Program to simulate dam overflow probability
set.seed(-123)

MyFinal = rep(0,50)
MyX = rep(0,50)
for(k in 1:50){
for(j in 1:500){
DamMaxVolume<- k*50 + 100
MyX[k] = DamMaxVolume
WaterReleaseRate<-5
MeanRainfall<-5
RainfallSD<-10
CurrentDamVolume<-DamMaxVolume/2
HowManyDaysToSimulate<-365
MyWater = rep(0,HowManyDaysToSimulate)
# iterate throgh the days
for (i in 1:HowManyDaysToSimulate){
  # How much rain falls?
  RainFallToday = rnorm(1,MeanRainfall,RainfallSD)
  # How much water is relased?
  WaterRelease = WaterReleaseRate
  # How much water in the dam at the end of the day
  CurrentDamVolume = CurrentDamVolume + RainFallToday - WaterRelease
  # If there is too much water, indicate that there has been an overlow
  MyWater[i] =CurrentDamVolume
  if(CurrentDamVolume <= 0){
      CurrentDamVolume = 0
  }
}
Probability[j] = length(MyWater[MyWater>=DamMaxVolume])/length(MyWater)
}
MyFinal[k] = mean(Probability)
}

plot(MyX, MyFinal, main="Emperical distribution of probability of overflow at different dam size", xlab="dam size", ylab="probability", ylim=c(0,1), lty = 1)