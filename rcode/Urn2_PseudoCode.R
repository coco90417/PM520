# Build it on your existing code (define variable, etc.)
# change the function for drawing a ball...
# notate colors with numbers, so black==0, other colors== 1,2,3...

GetUrn <- function(){
TargetNumberOfBalls = 50
Urn<-mat.or.vec(1,TargetNumberOfBalls)
# 1= black
Urn[1] = 1
Urn[2] = 2
# set up a counter (NumberOfBalls) to keep track of how many balls we have
NumberOfBalls<-sum(Urn==1)+sum(Urn==2)
HowManyBallsWeNeed = 50
while (NumberOfBalls<(HowManyBallsWeNeed+1)){
  # draw a ball
  MyBall = Urn[sample(1:length(Urn[Urn!=0]),1)]
  if (MyBall == 1){
    MyIndex = sample(2:length(Urn[Urn!=0]),1)
    MyOtherBall = Urn[MyIndex]
    # pick another ball
    # change color of ball to new color (i.e. new number)
    Urn[MyIndex] = max(unique(Urn) + 1)
    # Set color of ball equal to  NumberOfColorsUsed
    # note that we don’t increase the count of the number of balls
  }else{
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


# 1. number of non-black colors

MyColor = rep(0, 5000)
for (i in 1:5000){
    MyUrn  = GetUrn()
    MyColor[i] = length(unique(MyUrn)) - 1
}
hist(MyColor)


# 2. commonest color
MyColor = rep(0, 100)
for (i in 1:100){
    MyUrn  = GetUrn()
    MyMaxCount = 0
    for (j in seq_along(unique(MyUrn))){
        MyCount = length(MyUrn[MyUrn==unique(MyUrn)[j]])
        if(MyCount > MyMaxCount){
            MyColor[i] = MyCount
            MyMaxCount = MyColor[i]
        }
    }
}
hist(MyColor)


# 3.
