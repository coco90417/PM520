# set the random number seed
set.seed(16)

# define your variables
# How many balls do we start with
InitialNumberOfBalls<-2
# How many balls do we need eventually
TargetNumberOfBalls<-10
   
# set up the initial state of the urn
Urn<-mat.or.vec(1,TargetNumberOfBalls) 
# we will start with two balls of different colors: "red" and "blue"
# The first element of the urn vector should be set to "red", the second element should be set to "blue" (say)
Urn[1] = "red"
Urn[2] = "blue"

# set up a counter (NumberOfBalls) to keep track of how many balls we have
NumberOfBalls<-sum(Urn=="red")+sum(Urn=="blue")

# set-up a loop that pulls a ball from the urn and takes the appropriate action
while (NumberOfBalls<TargetNumberOfBalls){
	# draw a ball (WhichBall)
    MyBall = Urn[sample(1:length(Urn[Urn!=0]),1)]
	# return the ball and add another one like it
    Urn[length(Urn[Urn!=0]) + 1] = MyBall
	# increase the counter of how many balls we have in the urn
    NumberOfBalls<-sum(Urn=="red")+sum(Urn=="blue")
}

# output summaries of what is in the urn when we are done (ignoring the extra ball )
NumberOfBalls<-sum(Urn=="red")+sum(Urn=="blue")
Urn

###################################

# function
DrawBallsFromUrn <- function(InitialNumberOfBalls=1, TargetNumberOfBalls=10, ReturnBall=1){
    # set up the initial state of the urn
    Urn<-mat.or.vec(1,TargetNumberOfBalls)
    # we will start with two balls of different colors: "red" and "blue"
    # The first element of the urn vector should be set to "red", the second element should be set to "blue" (say)

    if(InitialNumberOfBalls == 1){
            Urn[1] = "red"
            Urn[2] = "blue"
    }else{
        for(i in 1:(InitialNumberOfBalls-1)){
            Urn[i] = "red"
        }
        Urn[InitialNumberOfBalls] = "blue"
    }
    # set up a counter (NumberOfBalls) to keep track of how many balls we have
    NumberOfBalls<-sum(Urn=="red")+sum(Urn=="blue")
    
    # set-up a loop that pulls a ball from the urn and takes the appropriate action
    while (NumberOfBalls<TargetNumberOfBalls){
        # draw a ball (WhichBall)
        MyBall = Urn[sample(1:length(Urn[Urn!=0]),1)]
        # return the ball and add another one like it
        MyCurrentIndex = length(Urn[Urn!=0])
        for(i in 1:ReturnBall){
            Urn[MyCurrentIndex + i] = MyBall
        }
        # increase the counter of how many balls we have in the urn
        NumberOfBalls<-sum(Urn=="red")+sum(Urn=="blue")
    }
    
    # output summaries of what is in the urn when we are done (ignoring the extra ball )
    NumberOfBalls<-sum(Urn=="red")+sum(Urn=="blue")
    Urn
}

###################################

# task 1

MyRedNumber = rep(0,1000)
for (i in 1:1000){
     MyUrn = DrawBallsFromUrn(InitialNumberOfBalls=2, TargetNumberOfBalls=50)
     MyRedNumber[i] = length(MyUrn[MyUrn=="red"])
}


# task 2
MyMean = rep(0,49)
for (j in 1:49){
    MyRedNumber = rep(0,1000)
    for (i in 1:1000){
    MyUrn = DrawBallsFromUrn(InitialNumberOfBalls=j, TargetNumberOfBalls=50)
    MyRedNumber[i] = length(MyUrn[MyUrn=="red"])
    }
    MyMean[j] = mean(MyRedNumber)
}

plot(1:49,MyMean)


# task 3
MyMean = rep(0,12)
par(mfrow=c(3,4))
for (j in 1:12){
    MyRedNumber = rep(0,1000)
    for (i in 1:1000){
        MyUrn = DrawBallsFromUrn(InitialNumberOfBalls=1, TargetNumberOfBalls=50, ReturnBall = j)
        MyRedNumber[i] = length(MyUrn[MyUrn=="red"])
    }
    hist(MyRedNumber)
    MyMean[j] = mean(MyRedNumber)
}

plot(1:10,MyMean)





