##################################################
#################### functions ###################
##################################################
# a function we will work with cos(x)-x, return f(x) and f'(x)
F1<-function(x){
    return(c(cos(x)-x, -sin(x)-1))
}

# a function we will work with log(x)-exp(-x), return f(x) and f'(x)
F2<-function(x){
    return(c(log(x)-exp(-x), 1/x+exp(-x)))
}

# Implement Secant Method
Secant <-function(func,X_zero, X_one,Tolerance,MaxNumberOfIterations){
    #initialize a variable, Deviation (say), to record |f(x)| so that you know how far away you are from 0.
    #(So initialze it to some arbitrary large number)
    Deviation = 10^5
    #Set up a counter, i, to record how many iterations you have performed. Set it equal to 0
    i <- 0
    MyX = rep(0,MaxNumberOfIterations+2)
    MyDeviance = rep(0,MaxNumberOfIterations+2)
    # Initialize the values of x and f(x)
    X_n_minus_one = X_zero
    X_n = X_one
    Z_n_minus_one = func(X_zero)[1]
    Z_n = func(X_one)[1]
    MyX[1] = X_n_minus_one
    MyX[2] = X_n
    MyDeviance[1] = abs(Z_n_minus_one)
    MyDeviance[2] = abs(Z_n)
    #Set up a while loop until we hit the required target accuracy or the max. number of steps
    cat("\nStart Secant")
    while ((i<MaxNumberOfIterations)&&(Deviation>Tolerance))
    {
        # Record the value of f(x) and f'(x), for the current x value.
        # I put them in a variable Z. Z[1]=f(x); Z[2]=f'(x)
        # To be safe, check that the function and it's derivative are defined at X (either could be NaN if you are unlucky)
        
        if ((Z_n_minus_one=="NaN")||(Z_n=="NaN")){
            cat("Function not defined error.\n")
            break
        }
        
        #Find the next X-value using Secant's formula. Let's call that value X
        X_n_plue_one = X_n - Z_n * (X_n - X_n_minus_one) / (Z_n - Z_n_minus_one)
        if(isTRUE(all.equal(func,F2))){
        if(X_n_plue_one < 0){
            X_n_plue_one = 10^(-10)
        }else if(is.infinite(X_n_plue_one)){
            X_n_plue_one = 10^10
        }
        }
        Deviation = abs(func(X_n_plue_one)[1])
        X_n_minus_one = X_n
        X_n = X_n_plue_one

        Z_n_minus_one = func(X_n_minus_one)[1]
        Z_n = func(X_n)[1]
        
        # increase the value of your iteration counter
        i<-i+1
        
        # if you like, have the program write out how it is getting on
        Index = i+2
        MyX[Index] = X_n
        MyDeviance[Index] = Deviation
        
        cat(paste("\nIteration ",i,":   X=",X_n,"  Y=",Z_n,"Deviation=", Deviation))

    }
    # output the result
    if (Deviation<Tolerance){
        cat(paste("\nFound the root point: ",X_n, "after ", i, "iterations"))
        return(cbind(XValue = MyX[1:Index], Deviance = MyDeviance[1:Index]))
    }else{
        cat(paste("\nConvergence failure. Deviation: ",Deviation, "after ", i, 	"iterations"))
        return(cbind(XValue = MyX, Deviance = MyDeviance))
    }
}

# Define your Newton-Raphson  function
NewtonRaphson<-function(func,StartingValue,Tolerance,MaxNumberOfIterations){
    #initialize a variable, Deviation (say), to record |f(x)| so that you know how far away you are from 0.
    #(So initialze it to some arbitrary large number)
    Deviation = 10^5
    #Set up a counter, i, to record how many iterations you have performed. Set it equal to 0
    i <- 0
    MyX = rep(0,MaxNumberOfIterations+1)
    MyDeviance = rep(0,MaxNumberOfIterations+1)
    # Initialize the values of x and f(x)
    X = StartingValue
    Z = rep(0,2)
    Z[1] = func(X)[1]
    Z[2] = func(X)[2]
    MyX[1] = X
    MyDeviance[1] = abs(Z[1])
    cat("\nStart Newton Raphson")
    #Set up a while loop until we hit the required target accuracy or the max. number of steps
    while ((i<MaxNumberOfIterations)&&(Deviation>Tolerance))
    {
        # Record the value of f(x) and f'(x), for the current x value.
        # I put them in a variable Z. Z[1]=f(x); Z[2]=f'(x)
        # To be safe, check that the function and it's derivative are defined at X (either could be NaN if you are unlucky)
        
        if ((Z[1]=="NaN")||(Z[2]=="NaN")){
            cat("Function or derivative not defined error.\n")
            break
        }
        
        #Find the next X-value using Newton-Raphson's formula. Let's call that value X
        X = X - Z[1]/Z[2]
        # fix inconvergence
        if(isTRUE(all.equal(func,F2))){
        if(X < 0){
            X = 10^(-10)
        }else if(is.infinite(X)){
            X = 10^10
        }
        }
        
        # calculate Deviation<- |f(x)-0|
        Z[1] = func(X)[1]
        Z[2] = func(X)[2]
        Deviation = abs(Z[1])
        Y = Z[1]
        # increase the value of your iteration counter
        i<-i+1
        
        # if you like, have the program write out how it is getting on
        cat(paste("\nIteration ",i,":   X=",X,"  Y=",Y, "Deviation=", Deviation))
        Index = i+1
        MyX[Index] = X
        MyDeviance[Index] = Deviation
    }
    
    # output the result
    if (Deviation<Tolerance){
        cat(paste("\nFound the root point: ",X, "after ", i, "iterations"))
        return(XValue = cbind(MyX[1:Index], Deviance = MyDeviance[1:Index]))
    }else{
        cat(paste("\nConvergence failure. Deviation: ",Deviation, "after ", i, 	"iterations"))
        return(XValue = cbind(MyX, Deviance = MyDeviance))
    }
}


##################################################
#################### analysis ####################
##################################################
# working directory setting
setwd("/Users/chengliangdong/Desktop/PM520/hw2/")

# 2.Test it, using x0=1 and x1=2 on: 1.cos(x)-x, 2.log(x)-exp(-x)
Secant_F1 = Secant(func=F1,X_zero=1, X_one=2,Tolerance=10^(-5),MaxNumberOfIterations=50)
Secant_F2 = Secant(func=F2,X_zero=1, X_one=2,Tolerance=10^(-5),MaxNumberOfIterations=50)

# 3.Compare it with the performance of Newton-Raphson on the same functions.

# a. Simple Comparison
NewtonRaphson_F1 = NewtonRaphson(func=F1,StartingValue=1,Tolerance=10^(-5),MaxNumberOfIterations=50)
NewtonRaphson_F2 =NewtonRaphson(func=F2,StartingValue=1,Tolerance=10^(-5),MaxNumberOfIterations=50)

# F1 y = cos(x)-x
pdf("hw2_Simple_comparison.pdf")
par(mfrow=c(2,2))
curve(cos(x)-x,min(min(Secant_F1[,1]),min(NewtonRaphson_F1[,1]))-0.5,max(max(Secant_F1[,1]),max(NewtonRaphson_F1[,1]))+0.5, lty = 2)
for(i in 1:nrow(Secant_F1)){
    segments(Secant_F1[i,1], 0, Secant_F1[i,1], F1(Secant_F1[i,1])[1], col="blue")
    text(Secant_F1[i,1], 0.2, labels = paste("X",i-1, sep=""), cex = 0.6)
    if(i < nrow(Secant_F1)){
        segments(Secant_F1[i,1], F1(Secant_F1[i,1])[1], Secant_F1[i+1,1], 0, col="blue", lty = 2)
    }
}

abline(h=0, lwd = 2) # this is the root we should find
title("cos(x)-x with Secant")


curve(cos(x)-x,min(min(Secant_F1[,1]),min(NewtonRaphson_F1[,1]))-0.5,max(max(Secant_F1[,1]),max(NewtonRaphson_F1[,1]))+0.5, lty = 2)
for(i in 1:nrow(NewtonRaphson_F1)){
    segments(NewtonRaphson_F1[i,1], 0, NewtonRaphson_F1[i,1], F1(NewtonRaphson_F1[i,1])[1], col="red")
    text(NewtonRaphson_F1[i,1], 0.2, labels = paste("X",i-1, sep=""), cex = 0.6)
    if(i < nrow(NewtonRaphson_F1)){
        segments(NewtonRaphson_F1[i,1], F1(NewtonRaphson_F1[i,1])[1], NewtonRaphson_F1[i+1,1], 0, col="red", lty = 2)
    }
}
abline(h=0, lwd = 2) # this is the root we should find
title("cos(x)-x with Newton Raphson")

# F2 y = log(x)-exp(-x)
curve(log(x)-exp(-x),min(min(Secant_F2[,1]),min(NewtonRaphson_F2[,1]))-0.5,max(max(Secant_F2[,1]),max(NewtonRaphson_F2[,1]))+0.5, lty = 2)
for(i in 1:nrow(Secant_F2)){
    segments(Secant_F2[i,1], 0, Secant_F2[i,1], F2(Secant_F2[i,1])[1], col="blue")
    text(Secant_F2[i,1], 0.2, labels = paste("X",i-1, sep=""), cex = 0.6)
    if(i < nrow(Secant_F2)){
        segments(Secant_F2[i,1], F2(Secant_F2[i,1])[1], Secant_F2[i+1,1], 0, col="blue", lty = 2)
    }
}
abline(h=0, lwd = 2) # this is the root we should find
title("log(x)-exp(-x) with Secant", cex = 0.6)


curve(log(x)-exp(-x),min(min(Secant_F2[,1]),min(NewtonRaphson_F2[,1]))-0.5,max(max(Secant_F2[,1]),max(NewtonRaphson_F2[,1]))+0.5, lty = 2) # supposing our function is f(x)=x^2 and we want to plot the function between -10 and 10
for(i in 1:nrow(NewtonRaphson_F2)){
    segments(NewtonRaphson_F2[i,1], 0, NewtonRaphson_F2[i,1], F2(NewtonRaphson_F2[i,1])[1], col="red")
    text(NewtonRaphson_F2[i,1], 0.2, labels = paste("X",i-1, sep=""), cex = 0.6)
    if(i < nrow(NewtonRaphson_F2)){
        segments(NewtonRaphson_F2[i,1], F2(NewtonRaphson_F2[i,1])[1], NewtonRaphson_F2[i+1,1], 0, col="red", lty = 2)
    }
}
abline(h=0, lwd = 2) # this is the root we should find
title("log(x)-exp(-x) with Newton Raphson", cex = 0.6)

dev.off()

# b. More Depth

# initializing value
# root of F1 = 0.739, F2 = 1.310

Length = 200
myStart_F1 = rep(0, Length)
myStart_F2 = rep(0, Length)
mySecant_Iteration_F1 = rep(0, Length)
myNewtonRaphson_Iteration_F1 = rep(0, Length)
mySecant_Iteration_F2 = rep(0, Length)
myNewtonRaphson_Iteration_F2 = rep(0, Length)

myNewtonRaphson_Time_F1 = cbind(rep(0, Length),rep(0, Length),rep(0, Length))
mySecant_Time_F1 = cbind(rep(0, Length),rep(0, Length),rep(0, Length))
myNewtonRaphson_Time_F2 = cbind(rep(0, Length),rep(0, Length),rep(0, Length))
mySecant_Time_F2 = cbind(rep(0, Length),rep(0, Length),rep(0, Length))

mySecant_Solution_F1 = rep(0, Length)
myNewtonRaphson_Solution_F1 = rep(0, Length)
mySecant_Solution_F2 = rep(0, Length)
myNewtonRaphson_Solution_F2 = rep(0, Length)

for(i in seq_along(myStart_F1)){
    myStart_F1[i] = 0.739 * 1.1^(i-1)
    myStart_F2[i] = 1.310 * 1.1^(i-1)
}

for(i in seq_along(myStart_F1)){
Secant_F1 = Secant(func=F1,X_zero=myStart_F1[i], X_one=myStart_F1[i]+1,Tolerance=10^(-5),MaxNumberOfIterations=500)
NewtonRaphson_F1 = NewtonRaphson(func=F1,StartingValue=myStart_F1[i],Tolerance=10^(-5),MaxNumberOfIterations=500)

Secant_F1_time = system.time(Secant(func=F1,X_zero=myStart_F1[i], X_one=myStart_F1[i]+1,Tolerance=10^(-5),MaxNumberOfIterations=500))
NewtonRaphson_F1_time = system.time(NewtonRaphson(func=F1,StartingValue=myStart_F1[i],Tolerance=10^(-5),MaxNumberOfIterations=500))

mySecant_Time_F1[i,1] = Secant_F1_time[[1]]
mySecant_Time_F1[i,2] = Secant_F1_time[[2]]
mySecant_Time_F1[i,2] = Secant_F1_time[[3]]

myNewtonRaphson_Time_F1[i,1] = NewtonRaphson_F1_time[[1]]
myNewtonRaphson_Time_F1[i,2] = NewtonRaphson_F1_time[[2]]
myNewtonRaphson_Time_F1[i,2] = NewtonRaphson_F1_time[[3]]

mySecant_Iteration_F1[i] = nrow(Secant_F1)
myNewtonRaphson_Iteration_F1[i] = nrow(NewtonRaphson_F1)

mySecant_Solution_F1[i] = Secant_F1[mySecant_Iteration_F1[i],1]
myNewtonRaphson_Solution_F1[i] = NewtonRaphson_F1[myNewtonRaphson_Iteration_F1[i],1]

Secant_F2 = Secant(func=F2,X_zero=myStart_F2[i], X_one=myStart_F2[i]+1,Tolerance=10^(-5),MaxNumberOfIterations=500)
NewtonRaphson_F2 = NewtonRaphson(func=F2,StartingValue=myStart_F2[i],Tolerance=10^(-5),MaxNumberOfIterations=500)

Secant_F2_time = system.time(Secant(func=F2,X_zero=myStart_F2[i], X_one=myStart_F2[i]+1,Tolerance=10^(-5),MaxNumberOfIterations=500))
NewtonRaphson_F2_time = system.time(NewtonRaphson(func=F2,StartingValue=myStart_F2[i],Tolerance=10^(-5),MaxNumberOfIterations=500))

mySecant_Time_F2[i,1] = Secant_F2_time[[1]]
mySecant_Time_F2[i,2] = Secant_F2_time[[2]]
mySecant_Time_F2[i,2] = Secant_F2_time[[3]]

myNewtonRaphson_Time_F2[i,1] = NewtonRaphson_F2_time[[1]]
myNewtonRaphson_Time_F2[i,2] = NewtonRaphson_F2_time[[2]]
myNewtonRaphson_Time_F2[i,2] = NewtonRaphson_F2_time[[3]]

mySecant_Iteration_F2[i] = nrow(Secant_F2)
myNewtonRaphson_Iteration_F2[i] = nrow(NewtonRaphson_F2)
mySecant_Solution_F2[i] = Secant_F2[mySecant_Iteration_F2[i],1]
myNewtonRaphson_Solution_F2[i] = NewtonRaphson_F2[myNewtonRaphson_Iteration_F2[i],1]
}

pdf("hw2_Initialization_comparison.pdf")
par(mfrow=c(2,2))
hist(mySecant_Iteration_F1, breaks = 20, main = "cos(x)-x with Secant", xlab = "Number of iterations")
abline(v=mean(mySecant_Iteration_F1), lwd = 2, col="red", cex = 0.2)
legend("topleft", lty = 1, col="red", legend = paste("mean=",mean(mySecant_Iteration_F1),sep="" ))
hist(myNewtonRaphson_Iteration_F1, breaks = 20, main = "cos(x)-x with Newton Raphson", xlab = "Number of iterations")
abline(v=mean(myNewtonRaphson_Iteration_F1), lwd = 2, col="red", cex = 0.2)
legend("topleft", lty = 1, col="red", legend = paste("mean=",mean(myNewtonRaphson_Iteration_F1),sep="" ))
hist(mySecant_Iteration_F2, breaks = 20, main = "log(x)-exp(-x) with Secant", xlab = "Number of iterations")
abline(v=mean(mySecant_Iteration_F2), lwd = 2, col="red", cex = 0.2)
legend("topleft", lty = 1, col="red", legend = paste("mean=",mean(mySecant_Iteration_F2),sep="" ))
hist(myNewtonRaphson_Iteration_F2, breaks = 20, main = "log(x)-exp(-x) with Newton Raphson", xlab = "Number of iterations")
abline(v=mean(myNewtonRaphson_Iteration_F2), lwd = 2, col="red", cex = 0.2)
legend("topleft", lty = 1, col="red", legend = paste("mean=",mean(myNewtonRaphson_Iteration_F2),sep="" ))
dev.off()

pdf("hw2_Initialization_comparison_along.pdf")
par(mfrow=c(2,2))
plot(myStart_F1-0.739, mySecant_Iteration_F1, type = "n", main = "cos(x)-x with Secant", xlab = "Distance from true solution", ylab="Number of iterations", ylim=c(1,500))
lines(myStart_F1-0.739, mySecant_Iteration_F1)
abline(h=mean(mySecant_Iteration_F1), lwd = 2, col="red", cex = 0.5)
legend("bottomright", lty = 1, col="red", legend = paste("mean=",mean(mySecant_Iteration_F1),sep="" ))
plot(myStart_F1-0.739, myNewtonRaphson_Iteration_F1, type = "n", main = "cos(x)-x with Newton Raphson", xlab = "Distance from true solution", ylab="Number of iterations", ylim=c(1,500))
lines(myStart_F1-0.739, myNewtonRaphson_Iteration_F1)
abline(h=mean(myNewtonRaphson_Iteration_F1), lwd = 2, col="red", cex = 0.5)
legend("bottomright", lty = 1, col="red", legend = paste("mean=",mean(myNewtonRaphson_Iteration_F1),sep="" ))
plot(myStart_F2-1.310, mySecant_Iteration_F2, type = "n", main = "log(x)-exp(-x) with Secant", xlab = "Distance from true solution", ylab="Number of iterations", ylim=c(1,130))
lines(myStart_F2-1.310, mySecant_Iteration_F2)
abline(h=mean(mySecant_Iteration_F2), lwd = 2, col="red", cex = 0.5)
legend("bottomright", lty = 1, col="red", legend = paste("mean=",mean(mySecant_Iteration_F2),sep="" ))
plot(myStart_F2-1.310, myNewtonRaphson_Iteration_F2, type = "n", main = "log(x)-exp(-x) with Newton Raphson", xlab = "Distance from true solution", ylab="Number of iterations",ylim=c(1,130))
lines(myStart_F2-1.310, myNewtonRaphson_Iteration_F2)
abline(h=mean(myNewtonRaphson_Iteration_F2), lwd = 2, col="red", cex = 0.5)
legend("topright", lty = 1, col="red", legend = paste("mean=",mean(myNewtonRaphson_Iteration_F2),sep="" ))

dev.off()


library(vioplot)
pdf("hw2_Time_consumption_userTime.pdf")
par(mfrow=c(2,2))
vioplot(mySecant_Time_F1[,1],myNewtonRaphson_Time_F1[,1],col="blue",  names =c("Secant","Newton Raphson"))
title("User time for cos(x)-x")
vioplot(mySecant_Time_F1[,2],myNewtonRaphson_Time_F1[,2],col="red",  names =c("Secant","Newton Raphson"))
title("System time for cos(x)-x")
vioplot(mySecant_Time_F2[,1],myNewtonRaphson_Time_F2[,1],col="green",  names =c("Secant","Newton Raphson"))
title("User time for log(x)-exp(-x)")
vioplot(mySecant_Time_F2[,2],myNewtonRaphson_Time_F2[,2],col="purple",  names =c("Secant","Newton Raphson"))
title("System time for log(x)-exp(-x)")
dev.off()


pdf("hw2_Solution_comparison.pdf")
par(mfrow=c(2,2))
hist(mySecant_Solution_F1, breaks = 20, main = "cos(x)-x with Secant", xlab = "Solutions")
abline(v=mean(mySecant_Solution_F1), lwd = 2, col="red", cex = 0.2)
legend("topleft", lty = 1, col="red", legend = paste("mean=",round(mean(mySecant_Solution_F1), digits = 4),sep="" ))
hist(myNewtonRaphson_Solution_F1, breaks = 20, main = "cos(x)-x with Newton Raphson", xlab = "Solutions")
abline(v=mean(myNewtonRaphson_Solution_F1), lwd = 2, col="red", cex = 0.2)
legend("topleft", lty = 1, col="red", legend =  paste("mean=",signif(mean(myNewtonRaphson_Solution_F1), digits = 4),sep="" ))
hist(mySecant_Solution_F2, breaks = 20, main = "log(x)-exp(-x) with Secant", xlab = "Solutions")
abline(v=mean(mySecant_Solution_F2), lwd = 2, col="red", cex = 0.2)
legend("topright", lty = 1, col="red", legend =  paste("mean=",round(mean(mySecant_Solution_F2), digits=4),sep="" ))
hist(myNewtonRaphson_Solution_F2, breaks = 20, main = "log(x)-exp(-x) with Newton Raphson", xlab = "Solutions")
abline(v=mean(myNewtonRaphson_Solution_F2), lwd = 2, col="red", cex = 0.2)
legend("topright", lty = 1, col="red", legend =  paste("mean=",round(mean(myNewtonRaphson_Solution_F2), digits = 4),sep="" ))
dev.off()


pdf("hw2_Solution_comparison_along.pdf")
par(mfrow=c(2,2))
plot(myStart_F1-0.739, mySecant_Solution_F1, type = "n", main = "cos(x)-x with Secant", xlab = "Distance from true solution", ylab="Number of Solutions", ylim=c(min(myNewtonRaphson_Solution_F1),max(max(myNewtonRaphson_Solution_F1), max(mySecant_Solution_F1))))
lines(myStart_F1-0.739, mySecant_Solution_F1)
abline(h=mean(mySecant_Solution_F1), lwd = 2, col="red", cex = 0.5)
legend("bottomright", lty = 1, col="red", legend = paste("mean=",mean(mySecant_Solution_F1),sep="" ))
plot(myStart_F1-0.739, myNewtonRaphson_Solution_F1, type = "n", main = "cos(x)-x with Newton Raphson", xlab = "Distance from true solution", ylab="Number of Solutions",ylim=c(min(myNewtonRaphson_Solution_F1),max(max(myNewtonRaphson_Solution_F1), max(mySecant_Solution_F1))))
lines(myStart_F1-0.739, myNewtonRaphson_Solution_F1)
abline(h=mean(myNewtonRaphson_Solution_F1), lwd = 2, col="red", cex = 0.5)
legend("bottomright", lty = 1, col="red", legend = paste("mean=",mean(myNewtonRaphson_Solution_F1),sep=""))
plot(myStart_F2-1.310, mySecant_Solution_F2, type = "n", main = "log(x)-exp(-x) with Secant", xlab = "Distance from true solution", ylab="Number of Solutions", ylim=c(1.30978, 1.30982))
lines(myStart_F2-1.310, mySecant_Solution_F2)
abline(h=mean(mySecant_Solution_F2), lwd = 2, col="red", cex = 0.5)
legend("topright", lty = 1, col="red", legend = paste("mean=",mean(mySecant_Solution_F2),sep="" ))
plot(myStart_F2-1.310, myNewtonRaphson_Solution_F2, type = "n", main = "log(x)-exp(-x) with Newton Raphson", xlab = "Distance from true solution", ylab="Number of Solutions", ylim=c(1.30978, 1.30982))
lines(myStart_F2-1.310, myNewtonRaphson_Solution_F2)
abline(h=mean(myNewtonRaphson_Solution_F2), lwd = 2, col="red", cex = 0.5)
legend("topright", lty = 1, col="red", legend = paste("mean=",mean(myNewtonRaphson_Solution_F2),sep=""))

dev.off()



