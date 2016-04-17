#########################################################
########################## hw3 ##########################
#########################################################

######################### No. 1 #########################
# 1. write code to simulate coalescent trees. Done.
# 2. how does the expeced height of the tree vary as a function of the sample size?
# 3. simulate 1,000 or 10,000 trees, for each of a range of n values n=2, n=3 ... n=30
#   make a plot of mean heights of the trees for each n
# 4. add the curve 2(1-1/n) to your plot use curve() function
#   comment what you see

######################### No. 2 #########################
# 1. write a function to see which of the last two lines each sample number belongs to
# 2. sample size of 50. what is the distribution of the number of descendants of the left ancestor?

######################### No. 3 #########################
# 1. code up a version of your coalescent code that includes mutation. Simulate trees and count how many mutations there are on the tree
#   a. sample size of 5, plot the mean number of mutations as a function of theta (theta=1,2,3...10)
#   b. theta=2, plot the mean number of mutations as a function of the sample size (n=2,3,4...20)
# 2. do either of these follow an obvious relationship?

######################### No. 4 #########################
# 1. estimate the mean total length of all external branches as a function of n (n=2,3,4...10)
#   comment your results

setwd("/Users/chengliangdong/Desktop/PM520/hw3/")

######################### No. 1 #########################
# 1. write code to simulate coalescent trees.
# Here's a list containing all the elements of a tree
MakeTree<-function(SampleSize){
	Tree<-list(
	Nodes= 1:(2*SampleSize-1),    # note that we can name each element of the list as we go
	LeftDescendants= rep(0,2*SampleSize-1), # note also that we need to put a comma after each list element, except for the last one
	RightDescendants= rep(0,2*SampleSize-1),
	Times= rep(0,2*SampleSize-1), 	
	MutationsOnBranch=rep(0,2*SampleSize-1)
	)
	return(Tree)	
}

# A function to empty the tree matrix of all its non-fixed contents
InitializeTree<-function(Tree){
    SampleSize<-(length(Tree$Nodes)+1)/2
    Tree<-list(
    Nodes= 1:(2*SampleSize-1),    # note that we can name each element of the list as we go
    LeftDescendants= rep(0,2*SampleSize-1), # note also that we need to put a comma after each list element, except for the last one
    RightDescendants= rep(0,2*SampleSize-1),
    Times= rep(0,2*SampleSize-1),
    MutationsOnBranch=rep(0,2*SampleSize-1)
    )
    return(Tree)
} 

# A function to count the mutations on a tree
CountTheNumberOfMutations<-function(Tree){
    return(sum(Tree$MutationsOnBranch))
} 

## Here's the beginnings of a function to simulate a tree with mutation, using the tree structure defined above
SimulateTree<-function(Tree,Theta){
	SampleSize<-(length(Tree$Nodes)+1)/2   # we can calculate the sample size, rather than passing it as an argument
	k<-SampleSize   # initialize K, the variable that will keep track of how many lines of ancestry we have
	SurvivingLines<-seq(1:SampleSize)  # use this to keep track of what lines of ancestry are alive
    MyTime = 0
	while (k>1){
	  # How long to the next event? The time will have an exponential distribution, 
	  # with parameter k(k-1)/2 + Theta/2    [=k(k-1+Theta)/2]
	  # is the next event a coalescence or a mutation?  P(coalescence)= (k-1)/(Theta+k-1)
		MyTime = MyTime + SimualteExponentialRV(k*(k-1)/2+Theta/2)
        p <- runif(1)
		if (p<((k-1)/(Theta+k-1))){
			# do a coalescence
            CoalescenceLinesIndex = sort(sample(1:length(SurvivingLines), 2, replace = F))
            CoalescenceLines = SurvivingLines[CoalescenceLinesIndex]
			# update the list SurvivingLines, reducing its length as we go
            if(p >= 0.5){
                SurvivingLines[SurvivingLines==CoalescenceLines[1]] = max(SurvivingLines)+1
                SurvivingLines = SurvivingLines[SurvivingLines!=CoalescenceLines[2]]
            }else{
                SurvivingLines[SurvivingLines==CoalescenceLines[2]] = max(SurvivingLines)+1
                SurvivingLines = SurvivingLines[SurvivingLines!=CoalescenceLines[1]]
            }
            # update descendants
            Tree$LeftDescendants[Tree$Nodes == max(SurvivingLines)] = CoalescenceLines[1]
            Tree$RightDescendants[Tree$Nodes == max(SurvivingLines)] = CoalescenceLines[2]
            # update time
            Tree$Times[Tree$Nodes == max(SurvivingLines)] <- MyTime
            # decrease the count of surviving lines
			k <- k-1
		}else{
			# add a mutation - it is equally likely to occur to any of the surviving lines
			# it might be worth adding an error check like the following here
			if (k != length(SurvivingLines)){
                stop()
            }
            # we have messed up somehow, so stop execution
			# pick a line at random
			p<-runif(1)
			LineWeChoose<-SurvivingLines[ceiling(k*p)]
			Tree$MutationsOnBranch[LineWeChoose]<-Tree$MutationsOnBranch[LineWeChoose]+1
			# note that we don't reduce k by 1 here since no line was lost
            # note that we don't need to update time as well
		}
	}
	return (Tree)
}

SimualteExponentialRV <- function(lambda){
    u<-runif(1,0,1)
    ConditionedExpRV<- (-1/lambda)*log(u)
    return(ConditionedExpRV)
}



# 2. how does the expeced height of the tree vary as a function of the sample size?
# 3. simulate 1,000 or 10,000 trees, for each of a range of n values n=2, n=3 ... n=30
#   make a plot of mean heights of the trees for each n
# 4. add the curve 2(1-1/n) to your plot use curve() function
#   comment what you see
## Here's the beginnings of the code you need to run the simulations
SampleSize<-2:30    # this problem has a fixed sample size
MeanHeight=rep(0,length(SampleSize))
for(i in seq_along(SampleSize)){
    MyTree<-MakeTree(SampleSize[i])  # initialize tree matrix
    Theta<- 0
    TotalNumberOfReps <- 1000
    CurrentHeight = rep(0,TotalNumberOfReps)
    for (Reps in 1:TotalNumberOfReps){  # replicate each situation TotalNumberOfReps times
            TotalNumberOfMutations<-0
            MyTree<-InitializeTree(MyTree)  # get the tree matrix ready to store a new tree
            MyTree<-SimulateTree(MyTree,Theta)  # Simulate A New Tree
            CurrentHeight[Reps] = max(MyTree$Times)
    }
    MeanHeight[i] = mean(CurrentHeight)
}
pdf("hw3_question1a.pdf")
plot(2:30, MeanHeight, ylab="Mean Height of Coalescent Tree", xlab="Sample Size")
title("Relationship between Sample Size and Mean Height (1000 simulations)")
curve(2*(1-1/x), add = T, col = "red")
legend("topleft", lty = 1, col="red", legend = "y=2(1-1/n)")
dev.off()

SampleSize<-2:30    # this problem has a fixed sample size
MeanHeight=rep(0,length(SampleSize))
for(i in seq_along(SampleSize)){
    MyTree<-MakeTree(SampleSize[i])  # initialize tree matrix
    Theta<- 0
    TotalNumberOfReps <- 10000
    CurrentHeight = rep(0,TotalNumberOfReps)
    for (Reps in 1:TotalNumberOfReps){  # replicate each situation TotalNumberOfReps times
        TotalNumberOfMutations<-0
        MyTree<-InitializeTree(MyTree)  # get the tree matrix ready to store a new tree
        MyTree<-SimulateTree(MyTree,Theta)  # Simulate A New Tree
        CurrentHeight[Reps] = max(MyTree$Times)
    }
    MeanHeight[i] = mean(CurrentHeight)
}
pdf("hw3_question1b.pdf")
plot(2:30, MeanHeight, ylab="Mean Height of Coalescent Tree", xlab="Sample Size")
title("Relationship between Sample Size and Mean Height (10000 simulations)")
curve(2*(1-1/x), add = T, col = "red")
legend("topleft", lty = 1, col="red", legend = "y=2(1-1/n)")
dev.off()



######################### No. 2 #########################
# 1. write a function to see which of the last two lines each sample number belongs to

GetNumberOfDescendants = function(Tree, Side="left"){
    NumberOfDescendants = 0
    MyFinalChildren = GetFinalChildren(Tree)
    if(Side == "left"){
        MyActiveParents = Tree$LeftDescendants[length(Tree$LeftDescendants)]
    }else{
        MyActiveParents = Tree$RightDescendants[length(Tree$RightDescendants)]
    }
    if(MyActiveParents <= max(MyFinalChildren)){
        NumberOfDescendants = 1
    }else{
        while(length(MyActiveParents) > 0){
            MyChildren = GetChildren(Tree, MyActiveParents)
            NumberOfFinalDescendant = sum(MyChildren <= max(MyFinalChildren))
            NumberOfDescendants = NumberOfDescendants + NumberOfFinalDescendant
            MyActiveParents = MyChildren[MyChildren>max(MyFinalChildren)]
        }
    }
    return(NumberOfDescendants)
}


GetChildren = function(Tree, ActiveParents){
    MyChildren = rep(0, 2*length(ActiveParents))
    for(i in 1:length(ActiveParents)){
        MyChildren[2*i-1] = Tree$LeftDescendants[ActiveParents[i]]
        MyChildren[2*i] = Tree$RightDescendants[ActiveParents[i]]
    }
    return(MyChildren)
}


GetFinalChildren = function(Tree){
    return(Tree$Nodes[Tree$LeftDescendants==0])
}

# 2. sample size of 50. what is the distribution of the number of descendants of the left ancestor?
SampleSize<-50    # this problem has a fixed sample size
MyTree<-MakeTree(SampleSize)  # initialize tree matrix
Theta<- 0

TotalNumberOfReps <- 1000
MyLeftDescendentCount = rep(0,TotalNumberOfReps)
for (Reps in 1:TotalNumberOfReps){  # replicate each situation TotalNumberOfReps times
    TotalNumberOfMutations<-0
    MyTree<-InitializeTree(MyTree)  # get the tree matrix ready to store a new tree
    MyTree<-SimulateTree(MyTree,Theta)  # Simulate A New Tree
    MyLeftDescendentCount[Reps]<-GetNumberOfDescendants(MyTree,"right")
}
pdf("hw3_question2a.pdf")
hist(MyLeftDescendentCount, breaks = seq(0,50,by=1), xlab="Number of Descendants of Left Ancestor", main="Distributin of Descendants of Left Ancestor Count (1000 simulations)")
MyFrequency = data.frame(table(MyLeftDescendentCount))$Freq
abline(h=mean(MyFrequency), col = "red")
legend("topleft", lty = 1, col="red", legend = paste("mean=", round(mean(MyFrequency), digits=2), ", sd=", round(sd(MyFrequency), digits =2)))
dev.off()


TotalNumberOfReps <- 10000
MyLeftDescendentCount = rep(0,TotalNumberOfReps)
for (Reps in 1:TotalNumberOfReps){  # replicate each situation TotalNumberOfReps times
    TotalNumberOfMutations<-0
    MyTree<-InitializeTree(MyTree)  # get the tree matrix ready to store a new tree
    MyTree<-SimulateTree(MyTree,Theta)  # Simulate A New Tree
    MyLeftDescendentCount[Reps]<-GetNumberOfDescendants(MyTree,"right")
}
pdf("hw3_question2b.pdf")
hist(MyLeftDescendentCount, breaks = seq(0,50,by=1), xlab="Number of Descendants of Left Ancestor", main="Distributin of Descendants of Left Ancestor Count (10000 simulations)")
MyFrequency = data.frame(table(MyLeftDescendentCount))$Freq
abline(h=mean(MyFrequency), col = "red")
legend("topleft", lty = 1, col="red", legend = paste("mean=", round(mean(MyFrequency), digits=2), ", sd=", round(sd(MyFrequency), digits =2)))
dev.off()



######################### No. 3 #########################
# 1. code up a version of your coalescent code that includes mutation. Simulate trees and count how many mutations there are on the tree
# Done.

#   a. sample size of 5, plot the mean number of mutations as a function of theta (theta=1,2,3...10)
#   b. theta=2, plot the mean number of mutations as a function of the sample size (n=2,3,4...20)
# 2. do either of these follow an obvious relationship?


#   a. sample size of 5, plot the mean number of mutations as a function of theta (theta=1,2,3...10)
SampleSize<-5    # this problem has a fixed sample size
MyTree<-MakeTree(SampleSize)  # initialize tree matrix
Theta<- seq(1,10, by=1)
MeanMutationCount = rep(0, length(Theta))

TotalNumberOfReps <- 1000
for(i in seq_along(Theta)){
    MyMutationCount=rep(0,TotalNumberOfReps)
    for (Reps in 1:TotalNumberOfReps){  # replicate each situation TotalNumberOfReps times
        TotalNumberOfMutations<-0
        MyTree<-InitializeTree(MyTree)  # get the tree matrix ready to store a new tree
        MyTree<-SimulateTree(MyTree,Theta[i])  # Simulate A New Tree
        MyMutationCount[Reps]=CountTheNumberOfMutations(MyTree)
    }
    MeanMutationCount[i] = mean(MyMutationCount)
}
pdf("hw3_question3a1.pdf")
plot(Theta, MeanMutationCount, xlab="Theta", ylab = "Mean Number of Mutations", ylim=c(0,(max(MeanMutationCount)+1)),main="Relationship between Theta and Number of Mutations (1000 runs)")
abline(lm(MeanMutationCount~Theta), col="red")
legend("topleft", lty = 1, col="red", legend = "regression line")
dev.off()

TotalNumberOfReps <- 10000
for(i in seq_along(Theta)){
    MyMutationCount=rep(0,TotalNumberOfReps)
    for (Reps in 1:TotalNumberOfReps){  # replicate each situation TotalNumberOfReps times
        TotalNumberOfMutations<-0
        MyTree<-InitializeTree(MyTree)  # get the tree matrix ready to store a new tree
        MyTree<-SimulateTree(MyTree,Theta[i])  # Simulate A New Tree
        MyMutationCount[Reps]=CountTheNumberOfMutations(MyTree)
    }
    MeanMutationCount[i] = mean(MyMutationCount)
}

pdf("hw3_question3a2.pdf")
plot(Theta, MeanMutationCount, xlab="Theta", ylab = "Mean Number of Mutations", ylim=c(0,(max(MeanMutationCount)+1)),main="Relationship between Theta and Number of Mutations (10000 runs)")
abline(lm(MeanMutationCount~Theta), col="red")
legend("topleft", lty = 1, col="red", legend = "regression line")
dev.off()


#   b. theta=2, plot the mean number of mutations as a function of the sample size (n=2,3,4...20)
Theta<- 2
SampleSize<-seq(2,20,by=1)    # this problem has a fixed sample size
TotalNumberOfReps <- 1000
MeanMutationCount = rep(0, length(SampleSize))
for(i in seq_along(SampleSize)){
    MyTree<-MakeTree(SampleSize[i])  # initialize tree matrix
    MyMutationCount=rep(0,TotalNumberOfReps)
    for (Reps in 1:TotalNumberOfReps){  # replicate each situation TotalNumberOfReps times
        TotalNumberOfMutations<-0
        MyTree<-InitializeTree(MyTree)  # get the tree matrix ready to store a new tree
        MyTree<-SimulateTree(MyTree,Theta)  # Simulate A New Tree
        MyMutationCount[Reps]=CountTheNumberOfMutations(MyTree)
    }
    MeanMutationCount[i] = mean(MyMutationCount)
}
pdf("hw3_question3b1.pdf")
plot(SampleSize, MeanMutationCount, xlab="Sample Size", ylab = "Mean Number of Mutations", ylim=c(0,(max(MeanMutationCount)+1)),main="Sample Size and Number of Mutations (1000 runs)")
abline(lm(MeanMutationCount~SampleSize), col="red")
legend("topleft", lty = 1, col="red", legend = "regression line")
dev.off()

TotalNumberOfReps <- 10000
MeanMutationCount = rep(0, length(SampleSize))
for(i in seq_along(SampleSize)){
    MyTree<-MakeTree(SampleSize[i])  # initialize tree matrix
    MyMutationCount=rep(0,TotalNumberOfReps)
    for (Reps in 1:TotalNumberOfReps){  # replicate each situation TotalNumberOfReps times
        TotalNumberOfMutations<-0
        MyTree<-InitializeTree(MyTree)  # get the tree matrix ready to store a new tree
        MyTree<-SimulateTree(MyTree,Theta)  # Simulate A New Tree
        MyMutationCount[Reps]=CountTheNumberOfMutations(MyTree)
    }
    MeanMutationCount[i] = mean(MyMutationCount)
}
pdf("hw3_question3b2.pdf")
plot(SampleSize, MeanMutationCount, xlab="Sample Size", ylab = "Mean Number of Mutations", ylim=c(0,(max(MeanMutationCount)+1)),main="Sample Size and Number of Mutations (10000 runs)")
abline(lm(MeanMutationCount~SampleSize), col="red")
legend("topleft", lty = 1, col="red", legend = "regression line")
dev.off()


######################### No. 4 #########################
# write a function
GetExternalBranchLength= function(Tree){
    MyFinalChildren = GetFinalChildren(Tree)
    MyFinalTime = 0
    for(i in seq_along(MyFinalChildren)){
        if(sum(MyFinalChildren[i] == Tree$LeftDescendants)==1){
            MyFinalTime = MyFinalTime + Tree$Times[Tree$Nodes[Tree$LeftDescendants==MyFinalChildren[i]]]
        }else{
            MyFinalTime = MyFinalTime + Tree$Times[Tree$Nodes[Tree$RightDescendants==MyFinalChildren[i]]]
        }
    }
    return(MyFinalTime)
}

# 1. estimate the mean total length of all external branches as a function of n (n=2,3,4...10)
Theta = 0
SampleSize<-seq(2,10,by=1)    # this problem has a fixed sample size
TotalNumberOfReps <- 1000
MeanExternalBranchCount = rep(0, length(SampleSize))
for(i in seq_along(SampleSize)){
    MyTree<-MakeTree(SampleSize[i])  # initialize tree matrix
    MyExternalBranchCount=rep(0,TotalNumberOfReps)
    for (Reps in 1:TotalNumberOfReps){  # replicate each situation TotalNumberOfReps times
        TotalNumberOfMutations<-0
        MyTree<-InitializeTree(MyTree)  # get the tree matrix ready to store a new tree
        MyTree<-SimulateTree(MyTree,Theta)  # Simulate A New Tree
        MyExternalBranchCount[Reps]=GetExternalBranchLength(MyTree)
    }
    MeanExternalBranchCount[i] = mean(MyExternalBranchCount)
}
pdf("hw3_question4a.pdf")
plot(SampleSize, MeanExternalBranchCount, xlab="Sample Size", ylab = "Mean Total Length of External Branch", ylim=c(0,(max(MeanExternalBranchCount)+1)),main="Sample Size and Number of External Branch (1000 runs)")
abline(h=mean(MeanExternalBranchCount), col = "red")
legend("topleft", lty = 1, col="red", legend = paste("mean=", round(mean(MeanExternalBranchCount), digits = 2), "sd=", round(sd(MeanExternalBranchCount), digits = 2) ))
dev.off()

TotalNumberOfReps <- 10000
MeanExternalBranchCount = rep(0, length(SampleSize))
for(i in seq_along(SampleSize)){
    MyTree<-MakeTree(SampleSize[i])  # initialize tree matrix
    MyExternalBranchCount=rep(0,TotalNumberOfReps)
    for (Reps in 1:TotalNumberOfReps){  # replicate each situation TotalNumberOfReps times
        TotalNumberOfMutations<-0
        MyTree<-InitializeTree(MyTree)  # get the tree matrix ready to store a new tree
        MyTree<-SimulateTree(MyTree,Theta)  # Simulate A New Tree
        MyExternalBranchCount[Reps]=GetExternalBranchLength(MyTree)
    }
    MeanExternalBranchCount[i] = mean(MyExternalBranchCount)
}
pdf("hw3_question4b.pdf")
plot(SampleSize, MeanExternalBranchCount, xlab="Sample Size", ylab = "Mean Total Length of External Branch", ylim=c(0,(max(MeanExternalBranchCount)+1)),main="Sample Size and Number of External Branch (10000 runs)")
abline(h=mean(MeanExternalBranchCount), col = "red")
legend("topleft", lty = 1, col="red", legend = paste("mean=", round(mean(MeanExternalBranchCount), digits = 2), "sd=", round(sd(MeanExternalBranchCount), digits = 2) ))
dev.off()

Theta = 5
SampleSize<-seq(2,10,by=1)    # this problem has a fixed sample size
TotalNumberOfReps <- 1000
MeanExternalBranchCount = rep(0, length(SampleSize))
for(i in seq_along(SampleSize)){
    MyTree<-MakeTree(SampleSize[i])  # initialize tree matrix
    MyExternalBranchCount=rep(0,TotalNumberOfReps)
    for (Reps in 1:TotalNumberOfReps){  # replicate each situation TotalNumberOfReps times
        TotalNumberOfMutations<-0
        MyTree<-InitializeTree(MyTree)  # get the tree matrix ready to store a new tree
        MyTree<-SimulateTree(MyTree,Theta)  # Simulate A New Tree
        MyExternalBranchCount[Reps]=GetExternalBranchLength(MyTree)
    }
    MeanExternalBranchCount[i] = mean(MyExternalBranchCount)
}
pdf("hw3_question4c.pdf")
plot(SampleSize, MeanExternalBranchCount, xlab="Sample Size", ylab = "Mean Total Length of External Branch", ylim=c(0,(max(MeanExternalBranchCount)+1)),main="Sample Size and Number of External Branch (1000 runs)")
abline(h=mean(MeanExternalBranchCount), col = "red")
legend("topleft", lty = 1, col="red", legend = paste("mean=", round(mean(MeanExternalBranchCount), digits = 2), "sd=", round(sd(MeanExternalBranchCount), digits = 2) ))
dev.off()

TotalNumberOfReps <- 10000
MeanExternalBranchCount = rep(0, length(SampleSize))
for(i in seq_along(SampleSize)){
    MyTree<-MakeTree(SampleSize[i])  # initialize tree matrix
    MyExternalBranchCount=rep(0,TotalNumberOfReps)
    for (Reps in 1:TotalNumberOfReps){  # replicate each situation TotalNumberOfReps times
        TotalNumberOfMutations<-0
        MyTree<-InitializeTree(MyTree)  # get the tree matrix ready to store a new tree
        MyTree<-SimulateTree(MyTree,Theta)  # Simulate A New Tree
        MyExternalBranchCount[Reps]=GetExternalBranchLength(MyTree)
    }
    MeanExternalBranchCount[i] = mean(MyExternalBranchCount)
}
pdf("hw3_question4d.pdf")
plot(SampleSize, MeanExternalBranchCount, xlab="Sample Size", ylab = "Mean Total Length of External Branch", ylim=c(0,(max(MeanExternalBranchCount)+1)),main="Sample Size and Number of External Branch (10000 runs)")
abline(h=mean(MeanExternalBranchCount), col = "red")
legend("topleft", lty = 1, col="red", legend = paste("mean=", round(mean(MeanExternalBranchCount), digits = 2), "sd=", round(sd(MeanExternalBranchCount), digits = 2) ))
dev.off()

#   comment your results










