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
		MyTime = MyTime + 1
        p <- runif(1)
		if (p<(k-1)/(Theta+k-1)){
			# do a coalescence
            CoalescenceLinesIndex = sample(1:length(SurvivingLines), 2)
            CoalescenceLines = SurvivingLines[CoalescenceLinesIndex]
			# update the list SurvivingLines, reducing its length as we go
            if(length(SurvivingLines) >= 2 ){
                SurvivingLines <- c(SurvivingLines, max(SurvivingLines)+ 1)
            }
            SurvivingLines <- SurvivingLines[!SurvivingLines == CoalescenceLines[1] & !SurvivingLines == CoalescenceLines[2]]
            # update descendants
            Tree$LeftDescendants[Tree$Nodes == max(SurvivingLines)] = min(CoalescenceLines)
            Tree$RightDescendants[Tree$Nodes == max(SurvivingLines)] = max(CoalescenceLines)
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



## Here's the beginnings of the code you need to run the simulations
SampleSize<-5    # this problem has a fixed sample size
MyTree<-MakeTree(SampleSize)  # initialize tree matrix 
MaxTheta <-  5
TotalNumberOfReps <- 100
for (Theta in 1:MaxTheta){    # loop through the mutation rates we wish to consider
  for (Reps in 1:TotalNumberOfReps){  # replicate each situation TotalNumberOfReps times
		TotalNumberOfMutations<-0
		MyTree<-InitializeTree(MyTree)  # get the tree matrix ready to store a new tree
		MyTree<-SimulateTree(MyTree,Theta)  # Simulate A New Tree
		# next line counts the number of mutations and update our total count
		TotalNumberOfMutatations<-TotalNumberOfMutations+CountTheNumberOfMutations(MyTree)	
	}	
	# calculate the average number of mutations
	AverageNumberOfMutationsForThisSituation<-TotalNumberOfMutations/TotalNumberOfReps
	# and then store AverageNumberOfMutationsForThisSituation somewhere
}

# At this point you have the average number of mutations for each Theta-value
# So now you just need to draw summary plots.







