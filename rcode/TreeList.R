SampleSize<-5  # use whatever value of n we have. In this example, n=5

MakeTree<-function(SampleSize){
	Tree<-list(
	Nodes= 1:(2*SampleSize-1),    # note that we can name each element of the list as we go
	LeftDescendants= rep(0,2*SampleSize-1), # note also that we need to put a comma after each list element, except for the last one
	RightDescendants= rep(0,2*SampleSize-1),
	Times= rep(0,2*SampleSize-1)  # no comma here
	)
	return(Tree)	
}

MyTree<-MakeTree(5)


