require(phangorn)

## This function receives a tree, the estimated latitude bins at each node, and the known latitude bins at each tip. The tips must be in the same order as in tree$tip.label, and the nodes must be named the same as the nodes in the edge object of the tree. The function provides a matrix of the proportion of ancestry for each latitudinal zone.

getAncestry <- function(tree, nodebins, tipbins){
	    
	    tips<- tipbins
	    names(tips) <- 1:length(tips)
	    allstates <- c(tips, nodebins)
	    bins <- unique(tipbins)
	    ancmat <- matrix(0, nrow = length(bins), ncol = length(bins))
	    colnames(ancmat) <- bins
	    rownames(ancmat) <- bins

	    for(i in 1:nrow(tree$edge)){
	    	  decstate <- allstates[as.character(tree$edge[i, 2])]
		  ancstate <- allstates[as.character(tree$edge[i, 1])]
		  ancmat[decstate, ancstate] <- ancmat[decstate, ancstate] + 1

	    }

	    #rowtots <- rowSums(ancmat)
	    
	    #for(i in 1:length(bins)){
	    #	  ancmat[i, ] <- ancmat[i, ] / rowtots[i]
	    #}

	    return(ancmat)


}