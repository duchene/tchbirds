require(phangorn)
require(geiger)
require(phytools)
require(picante)

# The following function produces a transition matrix to know the number of dispersal events across latitudinal zones.

getLatTM <- function(tree, ancstate, tipstate, size = 3){
	 
	 tipstate <- tipstate[tree$tip.label]
	 names(tipstate) <- 1:length(tree$tip.label)
	 ancstate <- c(ancstate, tipstate)
	 if(size == 3){
	       states <- c("s", "t", "n")
	       lattm <- matrix(0, nrow = 3, ncol = 3)
	       rownames(lattm) <- states
	       colnames(lattm) <- states
	       for(i in 1:nrow(tree$edge)){
	       	     ancestor <- ancstate[as.character(tree$edge[i,1])]
		     descendant <- ancstate[as.character(tree$edge[i,2])]
	       	     lattm[ancestor, descendant] <- lattm[ancestor, descendant] + 1

	       }

	 } else if(size == 4){
	   
	       states <- c("te", "ste", "str", "tr")
	       lattm <- matrix(0, nrow = 4, ncol = 4)
               rownames(lattm) <- states
               colnames(lattm) <- states
	       
	       for(i in 1:nrow(tree$edge)){
                     ancestor <- ancstate[as.character(tree$edge[i,1])]
                     descendant <- ancstate[as.character(tree$edge[i,2])]
                     lattm[ancestor, descendant] <- lattm[ancestor, descendant] + 1

               }


	 } else if(size == 5){

	       states <- c("s", "ts", "t", "tn", "n")
               lattm <- matrix(0, nrow = 5, ncol = 5)
               rownames(lattm) <- states
               colnames(lattm) <- states

               for(i in 1:nrow(tree$edge)){
                     ancestor <- ancstate[as.character(tree$edge[i,1])]
                     descendant <- ancstate[as.character(tree$edge[i,2])]
                     lattm[ancestor, descendant] <- lattm[ancestor, descendant] + 1

               }


	 }




	 return(lattm)

}
