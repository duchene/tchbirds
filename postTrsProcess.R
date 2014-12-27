require(phangorn)
require(geiger)
require(phytools)

# For any number of posterior trees, this function produces ancestral values of latitude, LI, TI, binned values of TI and LI for all nodes, transition matrices of the frequency of events between latitudinal zones of LI and TI, and standardized phylogenetic diversities for each latitudinal band.

getpostreResults <- function(postres, tiplats, tipTIs, tipLIs, latlamb, tilamb, lilamb, rands = 1000){
		 
		 reslist <- list(); reslist[[1]] <- list(); reslist[[2]] <- list(); reslist[[3]] <- list(); reslist[[4]] <- list(); reslist[[5]] <- list(); reslist[[6]] <- list(); reslist[[7]] <- list(); reslist[[8]] <- list(); reslist[[9]] <- list(); reslist[[10]] <- list(); reslist[[11]] <- list(); reslist[[12]] <- list()
		 names(reslist) <- c("anclat", "ancti", "ancli", "tipTIbins", "ancTIbins", "tipLIbins", "ancLIbins", "TItransmat", "LItransmat", "LIancestmat", "PDz", "tippyres")

		 for(i in 1:length(postres)){
		       print(paste("Started processing tree", i))

		       trantrlat <- rescale(postres[[i]], "lambda", latlamb)
		       trantrti <- rescale(postres[[i]], "lambda", tilamb)
		       trantrli <- rescale(postres[[i]], "lambda", lilamb)
		       print("Completed tree transformations")
		       tiplats <- tiplats[postres[[i]]$tip.label]
		       tipTIs <- tipTIs[postres[[i]]$tip.label]
		       tipLIs <- tipLIs[postres[[i]]$tip.label]
		       
		       anclat <- fastAnc(trantrlat, tiplats)
		       ancti <- getRandEdgeStates(trantrti, tipTIs, rands)
		       ancli <- getRandEdgeStates(trantrli, tipLIs, rands)
		       print("Ancestral states estimated")
		       tipTIbins <- sapply(tipTIs, function(x) if(x <= (-0.5)){ x <- "te" } else if(x >= 0.5){ x <- "tr" } else if(x >= 0){ x <- "str" } else { x <- "ste" })

		       ancTIbins <- sapply(ancti[1,], function(x) if(x <= (-0.5)){ x <- "te" } else if(x >= 0.5){ x <- "tr" } else if(x >= 0){ x <- "str" } else { x <- "ste" })

		       tipLIbins <- sapply(tipLIs, function(x) if(x <= (-0.75)){ x <- "s" } else if(x >= 0.75){ x <- "n" } else if(x >= 0.25){ x <- "tn" } else if(x <= -0.25){ x <- "ts" } else { x <- "t"})

		       ancLIbins <- sapply(ancli[1,], function(x) if(x <= (-0.75)){ x <- "s" } else if(x >= 0.75){ x <- "n" } else if(x >= 0.25){ x <- "tn" } else if(x <= -0.25){ x <- "ts" } else { x <- "t"})

		       TItransmat <- getLatTM(birdtr, ancTIbins, tipTIbins, size = 4)
		       LItransmat <- getLatTM(birdtr, ancLIbins, tipLIbins, size = 5)
		       LIancestmat <- getAncestry(postres[[i]], ancLIbins, tipLIbins)
		       PDz <- getlatPD(birdtr, birlats, latbins, rands)
		       print("Transition matrices and PDz calculated")

		       TIbin <- tipTIs; TIbin[which(TIbin >= 0)] <- 0; TIbin[which(TIbin < 0)] <- 1
		       tippyres <- tippystat(birdtr, TIbin, 100)
		       print("tippy results estimated")

		       reslist[[1]][[i]] <- anclat; reslist[[2]][[i]] <- ancti; reslist[[3]][[i]] <- ancli; reslist[[4]][[i]] <- tipTIbins; reslist[[5]][[i]] <- ancTIbins; reslist[[6]][[i]] <- tipLIbins; reslist[[7]][[i]] <- ancLIbins; reslist[[8]][[i]] <- TItransmat; reslist[[9]][[i]] <- LItransmat; reslist[[12]][[i]] <- LIancestmat; reslist[[11]][[i]] <- PDz; reslist[[12]][[i]] <- tippytyres


		 }

		 return(reslist)

}