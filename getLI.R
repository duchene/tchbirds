# This function is to calculate the latitudinal index as described in the manuscript, given the latitudinal ranges of each species.

getLI <- function(latrans){

      LIs <- vector()

	for(i in 1:nrow(latrans)){
	      if(latrans[i,1] > -23.5 && latrans[i,2] < 23.5){ LIs[i] <- 0; next }
	      if(all(latrans[i,] > 23.5)){ LIs[i] <- 1; next }
	      if(all(latrans[i,] < -23.5)){ LIs[i] <- -1; next }

	      range <- latrans[i, 2] - latrans[i, 1]

	      if(any(latrans[i,] > 23.5) && any(latrans[i,] < -23.5)){
		  temp <- (latrans[i,2] - 23.5) + (abs(latrans[i,1]) - 23.5)
		  temprop <- temp / range
		  LIs[i] <- (1 - temprop) - temprop

	       } else if(any(latrans[i,] > 23.5)){
		  temp <- latrans[i,2] - 23.5
		  temprop <- temp / range
		  LIs[i] <- temprop

	       } else {
		  trop <- latrans[i,1] + 23.5
                  troprop <- trop / range
		  LIs[i] <- troprop

		}
	
	}

	return(LIs)

}