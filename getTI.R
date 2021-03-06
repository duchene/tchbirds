# This function is used to calculate the tropicality index (Kerkhoff et al. 2012; PNAS) for each species given their latitudinal ranges.

getTI <- function(latrans){

      TIs <- vector()

	for(i in 1:nrow(latrans)){
	      if(any(latrans[i,] > 23.5) || any(latrans[i,] < -23.5)){
	      	if(all(latrans[i,] > 23.5) || all(latrans[i,] < -23.5)){ TIs[i] <- -1; next }
	        
		range <- latrans[i, 2] - latrans[i, 1]
		
		if(any(latrans[i,] > 23.5) && any(latrans[i,] < -23.5)){
		  temp <- (latrans[i,2] - 23.5) + (abs(latrans[i,1]) - 23.5)
		  temprop <- temp / range
		  TIs[i] <- (1 - temprop) - temprop

		} else if(any(latrans[i,] > 23.5)){
		  temp <- latrans[i,2] - 23.5
		  temprop <- temp / range
		  TIs[i] <- (1 - temprop) - temprop

		} else if(any(latrans[i,] < -23.5)){
		  temp <- abs(latrans[i,1]) - 23.5
                  temprop <- temp / range
		  TIs[i] <- (1 - temprop) - temprop

		}
	      
	      } else { TIs[i] <- 1 }
	
	}

	return(TIs)

}