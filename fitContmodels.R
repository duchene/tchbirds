require(geiger)

# Fit models of evolution to values of tropicality or latitudinality indices and latitudes for the birds.

getlatfit <- function(birtr, birdTI, birlats){

#models <- c("BM", "OU", "lambda", "white", "EB", "kappa")
models <- c("EB", "kappa")

fitTI <- matrix(NA, nrow = 1, ncol = 2)
#fitlat <- matrix(NA, nrow = 1, ncol = 2)

for(i in 1:4){

ti <- fitContinuous(birdtr, birdTI, model = models[i])
print(paste(models[i], "TI done"))
#lat <- fitContinuous(birdtr, birlats, model = models[i])
#print(paste(models[i], "lats done"))
fitTI <- rbind(fitTI, c(ti$opt$aicc, ti$opt$lnL))

#fitlat <- rbind(fitlat, c(lat$opt$aicc, lat$opt$lnL))

}

fitTI <- fitTI[2:5,]
#fitlat <- fitlat[2:5,]

colnames(fitTI) <- c("AICc", "lnLik");# colnames(fitlat) <- c("AICc", "lnLik")
rownames(fitTI) <- models;# rownames(fitlat) <- models

return(list(fitTI, fitlat))

}

#birdTI <- birdTI[birdtr$tip.label]; birlats <- birlats[birdtr$tip.label]; birdLI <- birdLI[birdtr$tip.label]
#ti <- fitContinuous(birdtr, birdTI, model = "lambda"); lat <- fitContinuous(birdtr, birlats, model = "lambda")

#titr <- transform(birdtr, "lambda", ti$opt$lambda); lattr <- transform(birdtr, "lambda", lat$opt$lambda); litr <- transform(birdtr, "lambda", li$opt$lambda)

#ancTI <- fastAnc(titr, birdTI); anclat <- fastAnc(lattr, birlats); ancLI <- fastAnc(litr, birdLI)

#tipTIbins <- sapply(birdTI, function(x) if(x <= (-0.5)){ x <- "te" } else if(x >= 0.5){ x <- "tr" } else if(x >= 0){ x <- "str" } else { x <- "ste" })

#ancTIbins <- sapply(ancTI, function(x) if(x <= (-0.5)){ x <- "te" } else if(x >= 0.5){ x <- "tr" } else if(x >= 0){ x <- "str" } else { x <- "ste" })

#tipLIbins <- sapply(birdLI, function(x) if(x <= (-0.75)){ x <- "s" } else if(x >= 0.75){ x <- "n" } else if(x >= 0.25){ x <- "tn" } else if(x <= -0.25){ x <- "ts" } else { x <- "t"})

#ancLIbins <- sapply(ancLI, function(x) if(x <= (-0.75)){ x <- "s" } else if(x >= 0.75){ x <- "n" } else if(x >= 0.25){ x <- "tn" } else if(x <= -0.25){ x <- "ts" } else { x <- "t"})

#LItransmat <- getLatTM(birdtr, ancLIbins, tipLIbins, size = 5)
#PDs <- getlatPD(birdtr, birlats, latbins)