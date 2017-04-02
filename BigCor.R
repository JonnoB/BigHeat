#It's questionable weather there is a point to parallelising this, 
#as if you have enough RAM you don't need bigcor

bigcor <- function(x, nblocks = 10, verbose = TRUE, par = FALSE, ...)
{
  library(ff, quietly = TRUE)
  NCOL <- ncol(x)
  
  #sets saving options to the current directory for the temp file this prevents
  #errors when using server based systems such as AWS that have write restrictions
  
  
  ## preallocate square matrix of dimension
  ## ncol(x) in 'ff' single format
  corMAT <- ff(vmode = "single", dim = c(NCOL, NCOL))
  print("Converted to ff matrix")
  
  remainder <- nblocks-(NCOL %%nblocks)
  groupsize <-(NCOL +remainder)/nblocks
  
  ## split column numbers into 'nblocks' groups
  SPLIT <- split(1:NCOL, rep(1:nblocks,each=groupsize)[1:NCOL])
  
  ## create all unique combinations of blocks
  COMBS <- expand.grid(1:length(SPLIT), 1:length(SPLIT))
  COMBS <- t(apply(COMBS, 1, sort))
  COMBS <- unique(COMBS)
  print("unique combinations identified")
  
  ## iterate through each block combination, calculate correlation matrix
  ## between blocks and store them in the preallocated matrix on both
  ## symmetric sides of the diagonal
  
  if(par){
    mclapply(1:nrow(COMBS), function(i){
      COMB <- COMBS[i, ]
      G1 <- SPLIT[[COMB[1]]]
      G2 <- SPLIT[[COMB[2]]]
      if (verbose) cat("Block", COMB[1], "with Block", COMB[2], "\n")
      flush.console()
      COR <- cor(x[, G1], x[, G2], ...)
      corMAT[G1, G2] <- COR
      corMAT[G2, G1] <- t(COR)
      COR <- NULL
    },
    
    mc.cores = detectCores())
  } else {
    for (i in 1:nrow(COMBS)) {
      COMB <- COMBS[i, ]
      G1 <- SPLIT[[COMB[1]]]
      G2 <- SPLIT[[COMB[2]]]
      if (verbose) cat("Block", COMB[1], "with Block", COMB[2], "\n")
      flush.console()
      COR <- cor(x[, G1], x[, G2], ...)
      corMAT[G1, G2] <- COR
      corMAT[G2, G1] <- t(COR)
      COR <- NULL
    }
  }
  
  gc()
  return(corMAT)
}

#taken from http://www.r-bloggers.com/bigcor-large-correlation-matrices-in-r/