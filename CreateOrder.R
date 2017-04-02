#Uses hierarchical Clustering by columns, rows, or both.
#If a similarity matrix is used then it won't corellate first.
#df = A datframe
#order options are both, row,column
#xblocks/yblocks = the amount of blocks a matrix should be broken into for clustering
#simMat = a Logical value that is TRUE if df is a symmetrical similarity/distance matrix

createorder <-function(df, order="both", simMat= FALSE,xblocks=10, yblocks=10,...){
  
  
  print("Creating Corellation")
  #If a similarity matrix has been used there is no need to perform a corellation
  if(simMat==TRUE){
    corRow<-df
  } else{
    corRow <- switch(order, 
                     both =bigcor(t(df), nblocks = yblocks) %>%as.ffdf %>%
                       as.data.frame,
                     row = bigcor(t(df), nblocks = yblocks) %>%as.ffdf %>% 
                       as.data.frame,
                     column = 0)
    
  }
  #removes NA's as these cause hclust to malfunction  
  corRow[is.na(corRow)]<-0
  print("Performing Row Clustering")
  #Extract the clustered row order
  Roworder <-switch(order,
                    both = (as.dist(sqrt(2*(1-corRow)))  %>% hclust(.))$order,
                    row = (as.dist(sqrt(2*(1-corRow)))  %>% hclust(.))$order,
                    column = 1:nrow(df)
  )
  rm(corRow)
  gc()
  
  if(simMat==TRUE){
    corCol <-df
  } else{
    corCol <- switch(order,
                     both= bigcor(df, nblocks = xblocks) %>%as.ffdf %>%
                       as.data.frame,
                     row = 0,
                     column = bigcor(df, nblocks = xblocks) %>%as.ffdf %>%
                       as.data.frame
    )
  }
  #removes NA's as these cause hclust to malfunction
  corCol[is.na(corCol)]<-0
  
  print("Performing Column Clustering")
  Colorder<- switch(order,
                    both = (as.dist(sqrt(2*(1-corCol)))  %>% hclust(.))$order,
                    row = 1:ncol(df),
                    column =(as.dist(sqrt(2*(1-corCol)))  %>% hclust(.))$order
  )
  rm(corCol)
  gc()
  #Return the Order in a named list
  output <-list(Colorder, Roworder)
  names(output)<- c("Colorder", "Roworder")
  return(output)
}