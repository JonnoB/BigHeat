CompressDf <-function(df,mergey = 5, mergex = 5){
  #Compresses a data frame to the mean value of a square set of cells in a dataframe
  #df = A datframe
  #mergey = the number of cells y direction to merge
  #mergex = the number of cells x direction to merge
  #Requires dplyr   
  
  print("Compressing data along rows")
  df <-df[rev(1:nrow(df)),] #reverses the order so that the data is visualised correctly
  totrows <-nrow(df)
  remainder <- mergey-(totrows %%mergey)
  numgroups <-(totrows +remainder)/mergey
  
  #rearrange dataframe columns and aggreagte into the predefined groups of size mergey
  #rows and columns are first ordered by similarity
  df <- df%>% as.data.frame %>%
    mutate(rowID =rep(1:numgroups,each=mergey)[1:totrows]) %>%
    group_by(rowID) %>% summarise_all(funs(mean)) %>%
    select(-rowID) #add in -rows again is the rows data is commented back in
  
  print("Compressing data along Columns")
  #allows the grouping to be flexible in the case that the dataframe changes size
  totcols <-ncol(df)
  remainder <- mergex-(totcols %%mergex)
  numgroups <-(totcols +remainder)/mergex
  
  
  #aggregate again transposing the data frame and aggregating by the smart meters
  df <- t(df) %>% data.frame %>% 
    mutate(rowID =rep(1:numgroups,each=mergex)[1:totcols]) %>%
    group_by(rowID) %>% summarise_all(funs(mean)) %>%select(-rowID) %>%
    t %>% data.frame %>% mutate( rowID = 1:nrow(.)) 
  
}