#Compresses a data frame to the mean value of a square set of cells in a dataframe
#df = A datframe
#merge = the number of cell in the x and y direction to merge
#Requires dplyr 

CompressDf <-function(df,merge = 5){
  
  print("Compressing data along rows")
  
  totrows <-nrow(df)
  remainder <- merge-(totrows %%merge)
  numgroups <-(totrows +remainder)/merge
  
  #rearrange dataframe columns and aggreagte into the predefined groups of size merge
  #rows and columns are first ordered by similarity
  test <- df%>% as.data.frame %>%
    mutate(rowID =rep(1:numgroups,each=merge)[1:totrows]) %>%
    group_by(rowID) %>% summarise_all(funs(mean)) %>%
    select(-rowID) #add in -rows again is the rows data is commented back in
  
  print("Compressing data along Columns")
  #allows the grouping to be flexible in the case that the dataframe changes size
  totcols <-ncol(test)
  remainder <- merge-(totcols %%merge)
  numgroups <-(totcols +remainder)/merge
  
  
  #aggregate again transposing the data frame and aggregating by the smart meters
  test <- t(test) %>% data.frame %>% 
    mutate(rowID =rep(1:numgroups,each=merge)[1:totcols]) %>%
    group_by(rowID) %>% summarise_all(funs(mean)) %>%select(-rowID) %>%
    t %>% data.frame %>% mutate( rowID = 1:nrow(.)) 
  
}