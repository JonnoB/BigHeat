#Produces a an aggregated heat map
#df = A datframe
#merge = the number of cell in the x and y direction to merge
#mid = The mid point of the heatmap scale, for corellation this would be 0.5
#Requires dplyr and ggplot

bigheat <-function(df,merge = 5,mid=0.5, legend="Aggregated score"){
  
  print("Compressing data along rows")
  
  totrows <-nrow(df)
  remainder <- merge-(totrows %%merge)
  numgroups <-(totrows +remainder)/merge
  
  #rearrange dataframe columns and aggreagte into the predefined groups of size merge
  #rows and columns are first ordered by similarity
  test <- df%>% as.data.frame %>%
    mutate(rowID =rep(1:numgroups,each=merge)[1:totrows]) %>%
    group_by(rowID) %>% summarise_each(funs(mean)) %>%
    select(-rowID) #add in -rows again is the rows data is commented back in
  
  print("Compressing data along Columns")
  #allows the grouping to be flexible in the case that the dataframe changes size
  totcols <-ncol(test)
  remainder <- merge-(totcols %%merge)
  numgroups <-(totcols +remainder)/merge
  
  
  #aggregate again transposing the data frame and aggregating by the smart meters
  test <- t(test) %>% data.frame %>% 
    mutate(rowID =rep(1:numgroups,each=merge)[1:totcols]) %>%
    group_by(rowID) %>% summarise_each(funs(mean)) %>%select(-rowID) %>%
    t %>% data.frame %>% mutate( rowID = 1:nrow(.)) 
  
  #gather the data for the plot
  print("Reformatting into long form for ggplot")
  #it's importnt to remove the X and convert to integer otherwise the order is messed up
  test%<>% gather(.,key = "columnID", value =Percentvalid, -rowID )%>%
    mutate(columnID = sub("X","", columnID) %>% as.integer)
  
  #Rows refer to grouped time periods
  #columns refer to groups smart meters
  
  print("Creating Plot")
  
  ggplot(test, aes(x=rowID, y= columnID, fill = Percentvalid )) +
    geom_raster() + 
    scale_fill_gradient2(low="blue",mid = "white" ,high = "red", midpoint = mid,
                         name=legend)+
    theme_minimal() #+ 
  #theme(axis.text.x  = element_blank(), axis.text.y = element_blank()) 
  
  
  
}