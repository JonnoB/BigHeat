
bigheat <-function(df,mergey = 5, mergex = 5, mid=0.5, legend="Aggregated score"){
  #Produces a an aggregated heat map
  #df = A dataframe
  #merge = the number of cell in the x and y direction to merge
  #mid = The mid point of the heatmap scale, for corellation this would be 0.5
  #Requires dplyr and ggplot
  
  df <- CompressDf(df, mergey, mergex)
  
  #gather the data for the plot
  print("Reformatting into long form for ggplot")
  #it's importnt to remove the X and convert to integer otherwise the order is messed up
  df %<>% gather(.,key = "columnID", value =Percentvalid, -rowID )%>%
    mutate(columnID = sub("X","", columnID) %>% as.integer)
  
  #Rows refer to grouped time periods
  #columns refer to groups smart meters
  
  print("Creating Plot")
  
  ggplot(df, aes(x=rowID, y= columnID, fill = Percentvalid )) +
    geom_raster() + 
    scale_fill_gradient2(low="blue",mid = "white" ,high = "red", midpoint = mid,
                         name=legend)+
    theme_minimal() #+ 
  #theme(axis.text.x  = element_blank(), axis.text.y = element_blank()) 
  
  
  
}