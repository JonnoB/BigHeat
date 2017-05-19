orderedheat <-function(df, order="both", mergey = 5, mergex = 5, xblocks=10, yblocks=10, 
                       simMat=FALSE, mid=0.5, legend="Aggregated Score"){
  #Produces a an aggregated heat map
  #cluster obejcts and an ordered heatmap
  #df = A datframe
  #order options are both, row,column, none
  #merge = the number of cell in the x and y direction to merge
  #xblocks/yblocks = the amount of blocks a matrix should be broken into for clustering
  #simMat = a Logical value that is TRUE if df is a symmetrical similarity/distance matrix
  #mid = The mid point of the heatmap scale, for corellation this would be 0.5
  
  #Combines Bigheat and createorder to create an orderedn aggregated heatmap
  
    
  #create the order vector for the x and y axis
  if(order=="none"){
    ordering <- list(1:ncol(df), 1:nrow(df))
    names(ordering)<-c("Colorder", "Roworder")
  } else{
    ordering<- createorder(df, order, simMat,xblocks, yblocks)
  }
  
  bigheat(df[ordering$Roworder,ordering$Colorder], mergey, mergex, mid, legend)
  
  
}