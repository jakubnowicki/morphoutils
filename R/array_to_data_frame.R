#' Convert landamrk array into data frame
#' 
#' Converts landmark array into data frame with additional column with specimens name. Can be used with lattice xyplot
#'  function to plot all specimens.
#'  
#'  @param data landmark array
#'  @param specimens.names optional vector containing specimens names
#'  @export

ar.to.df <- function(data,specimens.names=FALSE) {
  n.specimens <- dim(data)[3]
  output <- NULL
  for (i in 1:n.specimens) {
    tmp <- as.data.frame(data[,,i])
    ifelse(specimens.names!=FALSE,tmp$specimen<-specimens.names[i],tmp$specimen<-i)
    output <- rbind(output,tmp)
  }
  colnames(output)[1:2] <- c('x','y')
  output$specimen <- as.factor(output$specimen)
  return(output)
}