# dzieli tabelę z krzywymi na poszczególne krzywe
#' Convert semilandmark matrix into list
#' 
#' Function converts semilandmark matrix into list form, each curve in the separate element of the list.
#' @param curves Semilandmark matrix
#' @param break.points First rows of the curves
#' @export
#' @examples matrix.to.list()

matrix.to.list <- function(curves,break.points) {
  splited.curves <- list()
  for (i in 1:length(break.points)) {
    ifelse(i==length(break.points),yes = j<-nrow(curves),no = j<-(break.points[i+1]-1))
    splited.curves[[i]] <- unique(as.vector(curves[(break.points[i]:j),]))
  }
  return(splited.curves)
}