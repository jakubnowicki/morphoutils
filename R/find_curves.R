#' Find curves
#' 
#' Finds beginings of curves in semilandmark curves matrix used in geomorph::gpagen
#' @param curves Matrix of curves
#' @export
#' @examples find.curves()

find.curves <- function(curves) {
  break.points <- 1
  for (i in 2:nrow(curves)) {
    if (curves[i,2]!=curves[i-1,3]) {
      break.points <- c(break.points,i)
    }
  }
  return(break.points)
}