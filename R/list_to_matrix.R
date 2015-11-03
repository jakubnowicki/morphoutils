#' Converts a list of semilandmarks into matrix
#' 
#' Function converts a list of semilandmarks into a matrix that can be used in geomorph::gpagen.
#' @param curves.list List of semilandmark vectors
#' @export
#' @examples list.to.matrix()

list.to.matrix <- function(curves.list) {
  binded.curves <- NULL
  n.curves <- length(curves.list)
  for (i in 1:n.curves) {
    tmp <- vector.to.matrix(single.curve = curves.list[[i]])
    binded.curves <- rbind(binded.curves,tmp)
  }
  return(binded.curves)
}