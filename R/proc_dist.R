#' Calculate Procrustes distance
#' 
#' Calculate Procrustes distance
#' 
#' @param x first shape
#' @param y second shape
#' @export
#' 
proc.dist <- function(x,y) {
    sqrt(sum(apply((x-y)^2, 1, sum)))
}
