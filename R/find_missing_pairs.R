#' Find missing pairs in array
#' 
#' Find missing pairs of landmarks in array.
#' 
#' @param data landmark array
#' @param right right side landmarks
#' @param left left side landmarks
#' @export

find.missing.pairs <- function(data,right,left) {
    n.specimens <- dim(data)[3]
    missing <- NULL
    for (i in 1:n.specimens) {
        tmp <- missing.pairs(data[,,i],right, left)
        missing <- c(missing,tmp)
    }
    missing <- unique(missing)
    return(missing)
}