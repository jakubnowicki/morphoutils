#' Find missing pairs in matrix
#' 
#' Find missing pairs of landmarks in matrix.
#' 
#' @param data landmark matrix
#' @param right right side landmarks
#' @param left left side landmarks
#' @export

missing.pairs <- function(data,right,left) {
    n.pairs <- length(right)
    if (n.pairs != length(left)) {
        stop('Can\'t match vecors')
    }
    output <- NULL
    for (i in 1:n.pairs) {
        if (any(is.na(data[right[i],])) & any(is.na(data[left[i],]))) {
            output <- c(output,right[i],left[i])
        }
    }
    return(output)
}