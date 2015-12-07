#' Clear specimens ID's in landmark array
#' 
#' Clears specimens ID's in landmark array from directory and file extension.
#' 
#' @param data landmark array
#' @param split split pattern (directory mark); default = '\\'
#' @export

clear.specimens.names <- function(data,split='\\') {
    n.specimens <- dim(data)[3]
    for (i in 1:n.specimens) {
        dimnames(data)[[3]][i] <- clear.id(dimnames(data)[[3]][i],split = split)
    }
    return(data)
}