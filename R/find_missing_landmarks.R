#' Replace missing landmarks in array
#' 
#' Replaces missing landmarks in array with NA. Based on k-means clustering, seeks for separated group of landmarks (for example you can put all missing landmarks in the corner of the picture). Also handles with tpsdig way of writing missing andmarks (-1, -1). Also can clear specimens IDs.
#' 
#' @param data landmark array
#' @param method clustering method
#' @param clear.names optional bolean for clearing specimens IDs; default = FALSE
#' @param split optional split pattern for clearing IDs (directory mark); default = '\\'
#' @param clust.parameter minimal proportion of missed landmarks in 'clust method; deafult = 0.45
#' @export

find.missing.landmarks <- function(data,method = 'clust',clear.names = FALSE, split = '\\', clust.parameter = 0.45) {
    n.specimens <- dim(data)[3]
    for (i in 1:n.specimens) {
        data[,,i] <- missing.landmarks(data[,,i],method = method, clust.parameter = clust.parameter)
    }
    if (clear.names) {
        data <- clear.specimens.names(data,split = split)
    }
    return(data)
}