#' Calculate mean shape for size classes
#' 
#' Calculates ean shapes for size classes.
#' 
#' @param data landmark array
#' @param Csize size vector
#' @param breaks number of intervals or specified cutpoints
#' @param simplify does output should be a data frame? deafult = TRUE
#' @import geomorph
#' @export

get.mean.shapes <- function(data,Csize, breaks, simplify = TRUE) {
    Csize <- as.data.frame(Csize)
    Csize$intervals <- cut(Csize$Csize, breaks = breaks)
    splited <- split(x = Csize, f = Csize$intervals, drop = TRUE)
    output <- list()
    for (i in 1:length(splited)) {
        specimens <- match(dimnames(splited[[i]])[[1]],table = dimnames(data)[[3]])
        tmp <- data[,,specimens]
        m.shape <- geomorph::mshape(tmp)
        output <- c(output,list(m.shape))
    }
    names(output) <- levels(factor(Csize$intervals))
    if (simplify) {
        output <- do.call(rbind.data.frame, output)
        output$interval <- gl(length(levels(factor(Csize$intervals))),dim(data)[1],labels = levels(factor(Csize$intervals)))
    }
    return(output)
}