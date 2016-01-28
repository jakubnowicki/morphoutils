#' Shape proportions
#'
#' Calculate shape proportions. Shape axes shoud be parallel to x-y coordinate axes.
#' @param coords landmark array
#' @param sag.tr proportions direction (default saggital/transversal)
#' @export

shape.proportions <- function(coords, sag.tr = TRUE) {
    n.spec <- dim(coords)[3]
    proportions <- NULL
    for (i in 1:n.spec) {
        tr <- abs(max(coords[,1,i]) - min(coords[,1,i]))
        sag <- abs(max(coords[,2,i]) - min(coords[,2,i]))
        ifelse(sag.tr == TRUE, tmp <- sag/tr,tmp <- tr/sag)
        proportions <- c(proportions,tmp)
    }
    return(proportions)
}