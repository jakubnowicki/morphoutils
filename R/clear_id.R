#' Clear specimen ID
#' 
#' Clears specimens ID from directory and file extension.
#' 
#' @param string.to.clean specimens ID
#' @param split split pattern (directory mark); default = '\\'
#' @export

clear.id <- function(string.to.clean,split = '\\') {
    tmp <- strsplit(string.to.clean,split = split,fixed=T)
    id <- last(tmp[[1]])
    id <- gsub(pattern = '.jpg', replacement = '',x = id)
    id <- gsub(pattern = '.jpeg', replacement = '',x = id)
    id <- gsub(pattern = '.png', replacement = '',x = id)
    id <- gsub(pattern = '.tiff', replacement = '',x = id)
    id <- gsub(pattern = '.bmp', replacement = '',x = id)
    return(id)
}