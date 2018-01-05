#' Deform landmark data
#' 
#' Deforms landmark data using strain matrix.
#' 
#' @param data Landmark data (single specimens matrix)
#' @param strain.matrix Strain matrix
#' @export

deformacja <- function(data,strain.matrix, angle = NULL) {
    if (!is.null(angle)) {
        angle.matrix <- matrix(data = c(cos(angle), sin(angle),-sin(angle),cos(angle)), nrow = 2,ncol = 2)
        wynik <- angle.matrix %*% t(data)
        wynik <- t(wynik)
    } else {
        wynik <- data
    }
  wynik <- strain.matrix %*% t(wynik)
  return(t(wynik))
}