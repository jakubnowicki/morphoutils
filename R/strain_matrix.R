#' Generate strain matrix
#' 
#' Generates strain matrix
#' @param a a
#' @param theta theta
#' @param type as in Angielczyk and Sheets (2007) = 1, or with -sin(theta) = 2
#' @export

strain.matrix <- function(a, theta, type = 1) {
  wynik<-matrix(0,ncol=2,nrow=2)
  wynik[1,1]<-a*cos(theta)
  if (type == 1) {
      wynik[1,2]<-sin(theta)
  } else {
      wynik[1,2] <- -sin(theta)
  }
  wynik[2,1]<-sin(theta)
  wynik[2,2]<-(1/a)*cos(theta)
  return(wynik)
}
#strain.matrix.comp<-cmpfun(f = strain.matrix)
