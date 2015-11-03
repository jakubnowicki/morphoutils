#' Convert vector of semilandmarks into matrix
#' 
#' Function converts vector of semilandmarks into matrix version
#' @param single.curve Vector to be converted
#' @export

vector.to.matrix <- function(single.curve) {
  n.rows<-length(single.curve)-2
  m<- matrix(0,ncol=3,nrow=n.rows)
  for (i in 1:n.rows) {
    m[i,1]<-single.curve[i]
    m[i,2]<-single.curve[i+1]
    m[i,3]<-single.curve[i+2]
  }
  colnames(m)<-c('before','slide','after')
  return(m)
}