#' Symmetrization of bilateral landmarks dataset
#' 
#' Symmetrize and estimate missing landmarks in a set of specimens.
#' @param dane A set of specimens
#' @param mid Midline landmarks
#' @param left Leftside landmark
#' @param right Righside landmarks
#' @export

symetria.2<-function(dane,mid,left,right) {
  n<-dim(dane)[3]
  for (i in 1:n) {
    tmp<-OSymm(dane[,,i],mid,right,left)
    dane[,,i]<--tmp$symmconf
  }
  return(dane)
}