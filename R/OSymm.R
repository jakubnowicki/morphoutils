#' Symmetrize landmarks
#' 
#' Symmetrize and estimate missing landmarks in bilateral set.
#' @param X Landmark set
#' @param midline Midline landmarks
#' @param left Leftside landmark
#' @param right Righside landmarks
#' @author Annat Haber
#' @export


OSymm <- function(X, midline, right, left) {
  ncl <- ncol(X)
  Xr <- cbind(X[,-ncl], -X[,ncl])
  Xow <- Xo <- rbind(X[c(midline, right, left),])
  Xrw <- Xr <- rbind(Xr[c(midline, left, right),])
  rownames(Xrw) <- rownames(Xr) <- rownames(X)
  Xo[which(is.na(Xr))] <- NA
  Xr[which(is.na(Xo))] <- NA
  mo <- matrix(apply(Xo, 2, mean, na.rm=TRUE), byrow=TRUE, nr=nrow(Xow), nc=ncol(Xow))
  mr <- matrix(apply(Xr, 2, mean, na.rm=TRUE), byrow=TRUE, nr=nrow(Xrw), nc=ncol(Xrw))
  Xrwc <- Xrw-mr
  SVD <- svd(t(na.omit(Xr-mr)) %*% na.omit(Xo-mo))
  L <- diag(SVD$d)
  S <- ifelse(L<0, -1, L)
  S <- ifelse(L>0, 1, L)
  RM <- SVD$v %*% S %*% t(SVD$u)
  Xrot <- (Xow-mo) %*% RM                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
  SC <- apply(array(c(Xrwc,Xrot), dim=c(nrow(Xrot),ncol(Xrot),2), dimnames=list(rownames(Xrot),colnames(Xrot))), 1:2, mean, na.rm=TRUE)
  Xrot[which(is.na(Xrot))] <- Xrwc[which(is.na(Xrot))]
  Xrwc[which(is.na(Xrwc))] <- Xrot[which(is.na(Xrwc))]
  list(rec.orig=Xrot, symmconf=SC, rec.ref=Xrwc)}