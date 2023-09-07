yadaline <- function(xvec, w, par){
  if(par == 1){
    xvec<-cbind(1, xvec)}
  
u<-xvec %*% w
y<--sign(u)
if(y == -1){
  y <- 0
}
return((as.matrix(y)))
}