rm(list=ls())
library('plot3D')
library('corpcor')
library('mlbench')



dados <- mlbench.spirals(100,sd = 0.05)
xall <- dados$x
yall <- as.numeric(dados$classes) * 2 - 3
p <-  30
Z <- replicate(p, runif(3,-0.5,0.5))


Xaug <- cbind(replicate(dim(xall)[1],1),xall)

H <- as.matrix(tanh(Xaug %*% Z))



W <-  pseudoinverse(H) %*% yall







seqi <- seq(-2,2,0.05)
seqj <- seq(-2,2,0.05)
M <- matrix(0,nrow=length(seqi),ncol=length(seqj))

ci <- 0
for (i in seqi){
  ci <- ci + 1
  cj <- 0
  for (j in seqj)
  {
    cj <- cj + 1
    xg <- c(1,i,j)
    Hg <- as.matrix(tanh(xg %*% Z))
    
    M[ci,cj] <- sign(Hg %*% W)
    
    
  }
}

plot(xall[which(yall == 1),1],xall[which(yall == 1),2],col = "red", xlim =c(-2,2), ylim = c(-2,2))
par(new = T)
plot(xall[which(yall == -1),1],xall[which(yall == -1),2],col = "blue", xlim =c(-2,2), ylim = c(-2,2))
par(new = T)
contour(seqi,seqj,M, xlim =c(-2,2), ylim = c(-2,2), xlab = '', ylab = '')
