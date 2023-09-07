rm(list=ls())
source("treinadaline.R")
source("yadaline.R")
library('plot3D')

n1 = 200
m11 = 2;
m12 = 2;
s1 = 0.4;

n2 = 200;
m21 = 4;
m22 = 4;
s2 = 0.4;

c11 = rnorm(n1, m11, s1);
c12 = rnorm(n1, m12, s1);
c21 = rnorm(n2, m21, s2);
c22 = rnorm(n2, m22, s2);

Cp <- cbind(c11, c12)
Cn <- cbind(c21, c22)

y1 <- array(1, dim = c(n1, 1));
y2 <- array(-1, dim = c(n2, 1));

Yp = y1;
Yn = y2;

X = rbind(Cp, Cn)
Y = rbind(Yp, Yn)


plot(Cp[,1], Cp[,2], col = 'red', pch = 'o', xlim = c(0, max(Cp[,1],Cn[,1])),ylim = c(0,max(Cp[,1], Cn[,1])))
par(new = T)
plot(Cn[,1], Cn[,2], col = 'blue', pch = '+', xlim = c(0, max(Cp[,1], Cn[,1])), ylim = c(0, max(Cp[,1], Cn[,1])))


eta <- 0.001
tol <- 0.001
mxepoc <- 100

retlist = treinadaline(X, Y, eta, tol, mxepoc, 1)



w <- retlist[[1]]

erro <- retlist[[2]]

source("yadaline.R")
Yhat = yadaline(X, w, 1)


plot(erro, type = "l")

seqi <- seq(0.06, 6, 0.06)
seqj <- seq(0.06, 6, 0.06)

M <- matrix(0, nrow = length(seqi), ncol = length(seqj))
ci <- 0

for (i in seqi) {
  ci <- ci + 1
  cj <- 0
  for (j in seqj) {
    cj <- cj + 1
    M[ci, cj] = yadaline(cbind(i, j), w, 1)
  }
}

plot(Cp[,1], Cp[,2], col = 'red', pch = 'o', xlim = c(0, max(Cp[,1],Cn[,1])),ylim = c(0,max(Cp[,1], Cn[,1])))
par(new = T)
plot(Cn[,1], Cn[,2], col = 'blue', pch = '+', xlim = c(0, max(Cp[,1], Cn[,1])), ylim = c(0, max(Cp[,1], Cn[,1])))
#par(new = T)
x<-seq(0.2,5,0.01)
f <- function(x) (-x + 6)
plot(x, f(x), type = "l")


contour2D(M, seqi, seqj, xlim = c(0, max(Cp[,1], Cn[,1])), ylim = c(0, max(Cp[,1], Cn[,1])), levels = 0, add = TRUE)
##contour2D(M, seqi, seqj, add = TRUE)

persp3D(seqi, seqj, M, counter = T, theta = 55, phi = 30, r = 40, d = 20)

