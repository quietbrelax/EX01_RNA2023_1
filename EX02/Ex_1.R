rm(list = ls())
library('corpcor')

N <- 10
x <- runif(n = N, min = -15, max = 10)



yr<-(0.5*x^2+3*x+10) + 10*rnorm(length(x))

xgrid<-seq(-15,10,0.1)
ygrid<-(0.5*xgrid^2+3*xgrid+10)
#Aproximacao de grau dois
H<-cbind(x^2, x,1)
w<-pseudoinverse(H) %*% yr
#Aproximacao

yhat<-H %*% w

Hgrid<-cbind(xgrid^2, xgrid,1)
yhatgrid<-Hgrid %*% w

#plotando a projecao
plot(x,yr, col = 'red', xlim = c(-20,20), ylim = c(-20,100))
par(new=T)
plot(x,yhat, col = 'black', xlim = c(-20,20), ylim = c(-20, 100))

par(new=T)
plot(xgrid,ygrid, col = 'green',type = 'l', xlim = c(-20,20), ylim = c(-20, 100))
par(new=T)
plot(xgrid,yhatgrid, type = 'l',col = 'blue', xlim = c(-20,20), ylim = c(-20, 100))


###################################################
