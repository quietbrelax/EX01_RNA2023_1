rm(list=ls())
library('corpcor')

gauss <- function(x,m,s){
  return(exp(-0.5*(rowSums((x-m)^2/(s^2)))))
}

s1 <- 0.2
s2 <- 0.2
nc <- 100
xc1 <- matrix(rnorm(nc*2),ncol=2)*s1 +t(matrix((c(2,2)),ncol=nc,nrow=2))
xc2 <- matrix(rnorm(nc*2),ncol=2)*s2 +t(matrix((c(2,4)),ncol=nc,nrow=2))
xc1b <- matrix(rnorm(nc*2),ncol=2)*s1 +t(matrix((c(4,4)),ncol=nc,nrow=2))
xc2b <- matrix(rnorm(nc*2),ncol=2)*s2 +t(matrix((c(4,2)),ncol=nc,nrow=2))

xc1 <- rbind(xc1,xc1b)
xc2 <- rbind(xc2,xc2b)

x <- rbind(xc1,xc2)
d1 <- dim(xc1)[1]
d2 <- dim(xc2)[1]
y <- c(rep(-1,d1),rep(1,d2))

# Número de simulações
num_sim <- 10
# Vetores para armazenar as acurácias e os desvios padrão
accuracies <- numeric(num_sim)
std_deviations <- numeric(num_sim)

# Loop de simulações
for (i in 1:num_sim) {
  
  # Separação dos dados em conjuntos de treinamento e teste
  train_idx <- sample(1:nrow(x), nrow(x)*0.9)
  test_idx <- setdiff(1:nrow(x), train_idx)
  
  x_train <- x[train_idx,]
  y_train <- y[train_idx]
  x_test <- x[test_idx,]
  y_test <- y[test_idx]
  
  # Treinamento da RBF
  s <- 0.6
  h1 <- gauss(x_train,c(2,2),s)
  h2 <- gauss(x_train,c(2,4),s)
  h3 <- gauss(x_train,c(4,2),s)
  h4 <- gauss(x_train,c(4,4),s)
  
  H <- cbind(h1,h2,h3,h4,1)
  w <- pseudoinverse(H) %*% y_train
  
  # Teste da RBF
  h1_test <- gauss(x_test,c(2,2),s)
  h2_test <- gauss(x_test,c(2,4),s)
  h3_test <- gauss(x_test,c(4,2),s)
  h4_test <- gauss(x_test,c(4,4),s)
  H_test <- cbind(h1_test,h2_test,h3_test,h4_test,1)
  y_pred <- sign(H_test %*% w)
  
  # Cálculo da acurácia e do desvio padrão
  accuracy <- mean(y_pred == y_test)
  std_deviation <- sd(y_pred == y_test)
  
  # Armazenamento da acurácia e do desvio padrão para essa simulação
  accuracies[i] <- accuracy
  std_deviations[i] <- std_deviation
}
# Cálculo da média e do desvio padrão das acurácias e dos desvios padrão das simulações
mean_accuracy <- mean(accuracies)
mean_std_deviation <- mean(std_deviations)
sd_accuracy <- sd(accuracies)
sd_std_deviation <- sd(std_deviations)

# Impressão dos resultados
cat("Mean accuracy:", mean_accuracy, "\n")
cat("Mean standard deviation:", mean_std_deviation, "\n")
cat("SD accuracy:", sd_accuracy, "\n")
cat("SD standard deviation:", sd_std_deviation, "\n")
# Criando uma grade de pontos para plotar a região de separação
x1 <- seq(min(x[,1]), max(x[,1]), length=50)
x2 <- seq(min(x[,2]), max(x[,2]), length=50)
grid <- expand.grid(x1=x1, x2=x2)
h1_grid <- gauss(grid,c(2,2),s)
h2_grid <- gauss(grid,c(2,4),s)
h3_grid <- gauss(grid,c(4,2),s)
h4_grid <- gauss(grid,c(4,4),s)
H_grid <- cbind(h1_grid, h2_grid, h3_grid, h4_grid, 1)

# Classificando a grade de pontos utilizando o modelo treinado
y_grid <- sign(H_grid %*% w)
y_grid2 <- matrix(y_grid, nrow = 50, ncol = 50)



# Plotando a região de separação
plot(x[y==-1,], pch=20, col="red", xlim=c(min(x[,1]), max(x[,1])), ylim=c(min(x[,2]), max(x[,2])), xlab="X1", ylab="X2")
points(x[y==1,], pch=20, col="blue")
points(grid, pch=46, col=c("red", "blue")[y_grid+1], cex=0.2)

library('plot3D')
contour2D(y_grid2, x1, x2, xlim=c(min(x[,1]), max(x[,1])), ylim=c(min(x[,2]), max(x[,2])), levels = 0, add = TRUE)

persp3D(x1, x2, y_grid2, counter = T, theta = 10, phi = 30, r = 40, d = 20)
