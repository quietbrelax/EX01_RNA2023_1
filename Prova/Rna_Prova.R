source("Treinamento_Adaline.R")
library('plot3D')

data <- read.csv("BD_16.csv")

plot(data$V1, data$V2, col = data$V3)


test_indices <- c(sample(which(data$V3 == 1), 10), 
                  sample(which(data$V3 == 2), 10), 
                  sample(which(data$V3 == 3), 10))
test_set <- data[test_indices, ]
train_set <- data[-test_indices, ]


primeiro_teste <- train_set[1:180,]


tags_1 <- -2*(primeiro_teste$V3) + 3



eta <- 0.001
tol <- 0.001
mxepoc <- 100

X <- cbind(primeiro_teste$V1, primeiro_teste$V2)

retlist = treinadaline(X, tags_1, eta, tol, mxepoc, 1)



w_1 <- retlist[[1]]

erro <- retlist[[2]]

#retlist = treinadaline(X, Y, eta, tol, mxepoc, 1)



#w <- retlist[[1]]

#erro <- retlist[[2]]

plot(erro, type = "l")

source("Adaline.R")
Yhat = yadaline(X, w_1, 1)
set_tests_1 <- test_set[1:20,]
seqi <- sort(set_tests_1$V1, decreasing = FALSE)
seqj <- sort(set_tests_1$V2, decreasing = FALSE)

M <- matrix(0, nrow = length(seqi), ncol = length(seqj))
ci <- 0

for (i in seqi) {
  ci <- ci + 1
  cj <- 0
  for (j in seqj) {
    cj <- cj + 1
    M[ci, cj] = yadaline(cbind(i, j), w_1, 1)
  }
}

plot(data$V1, data$V2, col = data$V3)
contour2D(M, seqi, seqj, add = TRUE, levels = 0)

segundo_teste <- train_set[91:270,]


tags_2 <- -2*(segundo_teste$V3) + 5



eta <- 0.001
tol <- 0.001
mxepoc <- 100

K <- cbind(segundo_teste$V1, segundo_teste$V2)

retlist = treinadaline(K, tags_2, eta, tol, mxepoc, 1)



w_2 <- retlist[[1]]

erro <- retlist[[2]]

#retlist = treinadaline(X, Y, eta, tol, mxepoc, 1)



#w <- retlist[[1]]

#erro <- retlist[[2]]

plot(erro, type = "l")

source("Adaline.R")
Yhat = yadaline(K, w_2, 1)
set_tests_2 <- test_set[11:30,]
seqi_2 <- sort(set_tests_2$V1, decreasing = FALSE)
seqj_2 <- sort(set_tests_2$V2, decreasing = FALSE)

N <- matrix(0, nrow = length(seqi_2), ncol = length(seqj_2))
ci <- 0

for (i in seqi_2) {
  ci <- ci + 1
  cj <- 0
  for (j in seqj_2) {
    cj <- cj + 1
    N[ci, cj] = yadaline(cbind(i, j), w_2, 1)
  }
}

plot(data$V1, data$V2, col = data$V3)
contour2D(N, seqi_2, seqj_2, add = TRUE, levels = 0)
contour2D(M, seqi, seqj, add = TRUE, levels = 0)
persp3D(seqi, seqj, M, counter = T, theta = 55, phi = 30, r = 40, d = 20)
persp3D(seqi_2, seqj_2, N, counter = T, theta = 55, phi = 60, r = 40, d = 20)


seqi_temp <- seqi[1:10]
seqj_temp <- seqj[1:10]

seqi_Resul <- sort(c(seqi_temp, seqi_2), decreasing = FALSE)
print(paste(seqi_Resul))
seqj_Resul <- sort(c(seqj_temp, seqj_2), decreasing = FALSE)

Resultado <- matrix(0, nrow = length(seqi_Resul), ncol = length(seqj_Resul))

ci <- 0
for (i in seqi_Resul) {
  ci <- ci + 1
  cj <- 0
  for (j in seqj_Resul) {
    cj <- cj + 1
    Resultado[ci, cj] = yadaline(cbind(i, j), w_1, 1) + yadaline(cbind(i, j), w_2, 1)
  }
}

ResultadoObtido <- matrix(0, nrow = length(seqi_Resul), ncol = length(seqj_Resul))

ci <- 0
for (i in seqi_Resul) {
  ci <- ci + 1
  cj <- 0
  for (j in seqj_Resul) {
    cj <- cj + 1
    if(Resultado[ci, cj] > 1){
      ResultadoObtido[ci, cj] <- 1
    }
    if(-1 < Resultado[ci, cj] && Resultado[ci, cj] < 1){
      ResultadoObtido[ci, cj] <- 2
    }
    if(Resultado[ci, cj] < -1){
      ResultadoObtido[ci, cj] <- 3
    }
  }
}


ResultadoEsperado <- matrix(0, nrow = length(seqi_Resul), ncol = length(seqj_Resul))

ci <- 0
DadosCertos <- 0
classe <- test_set$V3
for (i in seqi_Resul) {
  ci <- ci + 1
  if(ResultadoObtido[ci, ci] == classe[ci]){
    DadosCertos <- DadosCertos + 1
  }
}
accuracy <- DadosCertos/length(classe)
print(round(accuracy, 4))
