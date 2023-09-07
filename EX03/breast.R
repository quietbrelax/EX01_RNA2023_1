install.packages("mlbench")
library("mlbench")
#pega os dados da package mlbench
data("BreastCancer")
data2 <- BreastCancer
#Realiza o tratamento dos dados para remoção de NA
data2 <- data2[complete.cases(data2),]
tag <- array(data = 1, dim = 683)
classe <- data2[11]
string <- "benign"

