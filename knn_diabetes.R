# Instalar o pacote mlbench (se ainda não estiver instalado)
install.packages("mlbench")
install.packages("corrplot")
install.packages("ggcorrplot")
install.packages("readr")
install.packages("dplyr")
install.packages("ggfortify")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("POET")
install.packages("biotools")
install.packages("GGally")
install.packages("caTools")
# Carregar o pacote
library(mlbench)
library(corrplot)
library(ggcorrplot)
library(readr)
library(dplyr)
library(ggfortify)
library(ggplot2)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(POET)
library(biotools)
library(ggplot2)
library(GGally)
library(caTools)
library(class)
# Carregar o dataset
data(PimaIndiansDiabetes)

# Visualizar os primeiros registros do dataset
dados <- PimaIndiansDiabetes
dados_numericos <- dados[1:8]
dados_normalizados <- as.data.frame(scale(dados_numericos)) 
dados_normalizados$diabetes <- dados$diabetes

# divisão de dados
set.seed(1)
divisao <- sample.split(dados_normalizados$diabetes, SplitRatio = 0.75)
#test.index <- 1:1000
knn_treinamento <- subset(dados_normalizados, divisao==TRUE) #treinamento
knn_teste <- subset(dados_normalizados, divisao==FALSE) # teste

# knn
set.seed(123)
previsoes <- knn(train = knn_treinamento[,-9], test= knn_teste[,-9],cl= knn_treinamento[,9],k=1)
head(previsoes)

# taxa de acerto
mean(knn_teste[,9] == previsoes)

# grafico de taxa de erro relativa a quantidade k de vizinhos
previsoes = NULL
perc.erro = NULL

for(i in 1:25){
  set.seed(1)
  previsoes <- knn(train = knn_treinamento[,-9], test= knn_teste[,-9],cl= knn_treinamento[,9],k=i)
  perc.erro[i] =mean(knn_teste[,9] != previsoes)
}

k.values <- 1:25
set.seed(1)
error.df <- data.frame(perc.erro,k.values)

ggplot(error.df,aes(x=k.values,y=perc.erro)) + geom_point()+ geom_line(lty="dotted",color='red')

# menor erro: k = 5
previsoes <- knn(train = knn_treinamento[,-9], test= knn_teste[,-9],cl= knn_treinamento[,9],k=22)
acuracia_knn <- mean(knn_teste[,9] == previsoes)
