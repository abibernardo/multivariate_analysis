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

dados <- read_csv(url("https://raw.githubusercontent.com/abibernardo/multivariate_analysis/main/data.csv"))
ggparcoord(dados, 
           columns = 3:32,
           groupColumn = 'diagnosis',
           scale = "uniminmax")
dados_pca <- dados[, 3:32] # variáveis numéricas


# normalizando valores para fazer comparação de vetores de média
dados_normalizados <- as.data.frame(scale(dados_pca))  
dados_normalizados$diagnosis <- dados$diagnosis
dados_medias <- dados_normalizados %>% group_by(diagnosis) %>% summarise_if(is.numeric, mean) #vetor de médias
dados_long <- dados_medias %>% 
  gather(key = "variavel", value = "media", -diagnosis)
n <- nrow(dados_long) / 2  
dados_long_1 <- dados_long[1:n, ]  # Primeira metade das variáveis
dados_long_2 <- dados_long[(n+1):nrow(dados_long), ]  # Segunda metade das variáveis


# Gráfico 1: Primeira metade das variáveis
grafico1 <- ggplot(dados_long_1, aes(x = variavel, y = media, fill = diagnosis)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Médias das Variáveis Numéricas por Diagnosis (Parte 1)",
       x = "Variável",
       y = "Média")
print(grafico1)


# Gráfico 2: Segunda metade das variáveis
grafico2 <- ggplot(dados_long_2, aes(x = variavel, y = media, fill = diagnosis)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Médias das Variáveis Numéricas por Diagnosis (Parte 2)",
       x = "Variável",
       y = "Média")
print(grafico2)

cor_matrix <- cor(dados_pca) # há variáveis extremamente correlacionadas
variancia_generalizada <- det(cov(dados_pca)) 
autovetores_cov <- eigen(cov(dados_pca))$vectors #autovetores matriz covariancia
autovetores_corr <- eigen(cor(dados_pca))$vectors #autovetores matriz correlação (melhor para unidades dif)

# Componentes:
componentes_prcomp <- prcomp(dados_pca, scale. = TRUE) #scale = TRUE: matriz de corr

# screeplot dos pca
screeplot(componentes_prcomp, type = "lines")
biplot(componentes_prcomp) #não está muito legivel, vou omitir no trabalho

pca_cor <- PCA(dados_pca, ncp = ncol(dados_pca))
fviz_pca_var(pca_cor, repel = TRUE) 

autovalores <- get_eigenvalue(pca_cor)[,"eigenvalue"]
# Critério de Kaisser para verificar quantidade de componentes:
componentes_prcomp$sdev^2 > mean(componentes_prcomp$sdev^2) 

# variância explicada pelos 6 primeiros
variancia_explicada <- componentes_prcomp$sdev^2 / sum(componentes_prcomp$sdev^2)
variancia_explicada_total <- sum(variancia_explicada[1:6])
variancia_explicada_total * 100
# cargas dos componentes principais:
componentes_prcomp$rotation

#componentes que passaram no Critério de Kaisser
componentes_selecionados <- as.data.frame(componentes_prcomp$x[, 1:6]) 
componentes_selecionados$diagnosis <- dados$diagnosis 

# agora, passando pra manova

#O teste M de Box de igualdade das matrizes de cov (rejeitou H0)
biotools::boxM(componentes_selecionados[,-7], componentes_selecionados$diagnosis)

manova_test <- manova(cbind(PC1, PC2, PC3, PC4, PC5, PC6) ~ diagnosis, data = componentes_selecionados)
summary(manova_test, test = 'Wilks')
