#Análise exploratória no R
#Lendo os dados
Caes = read.csv(file = "C:/Users/marin/OneDrive/Documentos/Marinalva/Disco Local/Marinalva/Disciplinas/2020-2/Multi2/AT2021/Unidade 1/caes.csv")
#Vetor de médias amostrais
colMeans(Caes[,2:7])
#Matriz de covariâncias: 
var(Caes[,2:7])
#Matriz de correlações: 
cor(Caes[,2:7])
#Padronização com a função scale
caespad <- as.data.frame(scale(Caes[,2:7]))
#Matriz de distâncias:
dist(Caes[,2:7])
dist(Caes[,2:7],method = "manhattan")
#Pacotes para gráfico de correlação no R: corrplot, corrgram, Ggally, ggplot2
library(corrplot)
M = cor(Caes[,2:7])
corrplot(M, method = "circle")
#Faces de Chernoff
#Instalar Pacote aplpack
library(aplpack)
faces(Caes[,2:7], labels=Caes$Grupo, face.type=0)
#Tem três tipos de faces, o tipo 0 cria só as linhas das faces, o tipo 1 cria faces coloridas e o tipo 3 cria faces de papai noel.
