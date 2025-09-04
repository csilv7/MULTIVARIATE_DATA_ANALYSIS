## Manova two way
#Exploratoria
summary(Barras)
Barras$Lubrificante = factor(Barras$Lubrificante,label = c("B1","B2","B3","B4"), levels = c(1,2,3,4))
Barras$Velocidade = factor(Barras$Velocidade,label = c("A1","A2"), levels = c(1,2))
#Diagramas de dispersao com grupos
pairs(Barras[1:2], pch = 19, col = Barras$Lubrificante)
pairs(Barras[1:2], pch = 19, col = Barras$Velocidade)
library(ggplot2)
ggplot(Barras, aes(x=X1, y=X2, group=Lubrificante, color=Lubrificante)) + geom_point() 
#Unindo as variaveis dependentes
Y=cbind(Barras[,1],Barras[,2])
#Teste se Y eh matriz
is.matrix(Y)
#Se a resposta for FALSE, transforme
Y=as.matrix(Y)
#Manova 2 fatores #Criando o modelo Manova 2 way
mF_VL=manova(Y~Barras$Velocidade+Barras$Lubrificante+Barras$Velocidade*Barras$Lubrificante)
saida.mF1.VL=summary.manova(mF_VL, test = "Wilks")
saida.mF1.VL
saida.mF2.VL=summary.manova(mF_VL, test = "Pillai")
saida.mF2.VL
saida.mF3.VL=summary.manova(mF_VL, test = "Hotelling-Lawley")
saida.mF3.VL
saida.mF4.VL=summary.manova(mF_VL, test = "Roy")
saida.mF4.VL
#Comparacoes multiplas com estimativas das medias marginais (pacote emmeans)
options(scipen = 999)
library(emmeans)
emmeans_test(Barras, X1 ~ Velocidade, p.adjust.method = "sidak")
emmeans_test(Barras, X2 ~ Velocidade, p.adjust.method = "sidak")
emmeans_test(Barras, X1 ~ Lubrificante, p.adjust.method = "sidak")
emmeans_test(Barras, X2 ~ Lubrificante, p.adjust.method = "sidak")
#Comparacoes multiplas com pos-hoc
TukeyHSD(x = aov(X1 ~ Velocidade, data=Barras), "Velocidade", conf.level = 0.95)
TukeyHSD(x = aov(X2 ~ Velocidade, data=Barras), "Velocidade", conf.level = 0.95)
TukeyHSD(x = aov(X1 ~ Lubrificante, data=Barras), "Lubrificante", conf.level = 0.95)
TukeyHSD(x = aov(X2 ~ Lubrificante, data=Barras), "Lubrificante", conf.level = 0.95)
#Verificacao de pressupostos
#Verificacao da homogeneidade das matrizes de covariancias 
## Se essa suposicao for violada e n iguais por grupo: Pillai e Hotelling sao confiaveis
## Caso os n sejam diferentes, uma opcao eh usar uma MANOVA robusta
#Teste Box-M - Pacote rstatix
box_m(Barras[,c("X1", "X2")], Barras$Velocidade)
box_m(Barras[,c("X1", "X2")], Barras$Lubrificante)
#Verificacao da normalidade MULTIVARIADA - por grupo:
# Teste de Henze-Zirkler - pacote MVN:
X = cbind(Barras[,1],Barras[,2],Barras[,4])
library(MVN)
mvn(data=X, subset = "Velocidade", mvnTest = "hz")
# Verificacao da presenca de outliers MULTIVARIADOS
## Pela distancia de Mahalanobis (outlier = p<0,001)
Barras %>%  group_by(Velocidade) %>% 
  doo(~mahalanobis_distance(.)) %>% 
  filter(is.outlier == TRUE)
#nao detectou outliers