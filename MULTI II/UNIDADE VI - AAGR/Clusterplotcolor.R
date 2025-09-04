#Análise de agrupamentos com pacotes cluster e factorextra
#Dados: poluentes
##Importar os dados
##Ativar os pacotes
library(FactoMineR)
library(factoextra)
library(cluster)
#Organizar banco de dados
Poluentes = as.data.frame(Poluentes)
rownames(Poluentes) <- Poluentes$Símbolo
Poluentes = Poluentes[,-6]
is.data.frame(Poluentes)
View(Poluentes)
summary(Poluentes)
## Análise exloratória
summary(Poluentes2$Combustivel)
#Se a categórica não for fator, transformar antes de fazer o plot
Poluentes2$Combustivel = as.factor(Poluentes2$Combustivel)
is.factor(Poluentes2$Combustivel)
#Diagrama de dispersão com grupo
plot(Poluentes2[,4:8], col = Poluentes2$Combustivel, pch = 19)
Combustivel <- as.integer(Poluentes2$Combustivel)
plot3d(Poluentes2[,4:8],col=Combustivel, type = "s",radius=0.2, main="Poluentes")
##Cluster hierárquico##
#Verificação do número ótimo de clusters
fviz_nbclust(Poluentes2[,4:8], hcut ,method = "gap_stat")
fviz_nbclust(Poluentes2[,4:8], hcut, method = "silhouette")
fviz_nbclust(Poluentes2[,4:8], hcut, method = "wss")
#Cluster 
#Padronizar as variáveis
Poluentes3 = scale(Poluentes2[,4:8])
rownames(Poluentes3)= rownames(Poluentes)
#Cluster com distância euclidiana e método de ligação completa
res.dist <- dist(Poluentes3, method = "euclidean")
res.hc <- hclust(d = res.dist, method = "complete")
#Dendrograma
fviz_dend(res.hc, cex = 0.5)
complete <- hclust(dist(Poluentes, method = "euclidean"), method = "complete")
completeCluster <- cutree(complete, k=3)
Poluentes$completeCluster <- completeCluster
Poluentes$completeCluster <- factor(Poluentes$completeCluster, levels = c(1,2,3))
fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)
#Kmeans
set.seed(123)
km.res <- kmeans(Poluentes, 3, nstart = 25)
fviz_cluster(km.res, data = Poluentes,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)
View(Poluentes)
