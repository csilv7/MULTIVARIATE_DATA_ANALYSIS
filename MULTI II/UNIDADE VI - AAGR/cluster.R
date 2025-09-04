ExemploKmeans
#Padroniza??o dos dados
zkmeans = scale(ExemploKmeans[-1])
zkmeans
#Matriz de dist?ncias
d = dist(zkmeans, method = "euclidean")
#Cluster com liga??o completa
res.h1 <- hclust(d, method = "complete")
#Dendograma
plot(res.h1, labels = c("A", "AG", "F", "CV", "CN", "P"), cex = 0.6, hang = -1, main = "Dendograma Liga??o Completa", ylab = "Ponto de fus?o")
#K-means 3 grupos
(res.k <- kmeans(zkmeans, centers = 3))
plot(zkmeans, xlab = "Areia",
     ylab = "Argila", pch = 16, col = res.k$cluster, cex = 1.5)
text(zkmeans[ ,1], zkmeans[,2], labels = c("A", "AG", "F", "CV", "CN", "P"),
     pos = 4)

#K-means 2 grupos
(res.k2 <- kmeans(zkmeans, centers = 2))
plot(zkmeans, xlab = "Areia",
     ylab = "Argila", pch = 16, col = res.k2$cluster, cex = 1.5)
text(zkmeans[ ,1], zkmeans[,2], labels = c("A", "AG", "F", "CV", "CN", "P"),
     pos = 4)
zkmeans
#Escolha de grupos
set.seed(123)
library(factoextra)
fviz_nbclust(zkmeans, kmeans, method = "wss", k.max = 4)
fviz_cluster(res.k, data = ExemploKmeans[,-1],
             palette = c("#2E9FDF", "#E7B800","#D7C800" ),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)
