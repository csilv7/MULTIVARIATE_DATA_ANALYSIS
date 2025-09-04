zclusterbussab <- as.data.frame(scale(ClusterBussab[,2:3]))
d <- dist(zclusterbussab, method = "euclidean")                               
d
res.hc <- hclust(d, method = "complete" )
res.hc
res.hc2 <- hclust(d, method = "single" )
res.hc3 <- hclust(d, method = "centroid" )
plot(res.hc, cex = 0.6, hang = -1, labels = c("A", "B", "C", "D", "E", "F"), main = "Dendograma Ligação completa", ylab = "Distâncias")
plot(res.hc2, cex = 0.6, hang = -1, labels = c("A", "B", "C", "D", "E", "F"), main = "Dendograma Ligação simples", ylab = "Distâncias")
plot(res.hc3, cex = 0.6, hang = -1, labels = c("A", "B", "C", "D", "E", "F"), main = "Dendograma Ligação centroide", ylab = "Distâncias")
res.hc3$height
# Compute agnes()
library(cluster)
res.agnes <- agnes(zclusterbussab, method = "single")
# Agglomerative coefficient
res.agnes$ac
# Plot the tree using pltree()
pltree(res.agnes, cex = 0.6, hang = -1, main = "Dendrogram of Agnes") 
d2<-cophenetic(res.agnes)
cor(d,d2)
#Agnes 2
res.agnes2 <- agnes(zclusterbussab, method = "complete")
d3<-cophenetic(res.agnes)
cor(d,d3)
d4<-cophenetic(res.agnes2)
cor(d,d4)
