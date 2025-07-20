##Analise Fatorial com o pacote FactoMineR e factoextra
# Instalar pacotes necessários (se ainda não tiver)
install.packages(c("readxl", "FactoMineR", "factoextra", "psych", "GPArotation", "corrplot"))
# Carregar pacotes
library(readxl)
library(FactoMineR)
library(factoextra)
library(psych)
library(GPArotation)
library(corrplot)
# Importar os dados do Excel
dados <- read_excel("HATCO.xlsx", sheet = "HATCO")
# Selecionar apenas as variáveis numéricas para AFE
dados_fat <- HATCO[, c("RapidezEntrega", "NivelPreco", "FlexibPreco", 
                           "ImagemFabricante", "Servico", "ImagemForcaVendas", 
                           "QualidadeProduto")]
#dados_fat2 <- HATCO[, c("RapidezEntrega", "ImagemFabricante", "Servico", 
                       "QualidadeProduto")]
# Visualizar estrutura
str(dados_fat)
# Matriz de correlação
cor_matrix <- cor(dados_fat)
corrplot(cor_matrix, method = "color", tl.cex = 0.8)
##Adequação da Técnica
# Teste de esfericidade de Bartlett
#Normalidade multivariada
library(MVN)
mvn(data=dados_fat, mvnTest = "royston")
cortest.bartlett(cor_matrix, n = nrow(dados_fat))
# Medida KMO
KMO(dados_fat)
# Scree plot e paralel analysis
fa.parallel(dados_fat, fa = "fa", n.iter = 100, show.legend = FALSE)
# Suponha que o número ideal de fatores seja 3
# AFE sem rotação 
resultado_fa1 <- fa(dados_fat, nfactors = 3, rotate = "none", fm = "ml")
print(resultado_fa1$loadings, cutoff = 0.3)
# AFE com rotação varimax
resultado_fa2 <- fa(dados_fat, nfactors = 3, rotate = "varimax", fm = "ml")
print(resultado_fa2$loadings, cutoff = 0.3)
# Visualização com PCA como proxy
fviz_pca_ind(PCA(dados_fat, graph = FALSE), 
             repel = TRUE, title = "Indivíduos - PCA (proxy da FA)")
fviz_pca_var(PCA(dados_fat, graph = FALSE), 
             col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "Variáveis - PCA (proxy da FA)")
