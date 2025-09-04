#Matriz de covariâncias das variáveis originais
S1 = cov(Ex1)
#visualizando a matriz S
S1
#Matriz de correlação das variáveis originais
R1 = cor(Ex1)
#visualizando a matriz R
R1
#fazendo a ACP
pcex1 <- PCA(Ex1, graph = FALSE)
#visualizando o resultado
summary(pcex1)
#Visualização somente dos autovalores
auto.val <- get_eigenvalue(pcex1)
auto.val
#Extraindo os resultados do acp para variáveis
var <- get_pca_var(pcex1)
var
#Correlações entre as variáveis e os PCs
var$cor
#coordenadas das variáveis - correlação da variável com o CP
var$coord
#Contribuição das variáveis nos PCs
var$contrib
