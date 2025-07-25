#Matriz de covari�ncias das vari�veis originais
S1 = cov(Ex1)
#visualizando a matriz S
S1
#Matriz de correla��o das vari�veis originais
R1 = cor(Ex1)
#visualizando a matriz R
R1
#fazendo a ACP
pcex1 <- PCA(Ex1, graph = FALSE)
#visualizando o resultado
summary(pcex1)
#Visualiza��o somente dos autovalores
auto.val <- get_eigenvalue(pcex1)
auto.val
#Extraindo os resultados do acp para vari�veis
var <- get_pca_var(pcex1)
var
#Correla��es entre as vari�veis e os PCs
var$cor
#coordenadas das vari�veis - correla��o da vari�vel com o CP
var$coord
#Contribui��o das vari�veis nos PCs
var$contrib
