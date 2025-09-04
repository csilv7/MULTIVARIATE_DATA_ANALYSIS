#Analise Fatorial-Dados AF1-adjetivos de colegas
#Matriz de correlacao
R=cor(AF1[,-1])
print(R, digits = 3)
#Autovalores de R
autovalores = eigen(R)
autovalores
#scree plot
plot(autovalores$values, xlab = 'Autovalor', ylab = 'Tamanho do autovalor', main = 'Scree Plot', type = 'b', xaxt = 'n')
axis(1, at = seq(1, 4, by = 1)) 
#Estimacao das cargas - Metodo componente principal
#Computacionalmente, para estimar as cargas fazemos a decomposicao espectral,
#Fatoramos S=CDC', onde C e uma matriz ortogonal dos autovetores
#normalizados de S e D eh uma matriz diagonal, cuja diagonal contem
#os autovalores de S
C = as.matrix(autovalores$vectors[,1:2])
C
D = matrix(0, dim(C)[2], dim(C)[2])
diag(D) = autovalores$values[1:2]
#Cargas fatoriais
cargas = C %*% sqrt(D)
cargas
#Comunalidades
h2 <- rowSums(cargas^2)
h2
#Variancia especifica
u2 <- diag(R) - h2
u2
#Proporcao das variancias total das cargas
prop.cargas <- colSums(cargas^2)
prop.cargas
prop.var <- cbind(prop.cargas[1] / sum(autovalores$values), prop.cargas[2] / sum(autovalores$values))
prop.var
#propor??o da vari?ncia explicada pelos fatores
prop.exp <- cbind(prop.cargas[1] / sum(prop.cargas), prop.cargas[2] / sum(prop.cargas))
prop.exp
#Estimacao das cargas - Metodo fator principal
#Como a matriz de correlacao eh singular, sera substuido na diagonal principal
#o maior valor de cada linha de R. A matriz sera chamada AF2
R2 = as.matrix(AF2)
R2
#Autovalores de R2
autovalores2 = eigen(R2)
autovalores2
#Estimacao das cargas
C2 = as.matrix(autovalores2$vectors[,1:2])
D2 = matrix(0, dim(C2)[2], dim(C2)[2])
diag(D2) = autovalores2$values[1:2]
#Cargas fatoriais
cargas2 = C2 %*% sqrt(D2)
cargas2

#Estimacao com o pacote psych
library(psych)
#Metodo componente principal
af.res = principal(AF1[,-1], nfactors = 2, rotate = 'none', cor = TRUE)
af.res
res.CP = principal(R, nfactors = 2, rotate = 'none', scores=TRUE, covar = FALSE)
res.CP
res.CP.rot = principal(R, nfactors = 2, rotate = 'varimax', scores=TRUE, covar = FALSE)
res.CP.rot
scorcp = factor.scores(AF1[,-1], f=res.CP.rot)
scorcp
res.CP.rot$
#estimacao maxima verossimilhanca
res.versim <- fa(R,rotate="none",fm="ml")
