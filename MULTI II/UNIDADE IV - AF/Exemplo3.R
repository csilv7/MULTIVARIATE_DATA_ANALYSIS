#Análise Fatorial-Dados AF3-Exemplo da área de Floresta
#Matriz de correlação
R3=cor(AF3)
print (R, digits = 3)
#Gráfico de correlação
corrplot(R3)
#Teste de esfericidade de Bartlett - para avaliar se a matriz de correlação é igual à matriz identidade
cortest.bartlett(R3)
#KMO - para validar a técnica e ver a importância das variáveis.
KMO(R3)
#Autovalores
autovalores = eigen(R3)
autovalores
#scree plot
plot(autovalores$values, xlab = 'Autovalor', ylab = 'Tamanho do autovalor', main = 'Scree Plot', type = 'b', xaxt = 'n')
axis(1, at = seq(1, 10, by = 1))
#Análise paralela para determinar o número de fatores
fa.parallel(AF3,fm="ml", fa="fa",  main = "Scree Plot da Análise Paralela", n.iter=500)
#Estimação das cargas fatoriais pelo método do componente principal 
res.CP3 = principal(R3, nfactors = 3, rotate = 'none', covar = FALSE)
res.CP3
#Estimação das cargas fatoriais pelo método da máxima verossimilhança
res.MV3 = fa(R3,nfactors=3, rotate="none",fm="ml")
res.MV3
#Estimação das cargas fatoriais pelo método do fator principal 
res.FP3 = fa(R3,nfactors=3, rotate="none",fm="pa")
res.FP3
#Estimação das cargas fatoriais pelo método do componente principal com rotação varimax
res.CP3V = principal(R3, nfactors = 3, rotate = 'varimax', covar = FALSE)
res.CP3V
#Estimação das cargas fatoriais pelo método da máxima verossimilhança com rotação
res.MV3V = fa(R3,nfactors=3, rotate="varimax",fm="ml")
res.MV3V
#Estimação das cargas fatoriais pelo método do fator principal com rotação
res.FP3V = fa(R3,nfactors=3, rotate="varimax",scores='Bartlett',fm="pa")
res.FP3V

#Estimação dos escores fatoriais pelo método do componente principal com rotação varimax
res.FP3VS = factor.scores(AF3, f='res.FP3V', method = c('Bartlett') )                                     

res.CP3V = principal(R3, nfactors = 3, rotate = 'varimax', score=TRUE, covar = FALSE)
head(res.CP3V$scores)
res.CP3V$score
res.CP3V$weights
res.CP3V$values
res.CP3V$rot.mat
res.FP3V$scores
scor1 = factor.scores(AF3, f=res.CP3V)
scor1
is.atomic(res.FP3V)
is.recursive(res.FP3V)
is.recursive(AF3)
