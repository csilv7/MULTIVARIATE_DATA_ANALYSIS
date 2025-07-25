#An�lise Fatorial-Dados AF3-Exemplo da �rea de Floresta
#Matriz de correla��o
R3=cor(AF3)
print (R, digits = 3)
#Gr�fico de correla��o
corrplot(R3)
#Teste de esfericidade de Bartlett - para avaliar se a matriz de correla��o � igual � matriz identidade
cortest.bartlett(R3)
#KMO - para validar a t�cnica e ver a import�ncia das vari�veis.
KMO(R3)
#Autovalores
autovalores = eigen(R3)
autovalores
#scree plot
plot(autovalores$values, xlab = 'Autovalor', ylab = 'Tamanho do autovalor', main = 'Scree Plot', type = 'b', xaxt = 'n')
axis(1, at = seq(1, 10, by = 1))
#An�lise paralela para determinar o n�mero de fatores
fa.parallel(AF3,fm="ml", fa="fa",  main = "Scree Plot da An�lise Paralela", n.iter=500)
#Estima��o das cargas fatoriais pelo m�todo do componente principal 
res.CP3 = principal(R3, nfactors = 3, rotate = 'none', covar = FALSE)
res.CP3
#Estima��o das cargas fatoriais pelo m�todo da m�xima verossimilhan�a
res.MV3 = fa(R3,nfactors=3, rotate="none",fm="ml")
res.MV3
#Estima��o das cargas fatoriais pelo m�todo do fator principal 
res.FP3 = fa(R3,nfactors=3, rotate="none",fm="pa")
res.FP3
#Estima��o das cargas fatoriais pelo m�todo do componente principal com rota��o varimax
res.CP3V = principal(R3, nfactors = 3, rotate = 'varimax', covar = FALSE)
res.CP3V
#Estima��o das cargas fatoriais pelo m�todo da m�xima verossimilhan�a com rota��o
res.MV3V = fa(R3,nfactors=3, rotate="varimax",fm="ml")
res.MV3V
#Estima��o das cargas fatoriais pelo m�todo do fator principal com rota��o
res.FP3V = fa(R3,nfactors=3, rotate="varimax",scores='Bartlett',fm="pa")
res.FP3V

#Estima��o dos escores fatoriais pelo m�todo do componente principal com rota��o varimax
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
