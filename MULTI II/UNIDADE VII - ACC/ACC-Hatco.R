library (haven)
#Importar banco HATCO
hatco = HATCO
hatco
#Devemos criar dois grupos de variaveis
# x - variaveis independentes:
## x1 - velocidade de entrega;
## x2 - nivel de preco;
## x3 - flexibilidade de preco;
## x4 - imagem do fabricante;
## x5 - servico geral;
## x6 - imagem da equipe de vendas;
## x7 - qualidade do produto.
X <- hatco[,2:8]
X
#variaveis dependentes
## x9  - nivel de uso;
## x10 - nivel de satisfacao. 
Y <- hatco[,10:11]
Y
#----------------------------------------------
##       Instalando pacotes  
#----------------------------------------------
# Instalando os pacotes necessario para a 
##  analise de correlacao canonica
install.packages("CCA")
# Instalando o pacote para teste de significancia das 
## cargas canonicas.
install.packages("CCP")
#-----------------------------------------------
library(CCA)
library(CCP)
#----------------------------------------------------
# Analisando a matriz de correlacao entre X e Y
# Podemos ver ambos os vetores X e Y quanto a correlacao
## dentro e entre os grupos
# A funcao utilizada serao "matcor()" e depois usamos 
## img.matcor() para representar graficamente
# Calculando a matriz e salvando em "correl"
correl <- matcor(X, Y)
correl
#Exibindo a matriz de correlacao
img.matcor(correl, type = 2)
#Interpretacao: temos tres diagramas. No primeiro temos
## a correlacao dentro do grupo de variaveis do grupo X;
## no segundo temos a correlacao entre as variaveis do 
## grupo Y e terceiro temos a correlacao entre as variaveis
## do grupo Y (na linha) e X (na coluna).
#-----------------------------------------------------
# Para obtermos a correlacao canonica devemos usar a funcao
## cc() do pacote CCA
# cc(X, Y)
## X - matriz numerica (n * p), contendo as coodenadas X
## Y - matriz numerica (n * q), contendo as coodenadas Y
ccyx <- cc(X,Y)
#Vizualizando os objetos salvos em ccyx
names(ccyx)
#Para obtermos a correlacao canonica, salvando no objeto v.ca
v.ca <-ccyx$cor
#Imprimindo na tela a correlaco canonica entre Uk e Vk
print(v.ca)
# [1] 0.9369129 0.5100225
# Cor(U1,V1) = 0.9369129
# Cor(U2,V2) = 0.5100225
#A correlacao canonica pode ser representada pelo grafico:
barplot(ccyx$cor, xlab = "Dimensao",
        ylab = "Correlacao canonica", names.arg = 1:2, ylim = c(0,1))

#-------------------------------------------------------
# Testes multivariados de significancia para ambas 
##  funcoes canonicas
rho <- ccyx$cor
## Defina o numero de observacoes "n",
## o numero de variaveis no primeiro conjunto de dados "p"
## e o numero de variaveis no segundo conjunto "q".

n <- dim(hatco)[1]
p <- dim(X)[2]
q <- dim(Y)[2]

#Lambda de Wilks
p.asym(rho, n, p, q, tstat = "Wilks")
#Tra?o de Pillai
p.asym(rho, n, p, q, tstat = "Pillai")
#Tra?o de Hoteling
p.asym(rho, n, p, q, tstat = "Hotelling")
#Tra?o de gcr de Roy
p.asym(rho, n, p, q, tstat = "Roy")

#---------------------------------------------------------
#Proporcao da variancia total explicada

## Precisaremos das cargas canonicas
# Usamos a funcao 'comput'
loadings<- comput(X,Y,ccyx)
# em que 'X' sao o conj. de variaveis independentes;
#        'Y' o conj. de variaveis dependentes;
#        'ccyx'  sao os resultados obtidos anteriormente 
#por meio da funcao 'cc()'
#Exibindo as cargas canonicas:
print(loadings[3:6])
# O comando utiliza a funcao 'print()' para imprimir os valores
## armazenados do terceiro ao sexto item da lista salva no 
## objetivo loadings acima. 

#PVTE Uk: independentes
pvte.u <-(colSums((loadings$corr.X.xscores)^2))/(dim(X)[2])*100

#PVTE Vk: dependentes
pvte.v <-(colSums((loadings$corr.Y.yscores)^2))/(dim(Y)[2])*100

print(pvte.u)
#PVTE U1 = 27.61225 => primeira variavel canonica U1 explica 27,61% da variancia
#                      original do grupo de variaveis independentes
#PVTE U2 = 8.22767 => segunda variavel canonica U2 explica 8,22% da variancia
#                      original do grupo de variaveis independentes
print(pvte.v)
#PVTE V1 = 85.46998 => primeira variavel canonica V1 explica 85,47% da variancia
#                      original do grupo de variaveis independentes
#PVTE V2 = 14.53002 => segunda variavel canonica V2 explica 14,53% da variancia
#                      original do grupo de variaveis independentes

#----------------------------------------------------------
#Indice de redundancia (IR)
# Sintetiza a PVTE e o R? canonico em um unico indicador
## R2 canonico: indica o quanto da variacia da variavel canonica dependente eh explicada pela variavel canonica 
## independente. eh  o quadrado da correlacao canonica.

# Podemos obter o R2 canonico
r2.c <-ccyx$cor^2
#Vizualizando:
print(r2.c)
#R2_1 = 0,8778 e R2_2 = 0,2601, ou seja, 87,78% da variancia da primeira variavel
#canonica dependente V1 eh explicada pela primeira variavel canonica independente U1. 
#Para a segunda variavel canonica dependente V2, temos que a segunda variavel 
# independente U2 explica 26,01% de sua variancia

#Calculando IR para as variaveis canonicas Uk
ir.x <-(colSums((loadings$corr.X.xscores)^2))/(dim(X)[2])*(ccyx$cor^2)
#Calculando IR para as variaveis canonicas Vk
ir.y <-(colSums((loadings$corr.Y.yscores)^2))/(dim(Y)[2])*(ccyx$cor^2)

#Vizualizando IR para Uk e Vk, respectivamente
print(ir.x)
print(ir.y)


#---------------------------------------------------------
# Coeficientes padronizados (pesos canonicos)
#A magnitude dos pesos canônicos (coeficientes padronizados) representa 
## a contribuicao relativa de cada variavel para com a respectiva variável 
## estatitica latente (variavel canonica).


## para as variaveis dependentes;
sy <- diag(sqrt(diag(cov(Y))))
sy %*% ccyx$ycoef

# Coeficientes padronizados (pesos canonicos) 
## para as variaveis independentes;
sx <- diag(sqrt(diag(cov(X))))
sx %*% ccyx$xcoef

#--------------------------------------------------------
#Cargas canonicas
# Correlacao entre as variaveis independentes e suas variaveis canonicas
print(ccyx$scores$corr.X.xscores)
# Correlacao entre as variaveis dependentes e suas variaveis canonicas
print(ccyx$scores$corr.Y.yscores)

#Instalando o pacote
#Execute APENAS UMA VEZ!
install.packages("knitr")
#Carregando o pacote. Voce deve executar toda vez que reiniciar o R
library(knitr)
#Obtendo as cargas canonicas para a variavel estatistica independente
kable(ccyx$scores$corr.X.xscores, 
      col.names = c("Carga canonica U1","Carga canonica U2"))
#Obtendo as cargas canonicas para a variavel estatistica dependente
kable(ccyx$scores$corr.Y.yscores, 
      col.names = c("Carga canonica V1","Carga canonica V2"))

#------------------------------------------------------
#Cargas canonicas cruzadas
# Correlacao entre as variaveis independentes 
# e variaveis estatisticas canonicas dependentes
ccyx$scores$corr.X.yscores
# Correlacao entre as variaveis dependentes 
# e variaveis estatisticas canonicas independentes
ccyx$scores$corr.Y.xscores






ccyx[3:4]



ccyx[3:6]

library(CCA)
plt.cc(ccyx, var.label = TRUE)
detach(package:CCA)

install.packages("vegan")
library(vegan)
cc3 <- cca(X, Y)
plot(cc3, scaling = 1)



