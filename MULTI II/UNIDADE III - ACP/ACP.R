#ACP com o pacote FactoMineR e factoextra
##Instalar e ativar os pacotes
##Importar a base de dados CP1
#Matriz de covariancias das variaveis originais
S = cov(DadosCP1)
#visualizando a matriz S
S
#Matriz de correlacao das variaveis originais
R = round(cor(DadosCP1),4)
#visualizando a matriz R
R
#padronizando os dados - padronizacao para variancia 1 e media qualquer
dadoscp=scale(DadosCP1, center = FALSE, scale = apply(DadosCP1, 2, sd, na.rm = TRUE))
#visualizando a matriz padronizada
dadoscp
#fazendo a ACP
pcexemplo <- PCA(dadoscp, graph = FALSE)
#visualizando o resultado
summary(pcexemplo)
#Visualizacao somente dos autovalores
auto.val <- get_eigenvalue(pcexemplo)
auto.val
#Extraindo os resultados do acp para vari?veis
var <- get_pca_var(pcexemplo)
var
#Correlacoes entre as variaveis e os PCs
var$cor
#coordenadas das variaveis - correlacao da variavel com o CP
var$coord
#Contribuicao das variaveis nos PCs
var$contrib
#Quadrado das correlacoes (cos2)- mede a qualidade da representacao das varaveis no mapa de calor
var$cos2
#Graficos
#scree plot
fviz_eig(pcexemplo, addlabels = TRUE, ylim = c(0, 50))
#Gr?fico de cargas fatoriais
fviz_pca_var(pcexemplo, col.var = "blue")
#Grafico de cargas fatoriais inserindo a qualidade das variaveis como cores
fviz_pca_var(pcexemplo, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # Avoid text overlapping
#Grafico de contribuicao das vari?veis no CP1
fviz_contrib(pcexemplo, choice = "var", axes = 1, top = 10)
#Grafico de contribuicao das vari?veis no CP2
fviz_contrib(pcexemplo, choice = "var", axes = 2, top = 10)
#Gr?fico dos escores individuais
fviz_pca_ind(pcexemplo)
#Gr?fico dos escores individuais com qualidade dos indiv?duos nas cores
fviz_pca_ind(pcexemplo, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
#Biplot
fviz_pca_biplot(pcexemplo, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969" # Individuals color
)

