# Instale os pacotes se ainda não tiver
pacotes <- c("MASS", "psych", "car", "klaR", "ggplot2", "candisc", "tidyverse", "caret")
install.packages(pacotes[!pacotes %in% installed.packages()[, "Package"]])

# Carregando
library(MASS)
library(psych)
library(car)
library(klaR)
library(ggplot2)
library(candisc)
library(tidyverse)
library(caret)
# Visualizar estrutura
str(Sitio)
##Padronizar dados
# Função para padronizar apenas as variáveis desejadas (Z-score)
padronizar_dados <- function(dados, colunas_num, col_grupo) {
  dados_padronizados <- dados
  dados_padronizados[, colunas_num] <- scale(dados[, colunas_num])
  # Garante que a variável de grupo fique como fator
  dados_padronizados[[col_grupo]] <- as.factor(dados[[col_grupo]])
  return(dados_padronizados)
}
Sitio2 <- padronizar_dados(dados = Sitio, colunas_num = 1:18, col_grupo = "Classe")

##Análise exploratória
str(Sitio2)

table(Sitio2$Classe)

# Ver estatísticas descritivas por grupo
describeBy(Sitio2[, 1:18], group = Sitio2$Classe)

# Boxplots por grupo
Sitio_long <- Sitio2 %>%
  pivot_longer(cols = 1:18, names_to = "variavel", values_to = "valor")

ggplot(Sitio_long, aes(x = Classe, y = valor, fill = Classe)) +
  geom_boxplot() +
  facet_wrap(~variavel, scales = "free") +
  theme_minimal()

##Testes de pressupostos
#Normalidade multivariada-por grupo
library(MVN)

for (g in unique(Sitio2$Classe)) {
  cat("\nGrupo:", g, "\n")
  grupo_dados <- Sitio2[Sitio2$Classe == g, 1:18]
  print(mvn(grupo_dados, mvnTest = "hz", multivariatePlot = "qq"))
}
#Homogeneidade das matrizes de covariância
# Teste de Box's M
library(biotools)
boxM(Sitio2[, 1:18], Sitio2$Classe)

#Multicolinearidade
# Ver correlação entre variáveis
cor(Sitio2[,1:18])

# VIF (Variance Inflation Factor)
modelo_vif <- lm(as.numeric(Classe) ~ ., data = Sitio2[, c(1:18, which(names(Sitio2) == "Classe"))])
vif(modelo_vif)

##Seleção de variáveis para discriminação
#Stepwise - seleção automática de variáveis
# Separar os dados: preditoras (X) e variável de grupo (y)
X <- Sitio2[, 1:18]               # Variáveis quantitativas padronizadas
y <- Sitio2$Classe                # Variável de grupo (fator)

# Executar seleção stepwise
step_vars <- stepclass(x = X, grouping = y, method = "lda", 
                       improvement = 0.01, direction = "both")

print(step_vars)

##Análise Discriminante Linear
# Usando as variáveis selecionadas (ou todas se quiser)
modelo_lda <- lda(Classe ~ ., data = Sitio2[, c(1:18, which(names(Sitio2) == "Classe"))])
modelo_lda
modelo_lda <- lda(Classe ~ Altura, data = Sitio2[, c(1:18, which(names(Sitio2) == "Classe"))])
modelo_lda
# Projeção discriminante
plot(modelo_lda)

##Avaliação do modelo
# Previsão nos próprios dados (ou use validação cruzada depois)

pred_lda <- predict(modelo_lda, Sitio2)

# Matriz de confusão
confusao <- table(Real = Sitio2$Classe, Previsto = pred_lda$class)
print(confusao)

# Acurácia
acuracia <- mean(Sitio$Classe == pred_lda$class)
cat("Acurácia: ", round(acuracia * 100, 2), "%\n")

# Adiciona ao banco
Sitio$Classe_Prevista <- pred_lda$class

##Validação cruzada com caret
modelo_cv <- train(Classe ~ ., data = Sitio2[, c(1:18, which(names(Sitio2) == "Classe"))],
                   method = "lda", trControl = trainControl(method = "cv", number = 10))
print(modelo_cv)
modelo_cv <- train(Classe ~ ., data = Sitio2[, c(1:18, which(names(Sitio2) == "Classe"))],
                   method = "lda", trControl = trainControl(method = "cv", number = 10))
print(modelo_cv)
