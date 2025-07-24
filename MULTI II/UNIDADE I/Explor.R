# ------------------------------------------------------
# [1] CONFIGURAÇÕES INICIAIS - ANÁLISE EXPLORATÓRIA NO R
# ------------------------------------------------------

# Pacotes
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(corrplot)

# Definindo o diretório de trabalho
setwd("~/PROJETOS/VS Code/MULTIVARIADA - VS/MULTI II/UNIDADE I")

# -------------------------------------
# [2] CARREGANDO BASE DE DADOS PARA O R
# -------------------------------------

# Definindo caminho dos arquivos
path <- c(
  "Caes.csv",
  "Hemo.xlsx"
)

# Leitura dos arquivos
Caes <- read_csv(path[1])
Hemo <- read_excel(path[2])

# ---------------------
# [3] MEDIDAS DE RESUMO
# ---------------------

# ---------------------------
# [3.1] Antes da Padronização
# ---------------------------

# Vetor de Médias: Caes
colMeans(Caes[, -1])

# Matriz de Variâncias e Covariâncias: Caes
var(Caes[, -1])

# Vetor de Médias: Hemo
colMeans(Hemo[, -1])

# Matriz de Variâncias e Covariâncias: Hemo
var(Hemo[, -1])

# ------------------
# [3.2] Padronização
# ------------------

Caes.scale <- Caes
# Dados Caes
Caes.scale[, -1] <- scale(Caes[, -1])  

Hemo.scale <- Hemo
# Dados Hemo
Hemo.scale[, -1] <- scale(Hemo.scale[, -1])


# ----------------------------
# [3.3] Depois da Padronização
# ----------------------------

# Vetor de Médias: Caes
colMeans(Caes.scale[, -1])

# Matriz de Variâncias e Covariâncias: Caes
var(Caes.scale[, -1])

# Vetor de Médias: Hemo
colMeans(Hemo.scale[, -1])

# Matriz de Variâncias e Covariâncias: Hemo
var(Hemo.scale[, -1])

# ---------------
# [3.4] Distância
# ---------------

# Distância Euclidiana
dist(Caes[, -1], method = "euclidean")
dist(Hemo[, -1], method = "euclidean")

# Distância Manhattan
dist(Caes[, -1], method = "manhattan")
dist(Hemo[, -1], method = "manhattan")

# Distância Minkowski
dist(Caes[, -1], method = "minkowski")
dist(Hemo[, -1], method = "minkowski")

# -------------------------
# [4] ALGUMAS VISUALIZAÇÕES
# -------------------------

# -----------------
# [4.1] DADOS: Caes
# -----------------

# 1º Gráfico: Boxplot
Caes %>%
  pivot_longer(
    cols = X1:X6, names_to = "Variável", values_to = "Medida"
  ) %>%
  ggplot(aes(x = `Variável`, y = Medida, color = `Variável`)) +
  geom_boxplot(show.legend = FALSE) +
  labs(x = "Variável", y = "Medida") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# 2º Gráfico: Histograma
Caes %>%
  pivot_longer(
    cols = X1:X6, names_to = "Variável", values_to = "Medida"
  ) %>%
  ggplot(aes(x = Medida, fill = `Variável`)) +
  geom_histogram() +
  labs(x = "Variável", y = "Frequência") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# 3º Gráfico: Pairplot
ggpairs(
  Caes, columns = c("X1", "X2", "X3", "X4", "X5", "X6")
)

# 4º Gráfico: Corrplot
corrplot(
  cor(Caes[, -1]), method = "color", 
  addCoef.col = "red", tl.col = "black", 
  number.digits = 2, number.cex = 1
)

# -----------------
# [4.2] DADOS: Hemo
# -----------------

# 1º Gráfico: Boxplot
Hemo %>%
  pivot_longer(
    cols = Y1:Y6, names_to = "Variável", values_to = "Medida"
  ) %>%
  ggplot(aes(x = `Variável`, y = Medida, color = `Variável`)) +
  geom_boxplot(show.legend = FALSE) +
  labs(x = "Variável", y = "Medida") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# 2º Gráfico: Histograma
Hemo %>%
  pivot_longer(
    cols = Y1:Y6, names_to = "Variável", values_to = "Medida"
  ) %>%
  ggplot(aes(x = Medida, fill = `Variável`)) +
  geom_histogram() +
  labs(x = "Variável", y = "Frequência") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# 3º Gráfico: Pairplot
ggpairs(
  Hemo, columns = c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6")
)

# 4º Gráfico: Corrplot
corrplot(
  cor(Hemo[, -1]), method = "color", 
  addCoef.col = "red", tl.col = "black", 
  number.digits = 2, number.cex = 1
)