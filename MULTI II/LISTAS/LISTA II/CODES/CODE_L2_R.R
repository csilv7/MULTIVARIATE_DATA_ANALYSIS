# -------------------------------
# [*] CONFIGURAÇÕES INICIAIS DO R
# -------------------------------
options(OutDec = ",", digits = 4)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(MVN)

# ----------------------------
# DEFINIÇÕES DE FUNÇÕES USADAS
# ----------------------------
AIQ <- function(x) {
  AIQ <- quantile(x, 0.75) - quantile(x, 0.25)
  LI <- quantile(x, 0.25) - 1.5 * AIQ
  LS <- quantile(x, 0.75) + 1.5 * AIQ
  
  return(data.frame(LI = LI, LS = LS))
}

ajust.iter <- function(x) {
  # Número de iterações necessárias
  n.iter <- ncol(x)
  
  # Iterando de Imputação de Dados
  for (col in seq(1, n.iter)) {
    # Obtendo Limites para Ajuste
    ajj <- AIQ(x[[col]])
    
    # Ajustando os Dados
    x[[col]] <- ifelse((x[[col]] >= ajj$LI) & (x[[col]] <= ajj$LS), x[[col]], median(x[[col]]))
  }
  
  # Retornar Dados Ajustados
  return(x)
}

plot.histogram <- function(df) {
  p11 <- ggplot(data = df, aes(x = S1)) +
    geom_histogram(bins = 10, fill ="green", alpha = 0.45) +
    labs(x = "1ª Semana", y = "Frequência") +
    theme_minimal()
  
  p12 <- ggplot(data = df, aes(x = S2)) +
    geom_histogram(bins = 10, fill ="red", alpha = 0.45) +
    labs(x = "2ª Semana", y = "Frequência") +
    theme_minimal()
  
  p13 <- ggplot(data = df, aes(x = S3)) +
    geom_histogram(bins = 10, fill ="blue", alpha = 0.45) +
    labs(x = "3ª Semana", y = "Frequência") +
    theme_minimal()
  
  return(grid.arrange(p11, p12, p13, nrow = 1))
}

# ---------------------
# [1] LEITURA DOS DADOS
# ---------------------

# Caminho do arquivo
path <- "C:/Users/user/Documents/PROJETOS/VS Code/MULTIVARIADA - VS/MULTI II/LISTAS/LISTA I/DATASETS/df.csv"

# Leitura do arquivo
df <- readr::read_csv(file = path)

# Separação dos Dados
y <- df[df$condition=="Jejum",]
x <- df[df$condition=="Ingestão",]

# --------------------------------
# [2] ANÁLISE DESCRITIVA DOS DADOS
# --------------------------------

# -----------------------
# [2.1] MEDIDAS DE RESUMO
# -----------------------

# ------------------
# [2.1.1] Y - JEJUM
# ------------------
describe <- rbind(
  matrix(summary(y$S1), nrow = 1, ncol = 6),
  matrix(summary(y$S2), nrow = 1, ncol = 6),
  matrix(summary(y$S3), nrow = 1, ncol = 6)
)

stds <- sqrt(diag(var(y[, -4])))

describe <- cbind(
  describe,
  as.matrix(stds, ncol = length(stds))
)

# Formatação de Tabela
knitr::kable(
  describe, 
  col.names = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo", "Desvio Padrão"),
  escape = FALSE,
  align = "c",
  booktabs = TRUE
)

# --------------------
# [2.1.2] X - INGESTÃO
# --------------------
describe <- rbind(
  matrix(summary(x$S1), nrow = 1, ncol = 6),
  matrix(summary(x$S2), nrow = 1, ncol = 6),
  matrix(summary(x$S3), nrow = 1, ncol = 6)
)

stds <- sqrt(diag(var(x[, -4])))

describe <- cbind(
  describe,
  as.matrix(stds, ncol = length(stds))
)

# Formatação de Tabela
knitr::kable(
  describe, 
  col.names = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo", "Desvio Padrão"),
  escape = FALSE,
  align = "c",
  booktabs = TRUE
)

# -------------
# [2.2] BOXPLOT
# -------------

# -----------------
# [2.2.1] Y - JEJUM
# -----------------

ggplot(data = y) +
  geom_boxplot(aes(x = "1ª", y = S1), fill = "green") + 
  geom_boxplot(aes(x = "2ª", y = S2), fill = "red") + 
  geom_boxplot(aes(x = "3ª", y = S3), fill = "blue") + 
  labs(x = "Semanas", y = "Níveis de Glicose no Sangue") +
  theme_minimal()

# --------------------
# [2.2.2] X - INGESTÃO
# --------------------

ggplot(data = x) +
  geom_boxplot(aes(x = "1ª", y = S1), fill = "green") + 
  geom_boxplot(aes(x = "2ª", y = S2), fill = "red") + 
  geom_boxplot(aes(x = "3ª", y = S3), fill = "blue") + 
  labs(x = "Semanas", y = "Níveis de Glicose no Sangue") +
  theme_minimal()

# ----------------------
# [2.3] AJUSTE DOS DADOS
# ----------------------

# Criando cópias dos Data Frames
y.copy <- dplyr::as_tibble(y)
x.copy <- dplyr::as_tibble(x)

# Ajustando os Dados
y.copy[, -4] <- ajust.iter(y.copy[, -4])
x.copy[, -4] <- ajust.iter(x.copy[, -4])

# -----------------------------
# [2.4] NOVAS MEDIDAS DE RESUMO
# -----------------------------

# ------------------
# [2.4.1] Y - JEJUM
# ------------------
describe <- rbind(
  matrix(summary(y.copy$S1), nrow = 1, ncol = 6),
  matrix(summary(y.copy$S2), nrow = 1, ncol = 6),
  matrix(summary(y.copy$S3), nrow = 1, ncol = 6)
)

stds <- sqrt( diag( var(y.copy[, -4]) ) )

describe <- cbind(
  describe,
  as.matrix(stds, ncol = length(stds))
)

# Formatação de Tabela
knitr::kable(
  describe, 
  col.names = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo", "Desvio Padrão"),
  escape = FALSE,
  align = "c",
  booktabs = TRUE
)

# --------------------
# [2.4.2] X - INGESTÃO
# --------------------
describe <- rbind(
  matrix(summary(x.copy$S1), nrow = 1, ncol = 6),
  matrix(summary(x.copy$S2), nrow = 1, ncol = 6),
  matrix(summary(x.copy$S3), nrow = 1, ncol = 6)
)

stds <- sqrt( diag( var(x.copy[, -4]) ) )

describe <- cbind(
  describe,
  as.matrix(stds, ncol = length(stds))
)

# Formatação de Tabela
knitr::kable(
  describe, 
  col.names = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo", "Desvio Padrão"),
  escape = FALSE,
  align = "c",
  booktabs = TRUE
)

# -------------
# [2.5] BOXPLOT
# -------------

# -----------------
# [2.5.1] Y - JEJUM
# -----------------

ggplot(data = y.copy) +
  geom_boxplot(aes(x = "1ª", y = S1), fill = "green") + 
  geom_boxplot(aes(x = "2ª", y = S2), fill = "red") + 
  geom_boxplot(aes(x = "3ª", y = S3), fill = "blue") + 
  labs(x = "Semanas", y = "Níveis de Glicose no Sangue") +
  theme_minimal()

# --------------------
# [2.5.2] X - INGESTÃO
# --------------------

ggplot(data = x.copy) +
  geom_boxplot(aes(x = "1ª", y = S1), fill = "green") + 
  geom_boxplot(aes(x = "2ª", y = S2), fill = "red") + 
  geom_boxplot(aes(x = "3ª", y = S3), fill = "blue") + 
  labs(x = "Semanas", y = "Níveis de Glicose no Sangue") +
  theme_minimal()

# ------------------------
# [2.6] BOXPLOT SEGMENTADO
# ------------------------

ggplot(data = rbind(y.copy, x.copy), aes(fill = condition)) +
  geom_boxplot(aes(x = "1ª", y = S1)) +
  geom_boxplot(aes(x = "2ª", y = S2)) +
  geom_boxplot(aes(x = "3ª", y = S2)) +
  labs(x = "Semanas", y = "Níveis de Glicose no Sangue") +
  theme(legend.title = "Condição") +
  theme_minimal()

# ----------------
# [2.7] HISTOGRAMA
# ----------------

# -----------------
# [2.7.1] Y - JEJUM
# -----------------
plot.histogram(y.copy)

# --------------------
# [2.7.2] X - INGESTÃO
# --------------------
plot.histogram(x.copy)

# ----------------
# [2.8] CORRELAÇÃO
# ----------------

# Scatterplot + Correlação (e Teste de Significância) + Densidade
GGally::ggpairs(rbind(y.copy, x.copy), 
                mapping = aes(colour = condition, alpha = 0.75), 
                columns = c("S1", "S2", "S3")) +
  theme_minimal()

# Plot dos Dados Gerais
corrplot::corrplot(cor(rbind(y.copy, x.copy)[,1:3]), method = "color", addCoef.col = "red", tl.col = "black", 
                   number.digits = 3, number.cex = 3)

# Plot das Mulheres em Jejum
corrplot::corrplot(cor(y.copy[,1:3]), method = "color", addCoef.col = "red", tl.col = "black", 
                   number.digits = 3, number.cex = 3)

# Plot das Mulheres em 1 Hora após a Ingestão de Açúcar
corrplot::corrplot(cor(x.copy[,1:3]), method = "color", addCoef.col = "red", tl.col = "black", 
                   number.digits = 3, number.cex = 3)

# --------------------------------------
# [3] TESTES DE NORMALIDADE MULTIVARIADA
# --------------------------------------

# Teste de Mardia
mardia.test.y <- mvn(y.copy[, -4], mvnTest = "mardia")
mardia.test.x <- mvn(x.copy[, -4], mvnTest = "mardia")

# Teste de Henze-Zirklers
hz.test.y <- mvn(y.copy[, -4], mvnTest = "hz")
hz.test.x <- mvn(x.copy[, -4], mvnTest = "hz")

# Teste de Royston
royston.test.y <- mvn(y.copy[, -4], mvnTest = "royston")
royston.test.x <- mvn(x.copy[, -4], mvnTest = "royston")