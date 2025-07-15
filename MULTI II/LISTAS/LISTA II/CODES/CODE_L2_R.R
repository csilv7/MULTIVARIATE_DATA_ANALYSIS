# --------------------------
# [*] CONFIGURAÇÕES INICIAIS
# --------------------------
library(readxl)
library(tidyverse)
library(MVN)
library(ggplot2)
library(car)

# Caminho do arquivo.XLSX
path <- "~/PROJETOS/VS Code/MULTIVARIADA - VS/MULTI II/LISTAS/LISTA II/DATASETS/DADOS_LISTA_II.xlsx"

# Leitura do arquivo.XLSX
DADOS_LISTA_II <- read_excel(path, sheet = "PEIXES")

# --------------------------------------------------
# [1] CONVERTA O BANCO DE DADOS PARA O FORMATO LONGO
# --------------------------------------------------

# 1. Empilhar os dados por método
m1 <- DADOS_LISTA_II %>%
  select(starts_with("M1")) %>%
  rename(AROMA = M1y1, SABOR = M1y2, TEXTURA = M1y3, UMIDADE = M1y4) %>%
  mutate(Method = "M1")

m2 <- DADOS_LISTA_II %>%
  select(starts_with("M2")) %>%
  rename(AROMA = M2y1, SABOR = M2y2, TEXTURA = M2y3, UMIDADE = M2y4) %>%
  mutate(Method = "M2")

m3 <- DADOS_LISTA_II %>%
  select(starts_with("M3")) %>%
  rename(AROMA = M3y1, SABOR = M3y2, TEXTURA = M3y3, UMIDADE = M3y4) %>%
  mutate(Method = "M3")

# 2. Juntar tudo em um único data frame
df <- bind_rows(m1, m2, m3) %>%
  mutate(Method = factor(Method)) %>%
  pivot_longer(cols = AROMA:UMIDADE, names_to = "VARIABLE", values_to = "SCORE")

# -------------------------------------------
# [2] FAÇA UMA ANÁLISE EXPLORATÓRIA DOS DADOS
# -------------------------------------------

# Medidas de Resumo
summary(df %>% filter(Method == "M1") %>% select(starts_with("y"))) # Método 1
summary(df %>% filter(Method == "M2") %>% select(starts_with("y"))) # Método 2
summary(df %>% filter(Method == "M3") %>% select(starts_with("y"))) # Método 3

# Boxplot
ggplot(data = df, aes(x = VARIABLE, y = SCORE, color = Method)) +
  geom_boxplot() +
  labs(x = "Características", y = "Score", color = "Método") +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
  theme_classic(base_size = 12)

# Visualizações variadas
df.WIDE <- bind_rows(m1, m2, m3) %>% mutate(Method = factor(Method))
GGally::ggpairs(df.WIDE, 
                mapping = aes(colour = Method, alpha = 0.75), 
                columns = colnames(df.WIDE)[1:4]) +
  theme_minimal()

# --------------------------------------
# [3] FAÇA A VERIFICAÇÃO DE PRESSUPOSTOS
# --------------------------------------