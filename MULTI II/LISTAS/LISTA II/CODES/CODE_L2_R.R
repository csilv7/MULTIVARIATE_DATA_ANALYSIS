# --------------------------
# [*] CONFIGURAÇÕES INICIAIS
# --------------------------
library(readxl)

library(dplyr)

library(ggplot2)

library(MVN)
library(biotools)

library(rstatix)

# Caminho do arquivo.XLSX
path <- "~/PROJETOS/VS Code/MULTIVARIADA - VS/MULTI II/LISTAS/LISTA II/DATASETS/DADOS_LISTA_II.xlsx"

# Leitura do arquivo.XLSX
DADOS_LISTA_II <- read_excel(path, sheet = "PEIXES")

# --------------------------------------------------
# [1] CONVERTA O BANCO DE DADOS PARA O FORMATO LONGO
# --------------------------------------------------

# ----------------------------
# [1.1] OBTER DADOS POR MÉTODO
# ----------------------------
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

# --------------------------------------
# [1.2] CONCATENAR E CONVERTER PARA LONG
# --------------------------------------
df.LONG <- bind_rows(m1, m2, m3) %>%
  mutate(Method = factor(Method)) %>%
  pivot_longer(cols = AROMA:UMIDADE, names_to = "VARIABLE", values_to = "SCORE")

# Salvar Dados em Formato Long
write.csv(df.LONG, file = "~/PROJETOS/VS Code/MULTIVARIADA - VS/MULTI II/LISTAS/LISTA II/DATASETS/DADOS_L2Q2_LONG.csv")

# -------------------------------------------
# [2] FAÇA UMA ANÁLISE EXPLORATÓRIA DOS DADOS
# -------------------------------------------

# -----------------------
# [2.1] MEDIDAS DE RESUMO
# -----------------------
summary(m1) # Método 1
summary(m2) # Método 2
summary(m3) # Método 3

# -------------
# [2.2] BOXPLOT
# -------------
ggplot(data = df.LONG, aes(x = VARIABLE, y = SCORE, color = Method)) +
  geom_boxplot() +
  labs(x = "Características", y = "Score", color = "Método") +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
  theme_classic(base_size = 12)

# -----------------------------
# [2.3] MÚLTIPLAS VISUALIZAÇÕES
# -----------------------------

# Converter para o Formato Wide
df.WIDE <- bind_rows(m1, m2, m3) %>% mutate(Method = factor(Method))

# Salvar Dados em Formato Wide
write.csv(df.WIDE, file = "~/PROJETOS/VS Code/MULTIVARIADA - VS/MULTI II/LISTAS/LISTA II/DATASETS/DADOS_L2Q2_WIDE.csv")

# Gerar Visualização
GGally::ggpairs(df.WIDE, 
                mapping = aes(colour = Method, alpha = 0.75), 
                columns = colnames(df.WIDE)[1:4]) +
  theme_minimal()

# --------------------------------------
# [3] FAÇA A VERIFICAÇÃO DE PRESSUPOSTOS
# --------------------------------------

# ------------------------------
# [3.1] NORMALIDADE MULTIVARIADA
# ------------------------------

# Teste de Mardia
mardia.test <- mvn(df.WIDE[, -5], mvnTest = "mardia") ; mardia.test

# Teste deHenze-Zirklers
hz.test <- mvn(df.WIDE[, -5], mvnTest = "hz") ; hz.test

# Teste de Royston
royston.test <- mvn(df.WIDE[, -5], mvnTest = "royston") ; royston.test

# ------------------------------------------------
# [3.2] HOMOCEDASTICIDADE DE MATRIZ DE COVARIÂNCIA
# ------------------------------------------------

# Teste M de Box
boxM.test <- boxM(data = df.WIDE[, -5], grouping = df.WIDE$Method) ; boxM.test

# --------------------------------------------------------------------------------------------------------------------------------
# [4] FAÇA UMA MANOVA USANDO TODOS OS QUATROS TESTES A UM FATOR PARA COMPARAR OS TRÊS MÉTODOS RELATIVAMENTE ÀS VARIÁVEIS ESTUDADAS
# --------------------------------------------------------------------------------------------------------------------------------

# Criando Modelo de Análise de Variância Multivariada
manova.model <- manova(cbind(AROMA, SABOR, TEXTURA, UMIDADE) ~ Method, data = df.WIDE)

# Testando a Hipótese Nula
summary(manova.model, test = "Wilks")
summary(manova.model, test = "Pillai")
summary(manova.model, test = "Hotelling-Lawley")
summary(manova.model, test = "Roy")

# ------------------------------------------------------------------------------------------------------
# [5] FAÇA ANÁLISE A POSTERIORI SE OS TESTES MULTIVARIADOS REJEITAREM A HIPÓTESE NULA. CONCLUA A ANÁLISE
# ------------------------------------------------------------------------------------------------------

# AROMA
emmeans_test(df.WIDE, AROMA ~ Method, p.adjust.method = "bonferroni")
emmeans_test(df.WIDE, AROMA ~ Method, p.adjust.method = "sidak")

# SABOR
emmeans_test(df.WIDE, SABOR ~ Method, p.adjust.method = "bonferroni")
emmeans_test(df.WIDE, SABOR ~ Method, p.adjust.method = "sidak")

# TEXTURA
emmeans_test(df.WIDE, TEXTURA ~ Method, p.adjust.method = "bonferroni")
emmeans_test(df.WIDE, TEXTURA ~ Method, p.adjust.method = "sidak")

# UMIDADE
emmeans_test(df.WIDE, UMIDADE ~ Method, p.adjust.method = "bonferroni")
emmeans_test(df.WIDE, UMIDADE ~ Method, p.adjust.method = "sidak")

# AROMA
TukeyHSD(x = aov(AROMA ~ Method, data = df.WIDE), "Method", conf.level = 0.95)

# SABOR
TukeyHSD(x = aov(SABOR ~ Method, data = df.WIDE), "Method", conf.level = 0.95)

# TEXTURA
TukeyHSD(x = aov(TEXTURA ~ Method, data = df.WIDE), "Method", conf.level = 0.95)

# UMIDADE
TukeyHSD(x = aov(UMIDADE ~ Method, data = df.WIDE), "Method", conf.level = 0.95)