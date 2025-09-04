# -------------------------------------------------------------
# LISTA V - ANÁLISE DISCRIMINANTE (AD) - CONFIGURAÇÕES INICIAIS
# -------------------------------------------------------------

# Pacotes Necessários
library(dplyr)
library(ggplot2)
#library(MVN)
#library(biotools)

# Definir Diretório de Trabalho
setwd("~/PROJETOS/VS Code/MULTIVARIADA - VS/MULTI II")

# Leitura de Dados
Players <- readxl::read_excel("UNIDADE V - AD/Jogadores.xlsx")

# ------------------------
# [1] ANÁLISE EXPLORATÓRIA
# ------------------------

# Convertendo para o formato long
df.to_plot <- Players %>% tidyr::pivot_longer(
  cols = colnames(Players %>% select(-Grupo)),
  names_to = "VARIABLES",
  values_to = "VALUES"
) %>%
  mutate(GROUP = as.factor(Grupo)) %>%
  select(-Grupo)

# -----------------------
# [1.1] TABELAS DE RESUMO
# -----------------------

# -----------------------------
# [1.1.1] TABELA RESUMO (GERAL)
# -----------------------------
df.to_plot %>% select(-GROUP) %>% group_by(VARIABLES) %>% summarise(
  Mean = mean(VALUES) %>% round(2),
  Std = sd(VALUES) %>% round(2),
  CV = sd(VALUES) / mean(VALUES),
  Minimun = min(VALUES) %>% round(2),
  Q1 = quantile(VALUES, 0.25) %>% round(2),
  Median = median(VALUES) %>% round(2),
  Q3 = quantile(VALUES, 0.75) %>% round(2),
  Maximun = max(VALUES) %>% round(2)
) %>%
  gt() %>%
  cols_label(
    VARIABLES = "Variável",
    Mean = "Média",
    Std = "Desvio Padrão",
    Minimun = "Mínimo",
    Q1 = "1º Quartil",
    Median = "Mediana",
    Q3 = "3º Quartil",
    Maximun = "Máximo"
  ) %>%
  cols_align(
    align = "center",
    columns = c("Mean", "Std", "CV", "Minimun", "Q1", "Median", "Q3", "Maximun")
  ) %>%
  fmt_percent(columns = CV, dec_mark = ",")

# ---------------------------------
# [1.1.2] TABELA RESUMO (POR GRUPO)
# ---------------------------------
df.to_plot %>% group_by(VARIABLES, GROUP) %>% summarise(
  Mean = mean(VALUES) %>% round(2),
  Std = sd(VALUES) %>% round(2),
  CV = sd(VALUES) / mean(VALUES),
  Minimun = min(VALUES) %>% round(2),
  Q1 = quantile(VALUES, 0.25) %>% round(2),
  Median = median(VALUES) %>% round(2),
  Q3 = quantile(VALUES, 0.75) %>% round(2),
  Maximun = max(VALUES) %>% round(2)
) %>%
  gt() %>%
  cols_label(
    GROUP = "Grupo",
    Mean = "Média",
    Std = "Desvio Padrão",
    Minimun = "Mínimo",
    Q1 = "1º Quartil",
    Median = "Mediana",
    Q3 = "3º Quartil",
    Maximun = "Máximo"
  ) %>%
  cols_align(
    align = "center",
    columns = c("Mean", "Std", "CV", "Minimun", "Q1", "Median", "Q3", "Maximun")
  ) %>%
  fmt_percent(columns = CV, dec_mark = ",")

# --------------
# [1.2] GRÁFICOS
# --------------

# -------------------
# [1.2.1] UNIVARIADOS 
# -------------------

# Histograma
ggplot(data = Players) +
  geom_histogram(aes(x = Larg_cab), bins = 15, color = "white", fill = "steelblue") +
  labs(
    x = "Larg_cab", y = "Frquência"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# Histograma
ggplot(data = Players) +
  geom_histogram(aes(x = Circu_cab), bins = 15, color = "white", fill = "steelblue") +
  labs(
    x = "Larg_cab", y = "Frquência"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# Histograma
ggplot(data = Players) +
  geom_histogram(aes(x = Frent_tras), bins = 15, color = "white", fill = "steelblue") +
  labs(
    x = "Larg_cab", y = "Frquência"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# Histograma
ggplot(data = Players) +
  geom_histogram(aes(x = olh_cab), bins = 15, color = "white", fill = "steelblue") +
  labs(
    x = "Larg_cab", y = "Frquência"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# Histograma
ggplot(data = Players) +
  geom_histogram(aes(x = ore_cab), bins = 15, color = "white", fill = "steelblue") +
  labs(
    x = "Larg_cab", y = "Frquência"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# Histograma
ggplot(data = Players) +
  geom_histogram(aes(x = Larg_quei), bins = 15, color = "white", fill = "steelblue") +
  labs(
    x = "Larg_cab", y = "Frquência"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# ----------------
# [1.2.2] BOXPLOTS
# ----------------
# Gráfico com ggplot2
ggplot(data = df.to_plot, aes(x = VARIABLES, y = VALUES, color = GROUP)) +
  geom_boxplot() +
  labs(
    x = "Variáveis", y = "Medições", color = "Grupo"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# ----------------
# [1.2.3] PAIRPLOT
# ----------------

# Gerar Visualização
GGally::ggpairs(
  Players, 
  mapping = aes(colour = as.factor(Grupo), alpha = 0.75), 
  columns = Players %>% select(-Grupo) %>% colnames()
) + theme_minimal()

# ------------------------------------------------
# [2] PRESSUPOSTOS: NORM MULT E HOMOG MATRIZ COVAR
# ------------------------------------------------

# ------------------------------
# [2.1] NORMALIDADE MULTIVARIADA
# ------------------------------

# Teste de Mardia
mardia.test <- MVN::mvn(Players[, -1], mvn_test = "mardia") ; mardia.test$multivariate_normality

# Teste de Henze-Zirklers
hz.test <- MVN::mvn(Players[, -1], mvn_test = "hz") ; hz.test$multivariate_normality

# Teste de Royston
royston.test <- MVN::mvn(Players[, -1], mvn_test = "royston") ; royston.test$multivariate_normality


# Teste de Normalidade Univariada
shapiro.rls <- tibble()
for (i in 2:7) {
  shapiro.rls <- shapiro.rls %>%
    bind_rows(
      tibble(
        Variable = colnames(Players)[i],
        W = shapiro.test(Players[[i]])$statistic,
        ValueP = shapiro.test(Players[[i]])$p.value,
        Normality = ifelse(shapiro.test(Players[[i]])$p.value <= 0.05, "Not normal", "Normal")
      )
    )
}

# Tabela Formata com o Teste de Shapiro para Normalidade Multivariada
gt(shapiro.rls) %>%
  cols_label(
    Variable = "Variável",
    W = md("Estatística $W$"),
    ValueP = md("Valor $p$"),
    Normality = "Normalidade"
  ) %>%
  fmt_number(
    columns = c(W, ValueP),
    decimals = 4
  ) %>%
  cols_align(
    align = "center",
    columns = c(W, ValueP)
  )

# ---------------------------
# [2.1.1] TRANSFORMAÇÃO BOX-COX
# ---------------------------
Players.trf <- 1 * Players

m1.for.boxcox <- lm(formula = Larg_cab ~ 1, data = Players.trf)
r1.boxcox <- MASS::boxcox(m1.for.boxcox, lambda = seq(-4, 4, by = 0.01))
lambda1.optim <- r1.boxcox$x[which.max(r1.boxcox$y)]
Players.trf$Larg_cab <- (Players.trf$Larg_cab^lambda1.optim - 1) / lambda1.optim

m2.for.boxcox <- lm(formula = Circu_cab ~ 1, data = Players.trf)
r2.boxcox <- MASS::boxcox(m2.for.boxcox, lambda = seq(-4, 4, by = 0.01))
lambda2.optim <- r1.boxcox$x[which.max(r2.boxcox$y)]
Players.trf$Circu_cab <- (Players.trf$Circu_cab^lambda2.optim - 1) / lambda2.optim

# Teste de Normalidade Univariada
shapiro.rls2 <- tibble()
for (column in Players.trf %>% select(-c(Grupo)) %>% colnames()) {
  shapiro.rls2 <- shapiro.rls2 %>%
    bind_rows(
      tibble(
        Variable = column,
        W = shapiro.test(Players.trf[[column]])$statistic,
        ValueP = shapiro.test(Players.trf[[column]])$p.value,
        Normality = ifelse(shapiro.test(Players.trf[[column]])$p.value <= 0.05, "Not normal", "Normal")
      )
    )
}

# Tabela Formata com o Teste de Shapiro para Normalidade Multivariada
gt(shapiro.rls2) %>%
  cols_label(
    Variable = "Variável",
    W = md("Estatística $W$"),
    ValueP = md("Valor $p$"),
    Normality = "Normalidade"
  ) %>%
  fmt_number(
    columns = c(W, ValueP),
    decimals = 4
  ) %>%
  cols_align(
    align = "center",
    columns = c(W, ValueP)
  )

# Teste de Normalidade Multivariada
mardia.test2 <- MVN::mvn(Players.trf[, -1], mvn_test = "mardia") ; mardia.test2$multivariate_normality
hz.test2 <- MVN::mvn(Players.trf[, -1], mvn_test = "hz") ; hz.test2$multivariate_normality
royston.test2 <- MVN::mvn(Players.trf[, -1], mvn_test = "royston") ; royston.test2$multivariate_normality

# ------------------------------------------------
# [2.2] HOMOCEDASTICIDADE DE MATRIZ DE COVARIÂNCIA
# ------------------------------------------------

# Teste M de Box
boxM.test <- biotools::boxM(data = Players.trf[, -1], grouping = Players.trf$Grupo) ; boxM.test

# ------------
# [2.3] MANOVA
# ------------

# Criando Modelo de Análise de Variância Multivariada
manova.model <- manova(
  cbind(Larg_cab, Circu_cab, Frent_tras, olh_cab, ore_cab, Larg_quei) ~ Grupo, 
  data = Players.trf
)

# Testando a Hipótese Nula
summary(manova.model, test = "Wilks")
summary(manova.model, test = "Pillai")
summary(manova.model, test = "Hotelling-Lawley")
summary(manova.model, test = "Roy")

# Formatar a Tabela de Resultados
rls.manova <- data.frame(
  Teste = c("Wilks", "Pillai", "Hotelling-Lawley", "Roy"),
  GL = rep(1, 4),
  Statistc = c(0.54228, 0.45772, 0.84407, 0.84407),
  ApproxF = c(11.676, 11.676, 11.676, 11.676),
  NumDF = rep(6, 4),
  DenDF = rep(83, 4),
  ValorP = c(1.912e-09, 1.912e-09, 1.912e-09, 1.912e-09),
  Signif = rep("***", 4)
)

# Ajustando a Formatação da Tabela
gt(rls.manova) %>%
  cols_label(
    GL = md("$gl$"),
    Statistc = "Estatística",
    ApproxF = md("F $\\approx$"),
    NumDF = md("$gl_{\\text{num}}$"),
    DenDF = md("$gl_{\\text{den}}$"),
    ValorP = md("Valor $p$"),
    Signif = "Significância"
  )  %>%
  cols_align(
    align = "center",
    columns = c("Statistc", "ApproxF", "NumDF", "DenDF", "ValorP", "Signif")
  ) %>%
  fmt_number(
    columns = c(Statistc, ApproxF),
    decimals = 2
  ) %>%
  fmt_scientific(columns = ValorP, dec_mark = ",") %>%
  tab_source_note(
    source_note = md(
      "Significância: Rejeita-se $H_{0}$ para 
      $\\alpha = 0$ ('`***`'); 
      $\\alpha = 0.001$ ('`**`'); 
      $\\alpha = 0.01$ ('`*`'); 
      $\\alpha = 0.05$ ('`.`')"
    )
  )

# -----------------------------------
# [3] ANÁLISE DE DSICRIMINANTE LINEAR
# -----------------------------------

# Tranformando para Fator
Players.trf$Grupo <- as.factor(Players.trf$Grupo)

# Modelo de Discriminação Linear
model.lda <- MASS::lda(Grupo ~ ., data = Players.trf %>% select(-Circu_cab))

# Fazer as previsões usando o modelo LDA
prevs.lda <- predict(model.lda, Players.trf)

# Imprimir a matriz de confusão
print(table(Real = Players.trf$Grupo, Previsto = prevs.lda$class))

# Criar a matriz de confusão
matrix.confusion <- matrix(
  data = c(26, 1, 3, 1, 20, 9, 2, 8, 20),
  ncol = 3,
  byrow = T,
  dimnames = list(
    paste0("Grupo ", 1:3),
    paste0("Grupo ", 1:3)
  )
) 

# Calcular a acurácia
accuracy <- sum(diag(matrix.confusion)) / sum(matrix.confusion) ; paste0("Acurácia do Modelo: ", round(accuracy * 100, 2), "%")

# Formatando como Tabela
matrix.confusion %>% as.data.frame() %>% gt(rownames_to_stub = T) %>%
  cols_align(columns = everything(), align = "center") %>% 
  tab_options(
    table.width = pct(35),
    latex.use_longtable = T
  ) %>%
  opt_table_font(
    size = 12
  )

# -------------------------------
# [3.1] FORMATAÇÃO DOS RESULTADOS
# -------------------------------

# Ajutes dos Dados
tbl.coef_lda <- as.data.frame(model.lda$scaling)
tbl.coef_lda$Vars <- c("Larg_cab", "Frent_tras", "olh_cab", "ore_cab", "Larg_quei")
tbl.coef_lda <- tbl.coef_lda %>% select(Vars, LD1, LD2)

# Tabela com gt
gt(tbl.coef_lda) %>%
  cols_label(Vars = "Variável") %>%
  cols_align(align = "center", columns = 2:3) %>%
  fmt_number(columns = 2:3, decimals = 4, dec_mark = ",")

gt(matrix.confusion)

# -------------------------------------------------------------
# [4] REALIZE O PROCEDIMENTO STEPWISE PARA SELEÇÃO DE VARIÁVEIS
# -------------------------------------------------------------

# Separação dos Dados
X <- Players.trf %>% select(-Grupo)
y <- Players.trf$Grupo

# Stepwise para Seleção de Variáveis
step.var.forward <- klaR::stepclass(
  x = X, grouping = y, 
  method = "lda",
  improvement = 1e-2,
  direction = "forward" # “forward”, “backward” or “both” (default)
)

# Visualizar
print(step.var.forward)

# Stepwise para Seleção de Variáveis
step.var.backward <- klaR::stepclass(
  x = X, grouping = y, 
  method = "lda",
  improvement = 1e-2,
  direction = "backward" # “forward”, “backward” or “both” (default)
)

# Visualizar
print(step.var.backward)

step.var.both <- klaR::stepclass(
  x = X, grouping = y, 
  method = "lda",
  improvement = 1e-2,
  direction = "both" # “forward”, “backward” or “both” (default)
)

# Visualizar
print(step.var.both)


# --------------------------------------
# [5] REFAZER A ANÁLISE DE DISCRIMINANTE
# --------------------------------------

# Modelo de Discriminação Linear
model.lda.adj <- MASS::lda(Grupo ~ Larg_cab + olh_cab, data = Players.trf)

# Fazer as previsões usando o modelo LDA
prevs.lda.adj <- predict(model.lda.adj, Players.trf)

# Criar a matriz de confusão
matrix.confusion.adj <- table(Real = Players.trf$Grupo, Previsto = prevs.lda.adj$class) ; print(matrix.confusion)

# Calcular a acurácia
accuracy.adj <- sum(diag(matrix.confusion.adj)) / sum(matrix.confusion.adj) ; paste0("Acurácia do Modelo: ", round(accuracy * 100, 2), "%")

# ---------------------------------------
# [6] ANÁLISE DE DISCRIMINANTE QUADRÁTICA
# ---------------------------------------

# -----------------------------------
# [6.1] MODELO COM TODAS AS VARIÁVEIS
# -----------------------------------

# Modelo de Discriminação Quadrático
model.qda <- MASS::qda(Grupo ~ ., data = Players.trf)

# Fazer as previsões usando o modelo LDA
prevs.qda <- predict(model.qda, Players.trf)

# Criar a matriz de confusão
matrix.confusion.qda <- table(Real = Players.trf$Grupo, Previsto = prevs.qda$class) ; print(matrix.confusion)

# Calcular a acurácia
accuracy.qda <- sum(diag(matrix.confusion.qda)) / sum(matrix.confusion.qda) ; paste0("Acurácia do Modelo: ", round(accuracy.qda * 100, 2), "%")

# -------------------------
# [6.2] MODELO COM STEPWISE
# -------------------------

# Separação dos Dados
X <- Players.trf %>% select(-Grupo)
y <- Players.trf$Grupo

# --------------------------
# [7] COMPARAÇÃO DOS MODELOS
# --------------------------

# Stepwise para Seleção de Variáveis
qda.step.var.forward <- klaR::stepclass(
  x = X, grouping = y, 
  method = "qda",
  improvement = 1e-2,
  direction = "forward" # “forward”, “backward” or “both” (default)
)

# Visualizar
print(qda.step.var.forward)

# Stepwise para Seleção de Variáveis
qda.step.var.backward <- klaR::stepclass(
  x = X, grouping = y, 
  method = "qda",
  improvement = 1e-2,
  direction = "backward" # “forward”, “backward” or “both” (default)
)

# Visualizar
print(qda.step.var.backward)

qda.step.var.both <- klaR::stepclass(
  x = X, grouping = y, 
  method = "qda",
  improvement = 1e-2,
  direction = "both" # “forward”, “backward” or “both” (default)
)

# Visualizar
print(qda.step.var.both)
