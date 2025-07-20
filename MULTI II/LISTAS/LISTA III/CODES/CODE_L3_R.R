# -------------------------------
# [*] CONFIGURAÇÕES INICIAIS DO R
# -------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# ----------------------
# [1] MANOVA A 2 FATORES
# ----------------------

library(readxl)
library(gt)

# Caminho do arquivo
path <- "~/PROJETOS/VS Code/MULTIVARIADA - VS/MULTI II/LISTAS/LISTA III/DATASETS/DADOS_LISTA_III.xlsx"

# Leitura do arquivo
df.L3 <- read_excel(path) %>%
  mutate(
    S = factor(S, labels = c("S1", "S2", "S3", "S4"), levels = c(1, 2, 3, 4)),
    V = factor(V, labels = c("V1", "V2", "V3"), levels = c(1, 2, 3))
  )

# -------------------------------------------
# [1.1] FAÇA A ANÁLISE EXPLORATÓRIA DOS DADOS
# -------------------------------------------

# Medidas de Resumo
df.L3 %>%
  group_by(S, V) %>%
  summarise(across(y1:y4, list(MEDIA = mean, SD = sd))) %>%
  gt() |>
  tab_spanner(
    label = md("$y_{1}$"),
    columns = c(y1_MEDIA, y1_SD)
  ) |>
  tab_spanner(
    label = md("$y_{2}$"),
    columns = c(y2_MEDIA, y2_SD)
  ) |> 
  tab_spanner(
    label = md("$y_{3}$"),
    columns = c(y3_MEDIA, y3_SD)
  ) |>
  tab_spanner(
    label = md("$y_{4}$"),
    columns = c(y4_MEDIA, y4_SD)
  ) |>
  cols_label(
    y1_MEDIA = html("Média"), y1_SD = html("Desvio Padrão"),
    y2_MEDIA = html("Média"), y2_SD = html("Desvio Padrão"),
    y3_MEDIA = html("Média"), y3_SD = html("Desvio Padrão"),
    y4_MEDIA = html("Média"), y4_SD = html("Desvio Padrão")
  ) |>
  tab_source_note(
    source_note = "DP: Desvio Padrão."
  )

# Boxplot (y1)
df.L3 %>%
  ggplot(aes(x = S, y = y1, color = V)) +
  geom_boxplot() +
  labs(x = "Data de Semeadura", y = "Precocidade do Rendimento", color = "Variedade") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# Boxplot (y2)
df.L3 %>%
  ggplot(aes(x = S, y = y2, color = V)) +
  geom_boxplot() +
  labs(x = "Data de Semeadura", y = "Precocidade da SLA", color = "Variedade") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# Boxplot (y3)
df.L3 %>%
  ggplot(aes(x = S, y = y3, color = V)) +
  geom_boxplot() +
  labs(x = "Data de Semeadura", y = "Rendimento Total", color = "Variedade") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# Boxplot (y4)
df.L3 %>%
  ggplot(aes(x = S, y = y3, color = V)) +
  geom_boxplot() +
  labs(x = "Data de Semeadura", y = "SLA Médio", color = "Variedade") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# -------------------------------
# [1.2] ORGANIZE O BANCO DE DADOS
# -------------------------------

# ------------------------------------------------------------------------------------------------------------
# [1.3] TESTE A INTERAÇÃO E EFEITOS PRINCIPAIS USANDO AS QUATRO ESTATÍSTICAS DE TESTE DA MANOVA A DOIS FATORES
# ------------------------------------------------------------------------------------------------------------

# Variáveis Respota
y <- as.matrix(df.L3[, !names(df.L3) %in% c("S", "V", "Rep")])

# Modelo MANOVA 2 Fatores
model.manova <- manova(y ~ S * V, data = df.L3)

# Testa a Hipótese NUla
summary(model.manova, test = "Wilks")
summary(model.manova, test = "Pillai")
summary(model.manova, test = "Hotelling-Lawley")
summary(model.manova, test = "Roy")

# Formatar a Tabela de Resultados
rls.manova <- tribble(
  ~Teste, ~Efeito, ~GL, ~Estatística, ~F, ~Num_DF, ~Den_DF, ~`Valor-p`, ~Signif,
  "Wilks", "S", 3, 0.000623, 151.94, 12, 119.35, "< 2,2e-16", "***",
  "Wilks", "V", 3, 0.065574, 32.682, 6, 96, "< 2,2e-16", "***",
  "Wilks", "S:V", 6, 0.135002, 5.111, 24, 158.20, "1,076e-10", "***",
  "Pillai", "S", 3, 2.3594, 43.28, 12, 141, "< 2,2e-16", "***",
  "Pillai", "V", 3, 1.1036, 14.157, 8, 92, "9,032e-13", "***",
  "Pillai", "S:V", 6, 1.3338, 4.002, 24, 192, "2,757e-08", "***",
  "Hotelling", "S", 3, 146.107, 531.67, 12, 131, "< 2,2e-16", "***",
  "Hotelling", "V", 3, 11.670, 64.19, 8, 88, "< 2,2e-16", "***",
  "Hotelling", "S:V", 6, 3.501, 6.35, 24, 174, "5,244e-14", "***",
  "Roy", "S", 3, 140.943, 1656.08, 4, 47, "< 2,2e-16", "***",
  "Roy", "V", 3, 10.251, 121.45, 4, 28, "< 2,2e-16", "***",
  "Roy", "S:V", 6, 2.686, 41.20, 6, 48, "4,496e-12", "***"
)

# Ajustando a Formatação da Tabela
rls.manova |>
  gt(groupname_col = "Teste") |>
  cols_label(
    Efeito = "Efeito",
    GL = md("$gl$"),
    Estatística = "Estatística",
    F = md("F $\\approx$"),
    Num_DF = md("$gl_{\\text{num}}$"),
    Den_DF = md("$gl_{\\text{den}}$"),
    `Valor-p` = md("Valor $p$"),
    Signif = "Signif."
  ) |>
  tab_source_note(
    source_note = md("Para $\\alpha = 0$: '`***`'; $\\alpha = 0.001$: '`**`'; $\\alpha = 0.01$: '`*`'; $\\alpha = 0.05$: '`.`'.")
  )

# -------------------------------------------------------------------------
# [1.4] FAÇA A ANÁLISE DESCREVENDO A METODOLOGIA, PROCEDIMENTOS E CONCLUSÃO
# -------------------------------------------------------------------------

# -------------------------------------
# [2] ANÁLISE DE COMPONENTES PRINCIPAIS
# -------------------------------------

library(HSAUR)

library(FactoMineR)
library(factoextra)

# -----------------------
# [2.1] CARREGAR OS DADOS
# -----------------------

# Carrgar Dados
df.PCA <- heptathlon

# --------------------------------
# [2.2] TRANSFORMAÇÃO DE VARIÁVEIS
# --------------------------------
df.PCA.modif <- df.PCA %>%
  mutate(
    hurdles = max(hurdles) - hurdles,
    run200m = max(run200m) - run200m,
    run800m = max(run800m) - run800m
  )

df.PCA.modif <- scale(df.PCA.modif)

# --------------------------
# [2.3] MATRIX DE CORRELAÇÃO
# --------------------------

# Matriz de Correlação
corr.matrix <- cor(df.PCA)

# Mapa de Calor
corrplot::corrplot(
  corr.matrix, 
  method = "color", addCoef.col = "green", tl.col = "black",
  number.digits = 2, number.font = 2
)

# ---------------------------------------
# [2.4] ANÁLISE DE COMPONENTES PRINCIPAIS
# ---------------------------------------

# Criando objeto PCA
model.PCA <- PCA(df.PCA.modif, graph = FALSE)

# -----------------------------------
# [2.4.1] ANÁLISE GRÁFICA: SCREE PLOT
# -----------------------------------

# Percentual de Variância
percent.of.var <- (model.PCA$svd$vs^2 / sum(model.PCA$svd$vs^2)) * 100
axis.X.scree <- 1:length(percent.of.var)

# Scree Plot
ggplot(data = NULL, aes(x = axis.X.scree, y = percent.of.var)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(color = "red") + geom_point(color = "red") +
  geom_text(
    aes(label = paste0(round(percent.of.var, 2), "%")),
    vjust = -0.5,
    hjust = -0.1,
    angle = 25,
    size = 2,
    fontface = "bold",
    color = "black"
  ) + 
  ylim(0, 70) +
  labs(x = "Dimensão", y = "Percentual de Variância Explicada") +
  theme_classic(base_size = 11) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
  scale_x_continuous(breaks = axis.X.scree)

# -----------------------------------------
# [2.4.2] ANÁLISE GRÁFICA: CARGAS FATORIAIS
# -----------------------------------------

# Obter Dados
var.coords <- as.data.frame(model.PCA$var$coord)

# Ajustar o nome das colunas
colnames(var.coords) <- paste("Dimensão", 1:ncol(var.coords))

# Plot de Cargas Fatoriais
ggplot(data = var.coords, aes(x = `Dimensão 1`, y = `Dimensão 2`)) +
  
  # Adicionar o Círculo de Corrlação Unitário (r = 1)
  annotate(
    "path",
    x = cos(seq(0, 2 * pi, length.out = 100)),
    y = sin(seq(0, 2 * pi, length.out = 100)),
    color = "black", linetype = "dashed"
  ) +
  
  # Linhas de Referência (x = 0, y = 0)
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  
  # Traçar Setas a partir da Origem
  geom_segment(
    aes(xend = `Dimensão 1`, yend = `Dimensão 2`, x = 0, y = 0),
    arrow = arrow(length = unit(0.01, "npc")),
    color = "blue", linewidth = 0.8
  ) +
  
  # Adicionar os nomes das variáveis
  geom_text(
    data = var.coords, 
    aes(x = `Dimensão 1` * 1.1, y = `Dimensão 2` * 1.1, label = row.names(var.coords)),
    color = "black", size = 4, #vjust = 1.5, hjust = 0.5
  ) +
  
  # Definir proporção de aspecto fixa (círculo parecerá um círculo)
  coord_fixed(ratio = 1) +
  # Ajustar limites dos eixos para incluir o círculo unitário e as setas
  xlim(-1.1, 1.1) +
  ylim(-1.1, 1.1) +
  labs(
    x = paste0("Dimensão 1 (", round(percent.of.var[1], 2), "%)"),
    y = paste0("Dimensão 2 (", round(percent.of.var[2], 2), "%)")
  ) +
  theme_classic(base_size = 11) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# -------------------------------
# [2.4.3] ANÁLISE GRÁFICA: BIPLOT
# -------------------------------

# Obter os Loadings
var.coords <- as.data.frame(model.PCA$var$coord)

# Obter os Dados Indivíduais (Atletas)
ind.coords <- as.data.frame(model.PCA$ind$coord)

# Ajustar o nome das colunas
colnames(var.coords) <- colnames(ind.coords) <- paste("Dimensão", 1:ncol(var.coords))

# Coluna "score" para Colorir Pontos (Perfumaria)
ind.coords$Score <- df.PCA$score[match(rownames(ind.coords), rownames(df.PCA))]

# Fator de Escala
f.scale <- 3

# Biplot
ggplot() +
  # Linhas de Referência (x = 0, y = 0)
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  
  # Indicíduos (Atletas)
  geom_point(
    data = ind.coords, aes(x = `Dimensão 1`, y = `Dimensão 2`),
    size = 3, alpha = 0.8
  ) +
  
  # Adicionar os Rótulos dos Atletas (opcional, pode deixar o gráfico carregado)
  geom_text(
    data = ind.coords, aes(x = `Dimensão 1`, y = `Dimensão 2`, label = 1:nrow(ind.coords)),
    vjust = -1, hjust = 0.5, size = 3, color = "black"
  ) +
  
  # Traçar Setas a partir da Origem
  geom_segment(
    data = var.coords, aes(xend = f.scale * `Dimensão 1`, yend = f.scale * `Dimensão 2`, x = 0, y = 0),
    arrow = arrow(length = unit(0.025, "npc")),
    color = "blue", linewidth = 0.8
  ) +
  
  # Adicionar os nomes das variáveis
  geom_text(
    data = var.coords, 
    aes(x = `Dimensão 1` * f.scale * 1.1, y = `Dimensão 2` * f.scale * 1.1, label = row.names(var.coords)),
    color = "blue", size = 4
  ) +
  # Títulos e rótulos
  labs(
    x = paste0("Dimensão 1 (", round(percent.of.var[1], 2), "%)"),
    y = paste0("Dimensão 2 (", round(percent.of.var[2], 2), "%)")
  ) +
  
  # Tema
  theme_classic(base_size = 11) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) + coord_fixed(ratio = 1)

# -------------------------------------
# [2.4.3] ANÁLISE GRÁFICA: CONTRIBUIÇÃO
# -------------------------------------

# Obter Dados de Contribuição das Variáveis
var.contrib <- as.data.frame(model.PCA$var$contrib)

# Ajustar o nome das colunas
colnames(var.contrib) <- 1:5 # paste("Dimensão", 1:ncol(var.contrib))

# Ajustando Dados para o ggplot2 (Formato Long)
var.contrib.LONG <- var.contrib %>%
  tibble::rownames_to_column(var = "Variable") %>%
  pivot_longer(
    cols = "1":"5",
    names_to = "Component",
    values_to = "Contribution"
  )

# Gerar gráfico
ggplot(
  data = var.contrib.LONG, 
  aes(x = Component, y = Variable, size = Contribution, fill = Contribution)
) +
  # Contribuição
  geom_point(shape = 21) + 
  scale_size_continuous(range = c(0, 10)) +
  scale_fill_gradientn(
    colours = c("lightblue", "skyblue", "royalblue", "blue", "darkblue"),
    values = scales::rescale(c(0, 25, 50, 75, 100))  # ajusta para sua escala de contribuição
  ) +
  # Rótulos
  labs(x = "Dimensão", y = "Variável", size = "Contribuição (%)", fill = "Contribuição (%)") +
  # Tema
  theme_classic(base_size = 11) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )