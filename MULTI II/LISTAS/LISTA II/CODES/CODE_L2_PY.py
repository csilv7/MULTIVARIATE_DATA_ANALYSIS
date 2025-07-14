import pandas as pd

j1 = pd.read_csv("C:/Users/user/Documents/PROJETOS/VS Code/MULTIVARIADA - VS/MULTI II/LISTAS/LISTA I/DATASETS/JEJUM.csv")
j2 = pd.read_csv("C:/Users/user/Documents/PROJETOS/VS Code/MULTIVARIADA - VS/MULTI II/LISTAS/LISTA I/DATASETS/JEJUM2.csv")

# Ajustes prévios a serem feitos
d1 = [j1[:i].values.T[0] if i == 3 else j1[i-3:i].values.T[0] for i in range(3, len(j1) + 1, 3)]
d2 = [j2[:i].values.T[0] if i == 3 else j2[i-3:i].values.T[0] for i in range(3, len(j2) + 1, 3)]

# Formatação de DataFrame e Coluna Indicadora
d1 = pd.DataFrame(d1)
d1["condition"] = "Jejum"

d2 = pd.DataFrame(d2)
d2["condition"] = "Ingestão"

# Ajustes Finais
df = pd.concat([d1, d2], ignore_index=True)
df.columns = ["S1", "S2", "S3", "condition"]

# Salvar Conjunto de Dados ajustados
df.to_csv("C:/Users/user/Documents/PROJETOS/VS Code/MULTIVARIADA - VS/MULTI II/LISTAS/LISTA I/DATASETS/df.csv", index=False)