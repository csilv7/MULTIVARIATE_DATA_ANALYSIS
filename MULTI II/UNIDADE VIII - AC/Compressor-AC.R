#Analise de correspondencia Simples com o pacote CA
#Usar dados compressor
#instalar pacote "ca" se ainda não estiver instalado
install.packages("ca")
#ativar pacote ca
library(ca)
library (tibble)
library(dplyr)
Compressor = Compressor %>%
 column_to_rownames (var = "Comp")
  fit = ca (Compressor)
print (fit) # resultado básico summary (fit) # resultado extendido
fit$rowmass 
fit$rowdist 
fit$colcoord 
fit$rownames
plot(fit) # mapa simétrico
plot (fit, mass = TRUE, contrib = "absolute", map =
        "rowgreen"
      , arrows = C (FALSE, TRUE)) # mapa assimétrico
#Opcionalmente pode colocar a primeira coluna como rótulos
rownames(Compressor) <- Compressor$Comp
Compressor
#Executar AC
fit = ca(Compressor[2:4])
print(fit) # resultado básico
summary(fit) # resultado extendido
fit$rowmass
fit$rowdist
fit$colcoord
fit$rownames
plot(fit) # mapa simétrico
plot(fit, mass = TRUE, contrib = "absolute", map =
       "rowgreen", arrows = c(FALSE, TRUE)) # mapa assimétrico

