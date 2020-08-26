library(dplyr, warn.conflicts = FALSE)

#Criando a coluna de ruptura, soma acumulada por material - quantidade em estoque.
rup <- rup %>% group_by ( Carros ) %>% arrange(rup$`Data entrega`)
rup <- rup %>% group_by ( Carros ) %>% mutate ( SomaAcumulada = cumsum ( Qtdecarteira ) )
rup["Ruptura"] <- rup$Qtdeestoque - rup$SomaAcumulada

#Criando um DataFrame somente com as colunas necessárias.
rup <- data.frame(rup$Carros,rup$`Data entrega`,rup$Ruptura)

#Filtrando somente valores negativos (Ruptura), ordenando por data e eliminando digitos após a virgula.
rup <- filter(rup, rup$rup.Ruptura < 0)
rup <- rup %>% arrange(rup$rup..Data.entrega.)
options(digits = 3)

#Fazendo Pivot dos dados.
rup2 <- reshape(rup, direction = "wide", idvar = "rup.Carros", timevar = "rup..Data.entrega.")

#Removendo valores N/A
rup2 <- rup2 %>% replace(is.na(.), "")

#Criando uma tabela com os resultados.
library(flextable)

ft <- flextable(rup2)
ft <- autofit(ft)
ft