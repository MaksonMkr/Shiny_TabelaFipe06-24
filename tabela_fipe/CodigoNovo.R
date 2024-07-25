library(tidyverse)

##Carregar arquivo

library(readxl)

#Dados novos
Tabela_fipe_bruto2 <- read_excel("dados/FIPE_JUNHO_2024.xlsx")

colnames(Tabela_fipe_bruto2) <- c("Id", "codigoFipe", "Marca", "Modelo", "anoModelo", 
                                  "Combustivel", "Valor", "TipoVeiculo", "Mês")

## Retirar motos e caminhoes do conjunto de dados e selecionar Carros a partir de 2003

Tabela_clust <- Tabela_fipe_bruto2 |> 
  filter(TipoVeiculo == "Carro" & anoModelo >= 2000)

## Verificar o tipo da variável de cada coluna

glimpse(Tabela_clust)

## Reajustando a coluna Valor
Tabela_clust$Valor <- as.numeric(sub("\\..*", "", Tabela_clust$Valor))

##Criando colunas
Tabela_clust <- Tabela_clust |>
  mutate(cambio = str_extract(Modelo, "Aut\\.|Mec\\.|Semi|Auto")) |> 
  mutate(cilindros = str_extract(Modelo, "(V\\d{2})|V\\d{1}")) |>
  mutate(valvulas = str_extract(Modelo, "(\\d{2}V|\\d{1}V)")) |> 
  mutate(tracao = str_extract(Modelo, "(4x4|4x2|4X4|4X2)")) |> 
  mutate(cilindradas = str_extract(Modelo, "(\\d{1}\\.\\d{1})"))

## Retirando veículos elétricos
Tabela_clust <- Tabela_clust[is.na(str_extract(Tabela_clust$Modelo, "Elé")), ]

## Botando carros OKm no ano 2024
Tabela_clust$Modelo <- ifelse(Tabela_clust$anoModelo == 32000,
                              paste(Tabela_clust$Modelo, "0km", sep = " "),
                              Tabela_clust$Modelo)
Tabela_clust$anoModelo[Tabela_clust$anoModelo == 32000] <- 2024

## colocar ano no modelo
Tabela_clust$Modelo <- paste(Tabela_clust$Modelo, 
                             Tabela_clust$anoModelo, sep = " ")

## Reajeitando coluna tração e câmbio
Tabela_clust$tracao[Tabela_clust$tracao == "4X2"] <- "4x2"
Tabela_clust$tracao[Tabela_clust$tracao == "4X4"] <- "4x4"
Tabela_clust$cambio[Tabela_clust$cambio == "Aut."] <- "Auto"

Marcas <- Tabela_clust |> 
            count(Marca) |> 
            arrange(desc(n))

#####################
## Colocar tração nos NA's
marcas_4x2 <- c("VW - VolksWagen", "Fiat", "Renault", "Peugeot", "Citroên", "Hyundai", "Nissan", 
                "Honda", "Kia Motors", "Mitsubishi")
marcas_4x4 <- c("Jeep", "Land Rover", "Toyota", "Subaru")

Tabela_clust <- Tabela_clust |>
  mutate(tracao = ifelse(Marca %in% marcas_4x2 & is.na(tracao), "4x2", tracao))
Tabela_clust <- Tabela_clust |>
  mutate(tracao = ifelse(grepl("L200", Modelo), "4x4", tracao))
Tabela_clust <- Tabela_clust |>
  mutate(tracao = ifelse(grepl("Pajero", Modelo), "4x4", tracao))
Tabela_clust <- Tabela_clust |>
  mutate(tracao = ifelse(Marca %in% marcas_4x4 & is.na(tracao), "4x4", tracao))
Tabela_clust <- Tabela_clust |>
  mutate(tracao = ifelse(grepl("Corolla", Modelo), "4x2", tracao))
Tabela_clust <- Tabela_clust |>
  mutate(tracao = ifelse(Marca == "BMW" & grepl("xDrive", Modelo), "4x4", tracao))
Tabela_clust <- Tabela_clust |>
  mutate(tracao = ifelse(Marca == "BMW" & grepl("XDRIVE", Modelo), "4x4", tracao))
Tabela_clust <- Tabela_clust |>
  mutate(tracao = ifelse(Marca == "BMW" & grepl("XDrive", Modelo), "4x4", tracao))
Tabela_clust <- Tabela_clust |>
  mutate(tracao = ifelse(Marca == "Audi" & grepl("QUATTRO", Modelo), "4x4", tracao))
Tabela_clust <- Tabela_clust |>
  mutate(tracao = ifelse(Marca == "Audi" & grepl("Quattro", Modelo), "4x4", tracao))
Tabela_clust <- Tabela_clust |>
  mutate(tracao = ifelse(Marca == "Porsche" & grepl("4S", Modelo), "4x4", tracao))
Tabela_clust <- Tabela_clust |>
  mutate(tracao = ifelse(Marca == "Porsche" & grepl(" 4 ", Modelo), "4x4", tracao))
Tabela_clust <- Tabela_clust |>
  mutate(tracao = ifelse(Marca == "Agrale" & grepl("MARRUÁ", Modelo), "4x4", tracao))
Tabela_clust <- Tabela_clust |>
  mutate(tracao = ifelse(Marca == "Mercedes-Benz" & grepl("4MATIC", Modelo), "4x4", tracao))
Tabela_clust <- Tabela_clust |>
  mutate(tracao = ifelse(grepl("4WD", Modelo), "4x4", tracao))
Tabela_clust <- Tabela_clust |>
  mutate(tracao = ifelse(grepl("AWD", Modelo), "4x4", tracao))

rm(marcas_4x2, marcas_4x4)

## Botando tracao 4x2 nos resto dos NA's
Tabela_clust <- Tabela_clust |> 
  mutate(tracao = replace_na(Tabela_clust$tracao, "4x2"))

##############################################
##Botando cambio mecanico nos valores na's

Tabela_clust <- Tabela_clust |> 
  mutate(cambio = replace_na(Tabela_clust$cambio, "Auto"))

## Botando carros com 2.0 de cilindradas
Tabela_clust <- Tabela_clust |> 
  mutate(cilindradas = replace_na(Tabela_clust$cilindradas, "2.0"))

## Colocando valores nos na's em cilindros e válvulas
Tabela_clust <- Tabela_clust |> 
  mutate(valvulas = replace_na(Tabela_clust$valvulas, "16V"))

Tabela_clust <- Tabela_clust |> 
  mutate(cilindros = replace_na(Tabela_clust$cilindros, "V4"))

rm(Marcas)

## Organizando colunas para ficar numerico

Tabela_clust <- Tabela_clust |> 
  mutate(valvulas = str_replace(valvulas, "V", "") |> as.numeric())

Tabela_clust <- Tabela_clust |> 
  mutate(cilindros = str_replace(cilindros, "V", "") |> as.numeric())

Tabela_clust$cilindradas <- as.numeric(Tabela_clust$cilindradas)

Tabela_clust <- Tabela_clust |> 
  select(Marca, Modelo, anoModelo, Combustivel, Valor, cambio, 
         cilindros, valvulas, tracao, cilindradas)

# One Hot Encoding (cambio, combustivel, tracao)

library(caret)
dummy <- dummyVars(" ~ .", data = Tabela_clust[, c(4, 6, 9)]) 
newdata <- data.frame(predict(dummy, newdata = Tabela_clust))

Tabela_clust <- cbind(Tabela_clust, newdata)
Tabela_clust <- Tabela_clust |> 
  select(-tracao, -Combustivel, -cambio)

rm(newdata)
rm(dummy)

##Clusterização

library(factoextra)

set.seed(10)

rm(indice)

indice <- sample(1:21820, 21820) 

x <- scale(Tabela_clust[indice, c(3:15)])

Tabela_clust.kmeans <- kmeans(x, centers = 3)

Tabela_clust.pca <- prcomp(x, center = TRUE, scale. = TRUE)

summary(Tabela_clust.pca)

Tabela_clust.plot <- data.frame(x,
                                Tabela_clust.pca$x,
                                Modelo = Tabela_clust$Modelo[indice],
                                Clusters = as.character(Tabela_clust.kmeans$cluster))

#Renomear nome das linhas de Tabela_clust.plot
rownames(Tabela_clust.plot) <- seq(1, nrow(Tabela_clust.plot))

rm(Tabela_clust.kmeans)

##Criando matrix das distancias entre os pontos do gráfico
distances <- dist(Tabela_clust.plot[, 1:13], diag = TRUE, upper = TRUE, method = "euclidean")
#distances <- dist(Tabela_clust.plot[c("PC1", "PC2")], diag = TRUE, upper = TRUE, method = "euclidean")
distances <- as.matrix(distances)

lista_indices_proximos <- vector("list", nrow(distances))

## ordenar cada linha da lista
for (i in 1:nrow(distances)) {
  lista_indices_proximos[[i]] <- as.vector(order(distances[i,]))
}

dados_proximos <- do.call(rbind, lista_indices_proximos)

##transformar para dataframe
dados_proximos <- as.data.frame(dados_proximos)

#selecionar só os veiculos mais proximos
dados_proximos <- dados_proximos[, 1:11]

# Função para transformar números em strings usando a posição das linhas de Y
transformar_numeros_em_strings <- function(df_X, df_Y, coluna_texto) {
  # Criar uma lista de substituições usando as posições das linhas de Y como chaves
  substituicoes <- setNames(df_Y[[coluna_texto]], seq_len(nrow(df_Y)))
  
  # Aplicar a transformação em todo o dataframe X
  df_X_strings <- df_X
  for (col in colnames(df_X)) {
    df_X_strings[[col]] <- substituicoes[as.character(df_X[[col]])]
  }
  
  return(df_X_strings)
}



dados_proximos <- t(dados_proximos)
dados_proximos <- as.data.frame(dados_proximos)

dados_proximos <- transformar_numeros_em_strings(dados_proximos, Tabela_clust.plot,
                                                 "Modelo")
colnames(dados_proximos) <- dados_proximos[1,]

# Mudar formato dos valores da tabela clust
Tabela_clust$Valor <- format(Tabela_clust$Valor, 
                             decimal.mark = ',', big.mark = '.')
Tabela_clust$Valor <- paste("R$", Tabela_clust$Valor, sep = " ")



#####################################################

#mesmo grafico porem usando ggplotly para graficos com interação

g <- ggplot(Tabela_clust.plot, aes(x = PC1, y = PC2, label = Modelo)) +
  geom_point(aes(colour = Clusters)) +
  labs(x = "PC1", y = "PC2") +
  ggtitle("titulo") +
  ylab("eixo y") +
  xlab("eixo x")

library(plotly)
plotly::ggplotly(g)

## Grafico com uso de clusterizacao k-means
ggplot(Tabela_clust.plot, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = Clusters)) +
  labs(x = "PC1", y = "PC2")


