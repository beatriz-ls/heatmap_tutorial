library(dplyr) # manipulação de dados
library(data.table) # manipulação de dados
library(readr) # baixar dados
library(ggplot2) # gráficos estáticos
library(pheatmap) # gráficos estáticos
library(heatmaply) # gráficos estáticos

#####  dados -------------------------------------------------------------------

load("data/dados_oobr_qualidados_SIVEP_Inconsistencia_2009_2023.rda")

dados <- dados_oobr_qualidados_SIVEP_Inconsistencia_2009_2023

rm(dados_oobr_qualidados_SIVEP_Inconsistencia_2009_2023)

setDT(dados)

# preparando dados com data.table

dataset_aux <- dados[ANO == 2022,
                     .(PORCENTAGEM_INCONSISTENTES = sum(INCONSISTENTES)/sum(TOTAIS)),
                     by = .(VARIAVEL, ESTADO)
]

# preparando dados com dplyr

dataset_aux <- dados %>% 
  filter(ANO == 2022) %>%
  group_by(VARIAVEL, ESTADO) %>%
  summarise(
    PORCENTAGEM_INCONSISTENTES = sum(INCONSISTENTES) / sum(TOTAIS),
    .groups = 'drop'  # remove o agrupamento após o summarise
  )

# colocaando dados em formato de matriz

matriz_dados <- reshape2::dcast(dataset_aux, VARIAVEL ~ ESTADO, value.var = "PORCENTAGEM_INCONSISTENTES")

rownames(matriz_dados) <- matriz_dados$VARIAVEL

matriz_dados <- as.matrix(matriz_dados[,-1])

##### cosntruindo mapas de calor -----------------------------------------------

#### ggplot ####

ggplot(dataset_aux, aes( x = ESTADO, y = VARIAVEL,
                         fill = PORCENTAGEM_INCONSISTENTES)) +
  geom_tile() +
  scale_fill_gradient(low = "pink",
                      high = "darkred",
                      name = "Porcentagem",
                      labels = scales::percent) +
  theme_minimal() +
  labs(title = "Inconsistências no SIVEP-Gripe em 2022 (ggplot)")


##### função heatmap da base ####

heatmap(matriz_dados,
        Rowv = NA,
        Colv = NA,
        col = colorRampPalette(c("pink", "darkred"))(100),
        scale = "none",
        #xlab = "Estado",
        # ylab = "Variável",
        main = "Inconsistências no SIVEP-Gripe em 2022 (base)"
)

#### pheatmap ####

pheatmap(matriz_dados,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         color = colorRampPalette(c("pink", "darkred"))(100),
         fontsize = 8,
         main =  "Inconsistências no SIVEP-Gripe em 2022 (pheatmap)"
)

