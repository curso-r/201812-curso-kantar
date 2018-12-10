library(tidyverse)
library(skimr)

# Dados --------------------------------------------------------------------

dados_seg <- haven::read_spss(
  "dados/segmentacao/Strategy_FINAL_com peso_S1_v2.sav"
)
View(dados_seg)

glimpse(dados_seg)

dados_seg <- dados_seg %>% 
  mutate_all(funs(as.numeric))

# Descritiva ---------------------------------------------------------------

dados_seg %>% 
  skim()


# Kohonen ------------------------------------------------------------------
#(Self-Organising Maps - SOM)


# Selecionando apenas as variáveis de segmentação
dados_treino <- dados_seg %>% 
  select(starts_with("S1_"))

# Padronizando as variáveis
dados_treino <- scale(dados_treino)

# Criando o grid
som_grid <- kohonen::somgrid(xdim = 25, ydim = 25, topo = "hexagonal")

# Treinando o modelo
modelo <- kohonen::som(
  X = dados_treino,
  grid = som_grid,
  rlen = 300,
  alpha = c(0.05, 0.01)
)

## GRÁFICOS

# Treino
plot(modelo, type = "changes")

# Tamanho dos nós
plot(modelo, type = "count")

# Distância dos clusters
plot(modelo, type = "dist.neighbours")

# Heatmap
plot(
  modelo, 
  type = "property", 
  property = modelo$codes[[1]][, 5],
  main = colnames(modelo$data[[1]])[5]
)

plot_heatmap <- function(modelo, id_coluna) {
  
  plot(
    modelo, 
    type = "property", 
    property = modelo$codes[[1]][, id_coluna],
    main = colnames(modelo$data[[1]])[id_coluna]
  )
  
}

plot_heatmap_grid <- function(modelo, colunas, nrow, ncol) {
  par(mfrow = c(nrow, ncol))
  walk(colunas, plot_heatmap, modelo = modelo)
}

plot_heatmap_grid(modelo, 1:12, 4, 3)

### Clustering

# Decidindo o número de grupos
dados_modelo <- modelo$codes[[1]]
SS <- (nrow(dados_modelo)-1)*sum(apply(dados_modelo, 2, var)) 
for (i in 2:15) {
  SS[i] <- sum(kmeans(dados_modelo, centers=i)$withinss)
}
plot(wss, type = "l")

# Clusterização hierárquica
som_cluster <- cutree(hclust(dist(modelo$codes[[1]])), 5)

# Gráfico
cores <- case_when(
  som_cluster == 1 ~ "purple",
  som_cluster == 2 ~ "red",
  som_cluster == 3 ~ "green",
  som_cluster == 4 ~ "yellow",
  som_cluster == 5 ~ "pink"
)

plot(
  modelo, 
  type = "mapping",
  main = "Clusters",
  bgcol = cores,
  col = NULL,
) 
kohonen::add.cluster.boundaries(modelo, som_cluster)

# Comparando com os grupos anteriores

classificacao <- tibble(
  id = 1:nrow(dados_seg),
  no = modelo$unit.classif
)

classificacao <- tibble(
  no = 1:length(som_cluster),
  cluster = as.numeric(som_cluster)
) %>% 
  right_join(classificacao, by = "no")

dados_seg %>% 
  mutate(novos_grupos = classificacao$cluster) %>% 
  select(GRUPO, novos_grupos) %>% 
  count(GRUPO, novos_grupos) %>% 
  spread(novos_grupos, n) %>% 
  View
