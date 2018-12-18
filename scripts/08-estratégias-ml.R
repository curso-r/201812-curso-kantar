library(tidyverse)
set.seed(1313)

# Criando banco de dados --------------------------------------------------

criar_amostra <- function(n, perc_treino) {
  data_frame(
    x = runif(n, 0, 20),
    y = 500 + 0.4 * (x-10)^3 + rnorm(n, sd = 50),
    is_train = 1:n %in% sample.int(n, n*perc_treino)
  )
}

df <- criar_amostra(100, 0.5)

df_train <- df %>% filter(is_train)
df_test <- df %>% filter(!is_train)

ggplot(df, aes(x = x, y = y, color = is_train)) +
  geom_point() +
  geom_smooth(data = df_train, method = "lm", se = FALSE)

# Ajustando o primeiro modelo ---------------------------------------------

modelo <- lm(y ~ x, data = df_train)
summary(modelo)

df$predicao <- predict(modelo, df)
df %>% 
  group_by(is_train) %>% 
  summarise(rmse = sqrt(mean((predicao - y)^2)))


# Ajustando um modelo mais complexo ---------------------------------------

ggplot(df, aes(x = x, y = y, color = is_train)) +
  geom_point() +
  geom_smooth(data = df_train, method = "lm", se = FALSE, 
              formula = y ~ poly(x, 50, raw = TRUE))


modelo <- lm(y ~ poly(x, 50, raw = TRUE), data = df_train)

df$predicao <- predict(modelo, df)
df %>% 
  group_by(is_train) %>% 
  summarise(rmse = sqrt(mean((predicao - y)^2)))


# Ajustando diversos modelos ----------------------------------------------

ajustar_polinomios <- function(df, grau) {
  
  erros <- NULL
  for(g in grau) {
    
    modelo <- lm(y ~ poly(x, g, raw = TRUE), data = df %>% filter(is_train))
    
    df$predicao <- predict(modelo, df)
    erros <- bind_rows(
      erros,
      df %>% 
        group_by(is_train) %>% 
        summarise(mse = mean((predicao - y)^2)) %>% 
        mutate(grau = g)
    )
    
  }
  erros
}

erros <- ajustar_polinomios(df, 1:50)
ggplot(erros, aes(x = grau, y = mse)) + 
  geom_line() + 
  geom_point(
    data = erros %>% group_by(is_train) %>% filter(mse == min(mse)),
    size = 3
    ) +
  facet_wrap(~is_train) 


# Por que variância alta?

n_vezes <- 100
df <- criar_amostra(10000, 0.5)
gg <- ggplot(df, aes(x = x, y = y)) +
  geom_point() 

for(i in 1:n_vezes) {
  gg <- gg + 
    geom_smooth(
      data = df %>% sample_n(50),
      alpha = 0.01,
      color = "red",
      method = "lm", se = FALSE, 
      formula = y ~ poly(x, 20, raw = TRUE)
      )
}

gg + coord_cartesian(ylim = c(0, 1000))
  

# Qual o efeito do tamanho da amostra? -------------------------------------

df <- criar_amostra(1000, 0.5)
erros <- ajustar_polinomios(df, seq(1, 200, by = 5))
ggplot(erros, aes(x = grau, y = mse)) + 
  geom_line() + 
  geom_point(
    data = erros %>% group_by(is_train) %>% filter(mse == min(mse)),
    size = 3
  ) +
  facet_wrap(~is_train) 


# Qual o efeito do tamanho da amostra de teste? ---------------------------

df <- criar_amostra(1000, 0.99)
erros <- ajustar_polinomios(df, seq(1, 200, by = 5))
ggplot(erros, aes(x = grau, y = mse)) + 
  geom_line() + 
  geom_point(
    data = erros %>% group_by(is_train) %>% filter(mse == min(mse)),
    size = 3
  ) +
  facet_wrap(~is_train) 

# Implementando validação cruzada -----------------------------------------

df <- criar_amostra(100, 0.5)
k <- 100
df$fold <- 1:nrow(df)
erros <- NULL
for (i in 1:k) {
  df$is_train <- df$fold != i
  erro <- ajustar_polinomios(df, grau = 1:50)
  erro$fold <- i
  erros <- bind_rows(erros, erro)
}

erros2 <- erros %>% 
  group_by(grau, is_train) %>% 
  summarise(
    media = median(mse),
    minimo = min(mse),
    maximo = max(mse),
    quantil95 = quantile(mse, probs = 0.95),
    quantil5 = quantile(mse, probs = 0.05)
    )

ggplot(erros2, aes(x = grau, y = media)) + 
  geom_line() + 
  geom_point(
    data = erros2 %>% group_by(is_train) %>% filter(media == min(media)),
    size = 3
  ) +
  facet_wrap(~is_train) +
  #coord_cartesian(ylim = c(0, 5000)) +
  geom_ribbon(aes(ymin = quantil5, ymax = quantil95), alpha = 0.2)


# Implementando LOOC ------------------------------------------------------


