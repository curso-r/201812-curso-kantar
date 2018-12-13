library(tidyverse)
set.seed(1313)

# Criando banco de dados --------------------------------------------------

criar_amostra <- function(n, perc_teste) {
  data_frame(
    x = runif(n, 0, 20),
    y = 500 + 0.4 * (x-10)^3 + rnorm(n, sd = 50),
    is_train = 1:length(x) %in% sample.int(n, n*perc_teste)
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

df$predicao <- predict(modelo, df)
df %>% 
  group_by(is_train) %>% 
  summarise(mse = mean((predicao - y)^2))


# Ajustando um modelo mais complexo ---------------------------------------

modelo <- lm(y ~ poly(x, 50, raw = TRUE), data = df_train)

df$predicao <- predict(modelo, df)
df %>% 
  group_by(is_train) %>% 
  summarise(mse = mean((predicao - y)^2))

ggplot(df, aes(x = x, y = y, color = is_train)) +
  geom_point() +
  geom_smooth(data = df_train, method = "lm", se = FALSE, formula = y ~ poly(x, 50, raw = TRUE))

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

n_vezes <- 400
gg <- ggplot(df, aes(x = x, y = y)) +
  geom_point() 

for(i in 1:n_vezes) {
  gg <- gg + 
    geom_smooth(
      data = df %>% sample_n(50),
      alpha = 0.01,
      method = "lm", se = FALSE, 
      formula = y ~ poly(x, 40, raw = TRUE)
      )
}

gg + coord_cartesian(ylim = c(0, 1000))
  

# Qual o efeito do tamanho da amostra? -------------------------------------


# Qual o efeito do tamanho da amostra de teste? ---------------------------


# Implementando validação cruzada -----------------------------------------


# Implementando LOOC ------------------------------------------------------


