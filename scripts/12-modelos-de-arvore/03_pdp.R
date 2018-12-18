variavel <- "y"

range_var <- diamantes_ok$y %>% range
seq_var <- seq(range_var[1], range_var[2], length.out = 5)

diamantes_ok_cenarios_pdp <- diamantes_ok %>%
  sample_n(300) %>%
  rownames_to_column("id") %>%
  select(-y) %>%
  crossing(
    y = seq_var
  ) %>%
  mutate(
    pred_rpart = predict(modelo_rpart, newdata = .)
    # INCLUA AS PREVISOES DO XGOOST AQUI
  ) 

## Gráfico

diamantes_ok_cenarios_pdp %>%
  ggplot(aes(x = y, y = pred_rpart)) +
  geom_line(aes(group = id), alpha = 0.5) +
  # geom_point(position = position_jitter(0)) +
  stat_summary(colour = "red", geom = "line", fun.y = mean)