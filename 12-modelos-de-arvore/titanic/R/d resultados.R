load("aulas/06-arvore-rf/titanic/RData/titanic_predicted.RData")

# sumario
resultados <- titanic_predicted %>%
  filter(base %in% "train") %>% # o meu verdadeiro testador é o Kaggle neste caso. Senão eu deixaria o "test"
  mutate(base = if_else(runif(n()) > 0.7, "test", "train")) %>% # só de mentirinha para ilustrar.
  select(PassengerId, base, Survived, Survived_num, starts_with("score")) %>%
  gather(modelo, score, starts_with("score")) %>%
  group_by(base, modelo) %>%
  nest %>%
  mutate(
    roc = map(data, ~ .x %$% AUC::roc(score, factor(Survived_num))),
    auc = map_dbl(roc, AUC::auc),
    ks = map_dbl(data, ~ .x %$% ks(score, Survived))
  )

resultados

# graficos para o chefe
resultados %>%
  select(-data, -roc) %>%
  gather(metrica, valor, auc, ks) %>%
  ggplot(aes(x = modelo, y = valor, colour = base, fill = base)) +
  # geom_col(position = "dodge") +
  geom_point() +
  geom_line(aes(group = base)) +
  facet_wrap(~metrica, scales = "free")
