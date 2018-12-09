library(tidyverse)
library(skimr)

# Dados --------------------------------------------------------------------

dados_jar <- haven::read_spss(
  "dados/penalty/CPT_Avatar_SPSS_reduzido_ADULTO.sav"
)
View(dados_jar)

dados_jar_emp <- haven::read_spss(
  "dados/penalty/CPT_Avatar_SPSS_reduzido_ADULTO_EMPILHADO.sav"
)
View(dados_jar_emp)

names(dados_jar)[!names(dados_jar) %in% names(dados_jar_emp)]
names(dados_jar_emp)[!names(dados_jar_emp) %in% names(dados_jar)]

glimpse(dados_jar_emp)

dados_jar_emp <- dados_jar_emp %>% 
  mutate_all(funs(as.numeric))


# Descritiva ---------------------------------------------------------------

dados_jar_emp %>% 
  select(SABOR:PURCHASE) %>%
  mutate_all(funs(as.character)) %>% 
  skim() %>% 
  kable(format = "html")
  

# Dados --------------------------------------------------------------------

dados_jar_emp %>% 
  ggplot(aes(x = OL_OVERALL)) +
  geom_bar(fill = "orange", color = "black") +
  geom_text(aes(y = ..count.. + 10, label = ..count..), stat = "count") + 
  labs(y = "Frequência") +
  theme_bw()
