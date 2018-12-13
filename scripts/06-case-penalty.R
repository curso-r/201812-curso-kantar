library(tidyverse)
library(skimr)

# install.packages("skimr")

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

glimpse(dados_jar_emp)

# Descritiva ---------------------------------------------------------------

dados_jar_emp %>% 
  select(SABOR:PURCHASE, CELA) %>%
  #mutate_all(funs(as.factor)) %>% 
  group_by(CELA) %>% 
  skim()
  

# Dados --------------------------------------------------------------------

dados_jar_emp %>% 
  ggplot(aes(x = OL_OVERALL)) +
  geom_bar(fill = "orange", color = "black") +
  geom_text(aes(y = ..count.. + 10, label = ..count..), stat = "count") + 
  facet_wrap(~CELA)  +
  labs(y = "Frequência") +
  theme_bw()
