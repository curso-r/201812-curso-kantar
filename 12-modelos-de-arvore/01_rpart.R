# pacotes e funcoes auxiliares -------------------------------------#
library(tidyverse)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(recipes)
library(magrittr)

gini <- function(.data, .resposta, .pergunta) {
  .resposta <- enquo(.resposta)
  .pergunta <- enquo(.pergunta)
  
  .data %>% 
    count(!!.resposta, !!.pergunta) %>% 
    group_by(!!.pergunta) %>%
    mutate(p = n/sum(n)) %>%
    summarise(
      n = sum(n),
      gini = 1 - sum(p^2)
    ) %$% 
    sum(n * gini)
}


# dados de mentira ------------------------------------------------------------------#
pacientes <- tribble(
  ~paciente, ~pressao, ~glicose, ~diabetes,
  "Alfredo", "hipertensao",  92, "nao",
  "Beatriz", "normal",      130, "sim",
  "Carla",   "normal",      130, "nao",
  "Daniela", "normal",       55, "nao",
  "Ernesto", "hipertensao", 220, "sim",
  "Flavia",  "normal",      195, "sim",
)


# ajuste de uma arvore de decisao usando rpart ------------------------------------------#
arvore_rpart <- rpart(
  diabetes ~ pressao + glicose, 
  data = pacientes, 
  control = rpart.control(
    minsplit = 1, 
    minbucket = 1, 
    cp = -1, 
    maxdepth = 10
  ),
  parms = list(split = "gini")
)

arvore_rpart

rpart.plot(arvore_rpart)
arvore_rpart$cptable
arvore_rpart$splits
predict(arvore_rpart)

# grafico de CP -----------------------------------------------------------------------#
grafico_de_cp(arvore_rpart)





# rpart no CARET -----------------------------------------------------------------------#
# info_rpart <- caret::getModelInfo("rpart", FALSE)
train_control <- trainControl( 
  method = "cv",
  number = 3,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
)

# ajuste de uma arvore de decisao usando rpart no CARET --------------------------------#
# info_rpart <- caret::getModelInfo("rpart", FALSE)
arvore_caret <- train(
    diabetes ~ pressao + glicose,
    data = pacientes,
    method = "rpart",
    control = rpart.control(minsplit = 2, minbucket = 1, maxdepth = 10),
    trControl = train_control,
    tuneLength = 5,
    metric = "ROC"
  )

plot(arvore_caret)
arvore_caret

arvore_caret$finalModel
rpart.plot(arvore_caret$finalModel)
arvore_caret$finalModel$cptable
arvore_caret$finalModel$splits
predict(arvore_caret$finalModel)



























# primeira divisao da arvore feita na mao -------------------------------------------#
primeira_decisao <- pacientes %>%
  mutate(
    p1 = glicose < 73.5,
    p2 = glicose < 111,
    p3 = glicose < 162.5,
    p4 = glicose < 207.5,
    p5 = pressao == "normal"
  ) %>%
  gather(pergunta, flag_pergunta, matches("p[0-9]$")) %>%
  group_by(pergunta) %>%
  nest %>%
  mutate(
    impureza_folhas = map_dbl(data, ~ .x %>% gini(diabetes, flag_pergunta)),
    impureza_no = data[[1]] %>% gini(diabetes, 1),
    ganho_de_informacao = impureza_no - impureza_folhas
  )


