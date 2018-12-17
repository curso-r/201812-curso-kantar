load("aulas/06-arvore-rf/titanic/RData/titanic.RData")
load("aulas/06-arvore-rf/titanic/RData/model01.RData")
load("aulas/06-arvore-rf/titanic/RData/model02.RData")

titanic_predicted <- titanic %>% 
  mutate(score_model01 = predict(model01, newdata = ., type = "prob")$yes,
         score_model02 = predict(model02, newdata = ., type = "prob")$yes)



# Stacking
modelXX <- glm(factor(Survived) ~ score_model01 + score_model02,
               data = titanic_predicted %>% dplyr::filter(base %in% "train"),
               family = binomial)

titanic_predicted %<>%
  mutate(score_modelXX = predict(modelXX, newdata = ., type = "response"))

save(titanic_predicted, file = "aulas/06-arvore-rf/titanic/RData/titanic_predicted.RData")
