load("aulas/06-arvore-rf/titanic/RData/titanic.RData")


train_control_model02 <- trainControl(
  method = "cv"
  ,number = 5
  ,verboseIter = TRUE
  ,classProbs = TRUE
  ,summaryFunction = metricas
  ,allowParallel = FALSE
)


tune_grid_model02 <- data.frame(
  mtry = c(2, 4, 6, 8, 10, 14)
)

model02 <- train(
  Survived ~ 1
  
  + flag_quotes
  + flag_mr
  + flag_miss
  + flag_master
  + flag_0_sibsp
  + flag_0_parch
  + pronoun_v2
  + flag_parenteses
  + Sex
  + age_status
  + factor(Pclass)
  # + flag_1_sibsp
  + SibSp
  + parch_v2
  + family_size
  + Fare
  + flag_has_cabin
  + age_v2
  + Embarked
  
  ,data = titanic %>% dplyr::filter(base %in% "train")
  
  , method = "rf"
  , preProcess = NULL
  , metric = "ROC"
  , trControl = train_control_model02
  , tuneGrid = tune_grid_model02
)

save(model02, file = "aulas/06-arvore-rf/titanic/RData/model02.RData")
