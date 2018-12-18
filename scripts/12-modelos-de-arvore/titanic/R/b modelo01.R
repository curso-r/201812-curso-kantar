
load("aulas/06-arvore-rf/titanic/RData/titanic.RData")



train_control_model01 <- trainControl(
   method = "cv"
  ,number = 5
  ,verboseIter = TRUE
  ,classProbs = TRUE
  ,summaryFunction = metricas
  ,allowParallel = FALSE
)

tune_grid_model01 <- expand.grid(
  lambda = c(0, 2^runif(50, min = -10, 1)),
  alpha = c(0,1)
)

model01 <- train(
  Survived ~ 1
  
  + (flag_quotes
  + flag_mr
  + flag_miss
  + flag_master
  + flag_parenteses
  + Sex
  + age_status
  + Pclass
  + SibSp
  + parch_v2
  + Fare
  + flag_has_cabin
  + age_v2
  + I(age_v2^2))^2
  
  ,data = titanic %>% dplyr::filter(base %in% "train")
  
  , method = "glmnet"
  , preProcess = NULL
  , metric = "ROC"
  , trControl = train_control_model01
  , tuneGrid = tune_grid_model01
)

save(model01, file = "aulas/06-arvore-rf/titanic/RData/model01.RData")
