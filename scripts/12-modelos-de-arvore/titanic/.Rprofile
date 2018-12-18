library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(purrr)
library(recipes)
library(caret)
library(xgboost)
library(randomForest)
library(rpart)
library(rpart.plot)

metricas <- function(data, lev = NULL, model = NULL) {
  c(
    defaultSummary(data, lev, model), 
    twoClassSummary(data, lev, model)
  )
}

