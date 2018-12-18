titanic_train <- read_csv("aulas/06-arvore-rf/titanic/csv/train.csv")
titanic_test <- read_csv("aulas/06-arvore-rf/titanic/csv/test.csv")


titanic <- bind_rows(
  titanic_train %>% mutate(base = "train"),
  titanic_test %>% mutate(base = "test")
) %>%
  mutate(Survived_num = Survived,
         Survived = if_else(Survived_num == 1, 'yes', 'no'),
         pronoun = str_extract(Name, ", [A-Za-z]+\\.? ") %>% str_extract("[A-Za-z]+"),
         pronoun_v2 = pronoun %>% fct_lump(5, other_level = "Rare pronoun"),
         flag_mr = if_else(pronoun_v2 %in% "Mr", 1, 0),
         flag_master = if_else(pronoun_v2 %in% "Master", 1, 0),
         flag_miss = if_else(pronoun_v2 %in% "Miss", 1, 0),
         flag_parenteses = if_else(str_detect(Name, "[\\(\\)]"), 1, 0),
         flag_quotes = if_else(str_detect(Name, '[\\"\\"]'), 1, 0),
         Embarked = if_else(is.na(Embarked), "S", Embarked),
         Pclass = (Pclass - 2)/2,
         flag_0_sibsp = as.numeric(SibSp %in% 0),
         flag_0_parch = as.numeric(Parch %in% 0),
         family_size = Parch + SibSp,
         parch_v2 = case_when(Parch == 0 ~ "a 0",
                              Parch <= 3 ~ "b 1 to 3",
                              TRUE ~ "c 4 or more"),
         Fare = if_else(is.na(Fare), log10(34), log10(Fare + 1)),
         flag_has_cabin = if_else(is.na(Cabin), 0, 1),
         ticket_prefix = str_sub(Ticket, end = -4),
         ticket_rank = cut(as.numeric(fct_reorder(ticket_prefix, Survived_num, mean, na.rm = TRUE)), breaks = 5))

# imputacao de idade

age_impute <- glm(Age ~ Sex + flag_miss + (flag_master + Fare + Parch)^3 , data = titanic, family = "Gamma")

titanic %<>%
  mutate(Age = if_else(is.na(Age), predict(age_impute, newdata = ., type = "response"), Age),
         age_status = case_when(Age - floor(Age) != 0 ~ "exact",
                                Age - floor(Age) == 0 ~ "estimated",
                                is.na(Age) ~ "unknown"),
         flag_under_age_of_6 = case_when(is.na(Age) ~ 0,
                                         Age <= 6 ~ 1,
                                         TRUE ~ 0),
         age_v2 = Age %>% scale %>% as.vector)

save(titanic, file = "aulas/06-arvore-rf/titanic/RData/titanic.RData")



