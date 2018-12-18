library(broom)
library(vtreat)
library(janitor)
library(cutpointr)
library(lubridate)
library(smbinning)
library(tidywoe)

janitor::crosstab(titanic, Pclass, Survived) %>% adorn_crosstab(show_n = TRUE)
janitor::get_dupes(titanic, Age)
janitor::tabyl(titanic, Pclass)
broom::bootstrap()

roc <- cutpointr::roc(data = titanic_predicted %>% dplyr::filter(base %in% "train"), 
                      class = "Survived", 
                      pos_class = "yes", 
                      neg_class = "no", 
                      x = "score_model02")

roc %<>% mutate(f1 = pmap_dbl(list(tp, fp, tn, fp), F1_score),
                kappa = pmap_dbl(list(tp, fp, tn, fp), cohens_kappa))

cp <- cutpointr(data = titanic_predicted %>% dplyr::filter(base %in% "train"), 
          score_model02, 
          Survived,
          pos_class = "no",
          metric = cutpointr::F1_score) %$% optimal_cutpoint %>% as.numeric

titanic_predicted %$% confusionMatrix(factor(if_else(score_model01 > cp, "yes", "no")), Survived)
(titanic_predicted %>% dplyr::count(score_model01 > cp))


p <- ggplot(roc) +
  geom_point(aes(x = x.sorted, y = tpr, group = x.sorted))

plotly::ggplotly(p)

p2 <- ggplot(roc) +
  geom_point(aes(x = fpr, y = tpr, group = x.sorted), colour = "salmon") +
  geom_point(aes(x = f1, y =tpr , group = x.sorted), colour = "royalblue")

plotly::ggplotly(p2)

mtcars %>% bootstrap(10) %>% do(tidy(lm(mpg ~ wt, .)))

vtreat::prepare()
a <- mtcars %>% bootstrap(10)


resposta <- titanic$Survived_num
teste <- titanic %>%
  map(~ smbinning(data.frame(x = .x, y = resposta), "y", "x"))
smbinning()




bins = 100
titanic_predicted %>%
  dplyr::filter(base %in% "train") %>%
  mutate(faixa = cut(score_model02, breaks = bins, labels = 1:bins) %>% as.numeric) %>%
  group_by(faixa, Survived) %>%
  summarise(n = n()) %>%
  group_by(faixa) %>%
  mutate(n_tot = sum(n),
         p = n/n_tot) %>%
  dplyr::filter(Survived %in% "yes") %>%
  ungroup %>%
  arrange(faixa %>% desc) %>%
  mutate(p_acum = cumsum(n)/sum(n),
         p = n/sum(n)) %>%
  ggplot() +
  geom_point(aes(x = faixa, y = p_acum)) +
  ylim(c(0,1))















titanic %>% 
  dplyr::filter(base %in% "train") %>% 
  count(Parch, Survived) %>% 
  spread(Survived, n) %>%
  mutate(p1 = yes/(no + yes)) 

titanic %>% 
  dplyr::filter(base %in% "train") %>% 
  ggplot() +
  stat_summary(aes(x = Fare, y = as.numeric(as.factor(Survived)) - 1), method = "glm", method.args = list(family = "binomial"), fun.y = "mean", geom = "point")


titanic %>% summarise_all(~sum(is.na(.x))) %>% t


