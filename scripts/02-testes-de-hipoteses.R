# Teste de médias ---------------------------------------------------------

mtcars
View(mtcars)
help(mtcars)

t.test(mpg ~ am, mtcars)

teste <- t.test(mpg ~ am, mtcars)

names(teste)
teste$p.value


# Teste de proporções -----------------------------------------------------

data("credit_data", package = "recipes")

View(credit_data)

tabela <- table(credit_data$Records, credit_data$Status)
  
prop.test(tabela)

tabela <- table(credit_data$Marita, credit_data$Status)
prop.test(tabela)


# Teste qui-quadrado ------------------------------------------------------

# Aderência

titanic <- as.tibble(datasets::Titanic)

tabela <- c(
  sum(titanic$n[titanic$Sex == "Male"]),
  sum(titanic$n[titanic$Sex == "Female"])
)

chisq.test(tabela)
chisq.test(tabela, p = c(0.8, 0.2))

# Contingência

tabela <- table(credit_data$Records, credit_data$Status)

chisq.test(tabela)

# Teste de Dunnett --------------------------------------------------------

# Queremos comparar vários tratamentos com um único controle

library(multcomp)

dados <- as.data.frame(datasets::ChickWeight)
View(dados)

dados_desfecho <- dados[dados$Time == 18, ]

ajuste <- aov(weight ~ Diet, dados_desfecho)

Dunnet <- glht(ajuste, linfct=mcp(Diet="Dunnett"))
summary(Dunnet)


# Teste de Tukey ---------------------------------------------------------

Tukey <- TukeyHSD(ajuste)
Tukey

# Teste de correlação -----------------------------------------------------

data("diamonds", package = "ggplot2")

View(diamonds)

cor(diamonds$carat, diamonds$price)
cor.test(~ carat + price, data = diamonds)
