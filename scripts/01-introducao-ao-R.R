# Nome dos objetos/variáveis ----------------------------------------------

# Os nomes devem começar com uma letra. Podem conter letras, números, _ e .

eu_uso_snake_case
outrasPessoasUsamCamelCase
algumas.pessoas.usam.pontos
E_algumasPoucas.Pessoas_RENUNCIAMconvenções

# Criando objetos/variáveis -----------------------------------------------

obj <- 1
obj

2 -> obj

# ATALHO para rodar o código: CTRL + ENTER
# ATALHO para a <- : ALT - (alt menos)

# O R difencia minúscula de maiúscula!

a <- 5
A <- 42

# Vetores -----------------------------------------------------------------

# Vetores são conjuntos ordenados de números

c(1, 4, 3, 10)

1:10

# Subsetting

vetor <- c(4, 8, 15, 16, 23, 42)

vetor[1]
vetor[c(1, 3)]
vetor[-5]
vetor[-c(1, 3)]

# Tipos -------------------------------------------------------------------

# Numéricos (numeric)

a <- 10
class(a)

# Caracteres (character, strings)

obj <- "a"
obj2 <- "masculino"

class(obj)

# Bases (data.frame)

mtcars
tibble::as_tibble(mtcars)

class(mtcars)
class(tibble::as_tibble(mtcars))

# Pacotes -----------------------------------------------------------------

# Para instalar pacotes

install.packages(c("tidyverse", "rmarkdown", "devtools"))
devtools::install_github("rstudio/flexdashboard")

dplyr::select()

# Para carregar pacotes

library(dplyr)
library(ggplot2)

sd
help(min)
?median


# Funções -----------------------------------------------------------------

seq(to = 10, from = 1, by = 2)
seq(1, 10, 2)

mean(seq(1, 10, 2))

y <- seq(1, 10, length.out = 5)
y

rm(y)
y

minha_soma <- function(x, y) {
  
  x + y
  
}

soma <- 50

minha_soma2 <- function(x, y) {
  
  x <- x^2
  y <-y^2
  
  soma <- x + y
  
  return(soma)
  
}

minha_soma2(1, 2)

minha_soma(x = 1, y = 2)
minha_soma(1, 2)


# Valores especiais -------------------------------------------------------

# Existem valores reservados para representar dados faltantes, 
# infinitos, e indefinições matemáticas.

NA   # (Not Available) significa dado faltante/indisponível. 

NaN  # (Not a Number) representa indefinições matemáticas, como 0/0 e log(-1). 
# Um NaN é um NA, mas a recíproca não é verdadeira.

Inf  # (Infinito) é um número muito grande ou o limite matemático, por exemplo, 
# 1/0 e 10^310. Aceita sinal negativo -Inf.

NULL # representa a ausência de informação.

# Use as funções is.na(), is.nan(), is.infinite() e is.null() 
# para testar se um objeto é um desses valores.

1 == NA

NA == 1


"b" == "a"

idade_joao <- 30

idade_maria <- NA

idade_joao == idade_maria

!is.na(idade_maria)
!is.na(idade_joao)

mean(c(idade_maria, idade_joao), na.rm = TRUE)

class(1L)
1L

# Identação ---------------------------------------------------------------

funcao_com_muitos_argumentos(
  argumento_1 = 10, 
  argumento_2 = 14, 
  argumento_3 = 30, 
  argumento_4 = 11
)

# ATALHO: CTRL+I

# Pipe (%>%) --------------------------------------------------------------

# Receita de bolo sem pipe. Tente entender o que é preciso fazer.

esfrie(
  asse(
    coloque(
      bata(
        acrescente(
          recipiente(
            rep(
              "farinha", 
              2
            ), 
            "água", "fermento", "leite", "óleo"
          ), 
          "farinha", até = "macio"
        ), 
        duração = "3min"
      ), 
      lugar = "forma", tipo = "grande", untada = TRUE
    ), 
    duração = "50min"
  ), 
  "geladeira", "20min"
)

# Veja como o código acima pode ser reescrito utilizando-se o pipe. 
# Agora realmente se parece com uma receita de bolo.

recipiente(rep("farinha", 2), "água", "fermento", "leite", "óleo") %>%
  acrescente("farinha", até = "macio") %>%
  bata(duração = "3min") %>%
  coloque(lugar = "forma", tipo = "grande", untada = TRUE) %>%
  asse(duração = "50min") %>%
  esfrie("geladeira", "20min")

# ATALHO: CTRL + SHIFT + M


# Controles de fluxo ------------------------------------------------------

x <- 0

if(x < 0) {
  "negativo"
} else if(x == 0) {
  "neutro"
} else {
  "positivo"
}

x <- -10:30

x_categorizado <- ifelse(x < 0, "negativo", "positivo")

a <- 1:3
b <- 4:9

a + 1

class(c(1, 2, 3))

c(1, 2, 3, "a")
c(TRUE, FALSE, "a")
c(1L, 2L, "a")
c(TRUE, FALSE, 1)

coluna <- c(1, 2, 3, NA, 3, 10, NA)

as.numeric(is.na(coluna))
as.numeric("10")

sum(is.na(coluna))

logico < inteiro < numerico < caracter

# R Markdown --------------------------------------------------------------

# Usado para gerar relatórios, apresentações e dashboards estáticos

