# Importação --------------------------------------------------------------

# Caminhos

# 1. Podem ser absolutos

read_csv("/home/william/Documents/Curso-R/201809-intro-r/data/imdb.csv")

# 2. Podem ser relativos ao diretório de trabalho

getwd()

read_csv("data/imdb.csv")

# Arquivos de texto

library(readr)

imdb <- read_csv(file = "dados/imdb.csv")
imdb2 <- read_delim("dados/imdb2.csv", delim = ";")

# Excel

library(readxl)

imdb_excel <- read_excel("dados/imdb.xlsx")

# Outros formatos

library(jsonlite)

imdb_json <- read_json("dados/imdb.json")

library(haven)

imdb_sas <- read_sas("dados/imdb.sas7bdat")
imdb_spss <- read_spss("dados/imdb.sav")
