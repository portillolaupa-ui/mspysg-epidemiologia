library(tidyverse)
library(readxl)
library(psych)

#=========================================================
#Importación de la base de datos

data <- read_excel("data/kuan-etal-plosone-2015.xlsx")
diccionario <- read_excel("data/diccionario_variables.xlsx")

#=========================================================
#Exploración de la base de datos

str(data)             #vista general
glimpse(data)         #vista general avanzada

dim(data)             #filasxcolumnas
colnames(data)        #nombre de columnas

summary(data)         #estadisticos descriptivos x variable
describe(data)        #estadísticos descriptivos x tabla

sapply(data, class)   #tipos de variables
colSums(is.na(data))  #número de valores nulos

sapply(data,unique)   #valores únicos x variable

data_valores_unicos <- data.frame(
  valores_Unicos = sapply(data, function(x) length(unique(na.omit(x))))
)
data_valores_unicos   #número de valores únicos x tabla