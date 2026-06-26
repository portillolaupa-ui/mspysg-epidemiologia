install.packages(c(
  "tidyverse",
  "readxl",
  "gtsummary",
  "epiR",
  "psych",
  "broom"
))

library(tidyverse)
library(readxl)
library(gtsummary)
library(epiR)
library(psych)
library(broom)

data <- read_excel("data/kuan-etal-plosone-2015.xlsx")

#=========================================================
str(data)
dim(data)
colnames(data)
summary(data)
#psych::describe(data)

sapply(data, class)
colSums(is.na(data))
sapply(data, dplyr::n_distinct)
sapply(data,unique)

#capture.output(str(data),file = "outputs/str.txt")
#find("colSums")

#=========================================================
data$Sex <- factor(
  data$Sex,
  levels=c(1,2),
  labels=c("Male","Female")
)

data$Survival <- factor(
  data$Survival,
  levels = c(0,1),
  labels = c("Absence", "Presence")
)

data$Recurrence <- factor(
  data$Recurrence,
  levels = c(0,1),
  labels = c("Absence","Presence")
)

data$stage_simple <- factor(
  data$stage_simple,
  levels = c(1,2,3,4),
  labels = c("I","II","III","IV")
)

table(data$Sex)
table(data$Survival)
table(data$Recurrence)
table(data$stage_simple)

#=========================================================
table(data$chemT, useNA = "ifany")
table(data$location, useNA = "ifany")

#=========================================================
data$CDKN2A_N <- factor(data$CDKN2A_N,
                        levels = c(0,1),
                        labels = c("Unmethylated","Methylated"))

data$CDKN2A_T <- factor(data$CDKN2A_T,
                        levels = c(0,1),
                        labels = c("Unmethylated","Methylated"))

data$MGMT_N <- factor(data$MGMT_N,
                      levels = c(0,1),
                      labels = c("Unmethylated","Methylated"))

data$MGMT_T <- factor(data$MGMT_T,
                      levels = c(0,1),
                      labels = c("Unmethylated","Methylated"))

data$MLH1_N <- factor(data$MLH1_N,
                      levels = c(0,1),
                      labels = c("Unmethylated","Methylated"))

data$MLH1_T <- factor(data$MLH1_T,
                      levels = c(0,1),
                      labels = c("Unmethylated","Methylated"))

table(data$CDKN2A_N)
table(data$CDKN2A_T)
table(data$MGMT_N)
table(data$MGMT_T)
table(data$MLH1_N)
table(data$MLH1_T)

#=========================================================
niveles_met <- c(
  "UnMe / Local (I-II)",
  "UnMe / Advanced (III-IV)",
  "Me / Local (I-II)",
  "Me / Advanced (III-IV)"
)

data$p16N <- factor(data$p16N, levels = 1:4, labels = niveles_met)
data$p16T <- factor(data$p16T, levels = 1:4, labels = niveles_met)
data$ml_N <- factor(data$ml_N, levels = 1:4, labels = niveles_met)
data$ml_T <- factor(data$ml_T, levels = 1:4, labels = niveles_met)
data$mg_N <- factor(data$mg_N, levels = 1:4, labels = niveles_met)
data$mg_T <- factor(data$mg_T, levels = 1:4, labels = niveles_met)

table(data$p16N)
table(data$p16T)
table(data$ml_N)
table(data$ml_T)
table(data$mg_N)
table(data$mg_T)

#=========================================================
data$stage_simple_2 <- factor(
  data$stage_simple_2,
  levels = c(1, 2),
  labels = c("Local (I-II)", "Advanced (III-IV)"),
  ordered = TRUE
)

table(data$CDKN2A_N, data$stage_simple_2)
table(data$CDKN2A_T, data$stage_simple_2)
table(data$MLH1_N, data$stage_simple_2)
table(data$MLH1_T, data$stage_simple_2)
table(data$MGMT_N, data$stage_simple_2)
table(data$MGMT_T, data$stage_simple_2)

#=========================================================
table(data$all_gene_N)

table(
  CDKN2A_N = data$CDKN2A_N,
  MLH1_N = data$MLH1_N,
  MGMT_N = data$MGMT_N,
  all_gene_N = data$all_gene_N
)

table(
  Combined_N = data$Combined_N,
  stage_simple_2 = data$stage_simple_2,
  all_gene_N = data$all_gene_N
)

table(data$CDKN2A_MGMT_N)

table(data$Combined_N)

table(data$Combined_2_N)

table(data$all_gene_N_2vs1, useNA = "ifany")

table(data$all_gene_N_3vs1, useNA = "ifany")

table(data$all_gene_N_23vs1, useNA = "ifany")

table(data$all_gene_N_4vs1, useNA = "ifany")

table(
  all_gene_N = data$all_gene_N,
  all_gene_N_2vs1 = data$all_gene_N_2vs1,
  useNA = "ifany"
)

table(
  all_gene_N = data$all_gene_N,
  all_gene_N_3vs1 = data$all_gene_N_3vs1,
  useNA = "ifany"
)

table(
  all_gene_N = data$all_gene_N,
  all_gene_N_23vs1 = data$all_gene_N_23vs1,
  useNA = "ifany"
)

table(
  all_gene_N = data$all_gene_N,
  all_gene_N_4vs1 = data$all_gene_N_4vs1,
  useNA = "ifany"
)

table(data$Combined_N)
table(data$Combined_T)
table(data$Combined_2_N)
table(data$Combined_2_T)
table(data$all_gene_N)
table(data$all_gene_T)
table(data$all_gene_2_N)
table(data$all_gene_2_T)

table(data$CDKN2A_N,
      data$MLH1_N,
      data$MGMT_N)



table(data$grade, useNA = "ifany")
table(data$Follow_up_statifi_24mon)
table(data$Follow_up_statifi_36mon)


subset(
  data,
  Combined_N == 1 &
    stage_simple_2 == "Advanced (III-IV)" &
    all_gene_N == 2,
  select = c(
    Serial_No,
    CDKN2A_N,
    MLH1_N,
    MGMT_N,
    Combined_N,
    all_gene_N,
    stage_simple,
    stage_simple_2
  )
)
#=========================================================
diccionario <- data.frame(
  Variable = names(data),
  Tipo_R = sapply(data, class),
  Valores_Unicos = sapply(data, dplyr::n_distinct),
  Descripcion = NA,
  Tipo_Epidemiologico = NA,
  Codificacion = NA
)

View(diccionario)

#=========================================================
documentar <- function(diccionario,
                       variable,
                       descripcion,
                       tipo_epi,
                       codificacion){
  
  diccionario[
    diccionario$Variable == variable,
    c("Descripcion",
      "Tipo_Epidemiologico",
      "Codificacion")
  ] <- list(
    descripcion,
    tipo_epi,
    codificacion
  )
  
  return(diccionario)
}

#=========================================================
diccionario <- documentar(diccionario, "Sex", "Sexo biológico del participante", "Cualitativa nominal dicotómica", "1 = Male; 2 = Female")
diccionario <- documentar(diccionario, "Survival", "Mortalidad", "Cualitativa nominal dicotómica", "0 = Absence; 1 = Presence")
diccionario <- documentar(diccionario, "Recurrence", "Recurrencia", "Cualitativa nominal dicotómica", "0 = Absence; 1 = Presence")
diccionario <- documentar(diccionario, "stage_simple", "Estadio clínico", "Cualitativa ordinal", "1 = I; 2 = II; 3 = III; 4 = IV")
diccionario <- documentar(diccionario, "Follow_up_month", "Tiempo de seguimiento", "Cuantitativa continua", "Meses")
diccionario <- documentar(diccionario, "chemT", "Quimioterapia adyuvante", "Cualitativa nominal dicotómica", "0 = No; 1 = Sí")
diccionario <- documentar(diccionario, "location", "Localización del tumor", "Cualitativa nominal politómica", "1–6 = Categorías de localización del tumor")
diccionario <- documentar(diccionario, "CDKN2A_N", "Metilación del gen p16/CDKN2A en tejido normal", "Cualitativa nominal dicotómica", "0 = Unmethylated; 1 = Methylated")

diccionario <- documentar(diccionario, "CDKN2A_N", "Estado de metilación del promotor del gen CDKN2A (p16) en tejido normal", "Cualitativa nominal dicotómica", "0 = Unmethylated; 1 = Methylated")
diccionario <- documentar(diccionario, "CDKN2A_T", "Estado de metilación del promotor del gen CDKN2A (p16) en tejido tumoral", "Cualitativa nominal dicotómica", "0 = Unmethylated; 1 = Methylated")
diccionario <- documentar(diccionario, "MLH1_N", "Estado de metilación del promotor del gen MLH1 en tejido normal", "Cualitativa nominal dicotómica", "0 = Unmethylated; 1 = Methylated")
diccionario <- documentar(diccionario, "MLH1_T", "Estado de metilación del promotor del gen MLH1 en tejido tumoral", "Cualitativa nominal dicotómica", "0 = Unmethylated; 1 = Methylated")
diccionario <- documentar(diccionario, "MGMT_N", "Estado de metilación del promotor del gen MGMT en tejido normal", "Cualitativa nominal dicotómica", "0 = Unmethylated; 1 = Methylated")
diccionario <- documentar(diccionario, "MGMT_T", "Estado de metilación del promotor del gen MGMT en tejido tumoral", "Cualitativa nominal dicotómica", "0 = Unmethylated; 1 = Methylated")

diccionario <- documentar(diccionario, "stage_simple_2", "Estadio clínico agrupado", "Cualitativa ordinal dicotómica", "1 = Local stage (I–II); 2 = Advanced stage (III–IV)")
diccionario <- documentar(diccionario, "p16N", "Estado combinado del promotor del gen CDKN2A (p16) en tejido normal y estadio clínico agrupado", "Cualitativa nominal politómica", "1 = Unmethylated + Local; 2 = Unmethylated + Advanced; 3 = Methylated + Local; 4 = Methylated + Advanced")
diccionario <- documentar(diccionario, "p16T", "Estado combinado del promotor del gen CDKN2A (p16) en tejido tumoral y estadio clínico agrupado", "Cualitativa nominal politómica", "1 = Unmethylated + Local; 2 = Unmethylated + Advanced; 3 = Methylated + Local; 4 = Methylated + Advanced")
diccionario <- documentar(diccionario, "ml_N", "Estado combinado del promotor del gen MLH1 en tejido normal y estadio clínico agrupado", "Cualitativa nominal politómica", "1 = Unmethylated + Local; 2 = Unmethylated + Advanced; 3 = Methylated + Local; 4 = Methylated + Advanced")
diccionario <- documentar(diccionario, "ml_T", "Estado combinado del promotor del gen MLH1 en tejido tumoral y estadio clínico agrupado", "Cualitativa nominal politómica", "1 = Unmethylated + Local; 2 = Unmethylated + Advanced; 3 = Methylated + Local; 4 = Methylated + Advanced")
diccionario <- documentar(diccionario, "mg_N", "Estado combinado del promotor del gen MGMT en tejido normal y estadio clínico agrupado", "Cualitativa nominal politómica", "1 = Unmethylated + Local; 2 = Unmethylated + Advanced; 3 = Methylated + Local; 4 = Methylated + Advanced")
diccionario <- documentar(diccionario, "mg_T", "Estado combinado del promotor del gen MGMT en tejido tumoral y estadio clínico agrupado", "Cualitativa nominal politómica", "1 = Unmethylated + Local; 2 = Unmethylated + Advanced; 3 = Methylated + Local; 4 = Methylated + Advanced")

diccionario <- documentar(
  diccionario,
  "all_gene_N_2vs1",
  "Comparación entre las categorías 2 y 1 de all_gene_N",
  "Cualitativa nominal dicotómica",
  "1 = Grupo 1; 2 = Grupo 2; NA = Grupos 3 y 4"
)

diccionario <- documentar(
  diccionario,
  "all_gene_N_3vs1",
  "Comparación entre las categorías 3 y 1 de all_gene_N",
  "Cualitativa nominal dicotómica",
  "1 = Grupo 1; 2 = Grupo 3; NA = Grupos 2 y 4"
)

diccionario <- documentar(
  diccionario,
  "all_gene_N_23vs1",
  "Comparación entre las categorías 2 y 3 versus la categoría 1 de all_gene_N",
  "Cualitativa nominal dicotómica",
  "1 = Grupo 1; 2 = Grupos 2 y 3; NA = Grupo 4"
)

diccionario <- documentar(
  diccionario,
  "all_gene_N_4vs1",
  "Comparación entre las categorías 4 y 1 de all_gene_N",
  "Cualitativa nominal dicotómica",
  "1 = Grupo 1; 2 = Grupo 4; NA = Grupos 2 y 3"
)