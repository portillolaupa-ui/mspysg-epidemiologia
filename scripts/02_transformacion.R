library(readxl)

data <- read_excel("data/kuan-etal-plosone-2015.xlsx")

#=========================================================
#Recodificación de variables descriptivas y covariables

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

data$stage_simple_2 <- factor(
  data$stage_simple_2,
  levels = c(1, 2),
  labels = c("Local", "Advanced"),
  ordered = TRUE
)

data$chemT <- factor(
  data$chemT,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

table(data$Sex)
table(data$Survival)
table(data$Recurrence)
table(data$stage_simple)
table(data$stage_simple_2)
table(data$grade, useNA = "ifany")
table(data$chemT, useNA = "ifany")
table(data$location, useNA = "ifany")

#=========================================================
#Recodificación de variables de biomarcadores

data$CDKN2A_N <- factor(
  data$CDKN2A_N,
  levels = c(0,1),
  labels = c("Unmethylated","Methylated")
)

data$CDKN2A_T <- factor(
  data$CDKN2A_T,
  levels = c(0,1),
  labels = c("Unmethylated","Methylated")
)

data$MGMT_N <- factor(
  data$MGMT_N,
  levels = c(0,1),
  labels = c("Unmethylated","Methylated")
)

data$MGMT_T <- factor(
  data$MGMT_T,
  levels = c(0,1),
  labels = c("Unmethylated","Methylated")
)

data$MLH1_N <- factor(
  data$MLH1_N,
  levels = c(0,1),
  labels = c("Unmethylated","Methylated")
)

data$MLH1_T <- factor(
  data$MLH1_T,
  levels = c(0,1),
  labels = c("Unmethylated","Methylated")
)

table(data$CDKN2A_N)
table(data$CDKN2A_T)
table(data$MGMT_N)
table(data$MGMT_T)
table(data$MLH1_N)
table(data$MLH1_T)

#=========================================================
#Recodificación de variables derivadas de biomarcadores

methylation_and_stage <- c(
  "UnMeth / Local",
  "UnMeth / Advanced",
  "Meth / Local",
  "Meth / Advanced"
)

data$p16N <- factor(
  data$p16N,
  levels = 1:4,
  labels = methylation_and_stage)

data$p16T <- factor(
  data$p16T,
  levels = 1:4,
  labels = methylation_and_stage)

data$ml_N <- factor(
  data$ml_N,
  levels = 1:4,
  labels = methylation_and_stage)

data$ml_T <- factor(
  data$ml_T,
  levels = 1:4,
  labels = methylation_and_stage)

data$mg_N <- factor(
  data$mg_N,
  levels = 1:4,
  labels = methylation_and_stage)

data$mg_T <- factor(
  data$mg_T,
  levels = 1:4,
  labels = methylation_and_stage)

table(data$p16N)
table(data$p16T)
table(data$ml_N)
table(data$ml_T)
table(data$mg_N)
table(data$mg_T)

#=========================================================
#Guardar la data transformada para siguientes análisis

saveRDS(data, "data/data_transformada.rds")
