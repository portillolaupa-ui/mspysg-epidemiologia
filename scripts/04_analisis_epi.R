library(epiR)

data <- readRDS("data/data_transformada.rds")

CDKN2A_rr <- factor(
  data$CDKN2A_N,
  levels = c("Methylated", "Unmethylated")
)

Recurrence_rr <- factor(
  data$Recurrence,
  levels = c("Presence", "Absence")
)


riesgo_no_metilados <- 54/152
riesgo_metilados <- 31/63

RR <- riesgo_metilados / riesgo_no_metilados


rr_cdkn2a <- epi.2by2(
  dat = tabla_cdkn2a_rr,
  method = "cohort.count",
  conf.level = 0.95
)

rr_cdkn2a

#¿Existe asociación entre la metilación de del gen en tejido normal y la recurrencia del cáncer colorrectal?