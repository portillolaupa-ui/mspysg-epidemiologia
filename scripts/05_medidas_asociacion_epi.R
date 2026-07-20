library(epiR)
library(gt)
library(dplyr)

data <- readRDS("data/data_transformada.rds")


#Razón de incidencias acumuladas de recurrencia de cáncer
#==============================================================================
tabla_2x2_cdkn2a_t <- table(
  data$CDKN2A_T,
  data$Recurrence
)

tabla_2x2_mgmt_t <- table(
  data$MGMT_T,
  data$Recurrence
)

tabla_2x2_mlh1_t <- table(
  data$MLH1_T,
  data$Recurrence
)

tabla_2x2_cdkn2a_t
tabla_2x2_mgmt_t
tabla_2x2_mlh1_t

epi.2by2(tabla_2x2_cdkn2a_t, method = "cohort.count")
epi.2by2(tabla_2x2_mgmt_t, method = "cohort.count")
epi.2by2(tabla_2x2_mlh1_t, method = "cohort.count")


##Razón de tasas de incidencia
#==============================================================================

matriz_tasa_r_cdkn2at <- tasa_recurrencia_cdkn2a_t %>%
  select(cases, follow_up_time)

matriz_tasa_r_mgmtt <- tasa_recurrencia_mgmt_t %>%
  select(cases, follow_up_time)

matriz_tasa_r_mlh1t <- tasa_recurrencia_mlh1_t %>%
  select(cases, follow_up_time)

matrix_tasa_r_cdkn2at <- as.matrix(matriz_tasa_r_cdkn2at)
matrix_tasa_r_mgmtt <- as.matrix(matriz_tasa_r_mgmtt)
matrix_tasa_r_mlh1t <- as.matrix(matriz_tasa_r_mlh1t)

epi.2by2(matrix_tasa_r_cdkn2at, method = "cohort.time", units = 1000)
epi.2by2(matrix_tasa_r_mgmtt, method = "cohort.time", units = 1000)
epi.2by2(matrix_tasa_r_mlh1t, method = "cohort.time", units = 1000)

#Creacion de tabla unificada de RR y IRR
#==============================================================================
tabla_asociacion <- tibble(
  Gen = c("CDKN2A", "MGMT", "MLH1"),
  
  RR = c(1.43, 1.21, 1.18),
  RR_IC95 = c(
    "1.01–2.03",
    "0.86–1.70",
    "0.82–1.71"
  ),
  
  IRR = c(1.56, 1.40, 1.14),
  IRR_IC95 = c(
    "0.99–2.52",
    "0.89–2.23",
    "0.67–1.88"
  )
)


tabla_asociacion_gt <-
  tabla_asociacion %>%
  mutate(
    `RR (IC95%)` =
      sprintf("%.2f (%s)", RR, RR_IC95),
    
    `IRR (IC95%)` =
      sprintf("%.2f (%s)", IRR, IRR_IC95)
  ) %>%
  select(
    Gen,
    `RR (IC95%)`,
    `IRR (IC95%)`
  ) %>%
  gt() %>%
  tab_header(
    title = md(
      "**Tabla 4. Medidas de asociación entre la metilación tumoral y la recurrencia**"
    )
  ) %>%
  tab_source_note(
    source_note =
      md("**RR:** Razón de incidencias acumuladas (riesgo relativo). **IRR:** Razón de tasas de incidencia. IC95%: intervalo de confianza del 95%.")
  )

gtsave(
  tabla_asociacion_gt,
  filename = "outputs/tablas/tabla_4_medidas_asociacion.png",
  vwidth = 1800,
  vheight = 700,
  expand = 10
)