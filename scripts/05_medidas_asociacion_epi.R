library(epiR)
library(gt)
library(dplyr)
library(survival)
library(ggfortify)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(survminer)
library(broom)
library(discSurv)
library(ggsci)
library(DT)
library(broom)

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

#Kaplan Meier
#==============================================================================
data_km <- data %>%
  mutate(
    status = case_when(
      Recurrence == "Presence" ~ 1,
      Recurrence == "Absence" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(
    !is.na(Follow_up_month),
    !is.na(status)
  )

table(data_km$Recurrence, data_km$status)

# Cohorte general
km_general <- survfit(Surv(Follow_up_month, status) ~ 1, data = data_km)
#tidy(km_general)
#tail(tidy(km_general), 15)

# Según CDKN2A tumoral
km_cdkn2a <- survfit(Surv(Follow_up_month, status) ~ CDKN2A_T, data = data_km)

# Según MGMT tumoral
km_mgmt <- survfit(Surv(Follow_up_month, status) ~ MGMT_T, data = data_km)

# Según MLH1 tumoral
km_mlh1 <- survfit(Surv(Follow_up_month, status) ~ MLH1_T, data = data_km)

#Creación de gráficos de Kaplan Meier: General y por tipo de gen
grafico_km_general <- autoplot(km_general, fun= "event", surv.colour = "black") + 
  labs(x = "Tiempo (meses)",
       y = "Probabilidad acumulada de recurrencia",
       title = "Cohorte general") +
  coord_cartesian(xlim = c(0, 80)) +
  theme_minimal()

grafico_km_cdkn2a <- autoplot(km_cdkn2a, fun= "event", surv.alpha = 0.8, conf.int.alpha = 0.4) +
  labs(x = "Tiempo (meses)",
       y = "Probabilidad acumulada de recurrencia",
       title = "Gen CDKN2A en tejido tumoral") +
  scale_fill_lancet() +
  scale_color_lancet() +
  coord_cartesian(xlim = c(0, 80)) +
  theme_minimal()

grafico_km_mgmt <- autoplot(km_mgmt, fun= "event", surv.alpha = 0.8, conf.int.alpha = 0.4) +
  labs(x = "Tiempo (meses)",
       y = "Probabilidad acumulada de recurrencia",
       title = "Gen MGMT en tejido tumoral") +
  scale_fill_lancet() +
  scale_color_lancet() +
  coord_cartesian(xlim = c(0, 80)) +
  theme_minimal()

grafico_km_mlh1 <- autoplot(km_mlh1, fun= "event", surv.alpha = 0.8, conf.int.alpha = 0.4) +
  labs(x = "Tiempo (meses)",
       y = "Probabilidad acumulada de recurrencia",
       title = "Gen MLH1 en tejido tumoral") +
  scale_fill_lancet() +
  scale_color_lancet() +
  coord_cartesian(xlim = c(0, 80)) +
  theme_minimal()

grafico_km_completo <-
  (grafico_km_general | grafico_km_cdkn2a) /
  (grafico_km_mgmt | grafico_km_mlh1) +
  plot_annotation(
    title = paste(
      "Probabilidad acumulada de recurrencia según",
      "el estado de metilación en tejido tumoral"
    )
  )

grafico_km_completo

ggsave(
  filename = "outputs/figuras/kaplan_meier_general_y_genes.png",
  plot = grafico_km_completo,
  width = 14,
  height = 10,
  dpi = 300
)

#Long-rank: Determinar si existen diferencias entre las curvas

#CDKN2A
logrank_cdkn2a <- survdiff(
  Surv(Follow_up_month, status) ~ CDKN2A_T,
  data = data_km
)

p_cdkn2a <- 1 - pchisq(
  logrank_cdkn2a$chisq,
  df = length(logrank_cdkn2a$n) - 1
)

logrank_cdkn2a
p_cdkn2a

#MGMT
logrank_mgmt <- survdiff(
  Surv(Follow_up_month, status) ~ MGMT_T,
  data = data_km
)

p_mgmt <- 1 - pchisq(
  logrank_mgmt$chisq,
  df = length(logrank_mgmt$n) - 1
)

logrank_mgmt
p_mgmt

#MLH1
logrank_mlh1 <- survdiff(
  Surv(Follow_up_month, status) ~ MLH1_T,
  data = data_km
)

p_mlh1 <- 1 - pchisq(
  logrank_mlh1$chisq,
  df = length(logrank_mlh1$n) - 1
)

logrank_mlh1
p_mlh1

#Resumen
resultado_logrank <- data.frame(
  Gen = c("CDKN2A", "MGMT", "MLH1"),
  Chi_cuadrado = c(
    logrank_cdkn2a$chisq,
    logrank_mgmt$chisq,
    logrank_mlh1$chisq
  ),
  p_valor = c(
    p_cdkn2a,
    p_mgmt,
    p_mlh1
  )
)

resultado_logrank