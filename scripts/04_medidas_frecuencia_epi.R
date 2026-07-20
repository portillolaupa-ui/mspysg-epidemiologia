library(readxl)
library(dplyr)
library(epiR)
library(gt)

data <- readRDS("data/data_transformada.rds")

#Incidencia acumulada de recurrencia
incidencia_acumulada_rec <- data %>%
  group_by(Recurrence) %>%
  summarise(n=n()) %>%
  mutate(incidence=n/sum(n)*100)

incidencia_acumulada_rec

#Tasa de incidencia de recurrencia
tasa_recurrencia_global <- data %>%
  summarise(
    cases = sum(Recurrence =="Presence"),
    follow_up_time = sum(Follow_up_month),
    tasa_incidencia = cases/follow_up_time*1000)

tasa_recurrencia_global

#Tasa de incidencia de recurrencia por estado de metilación 
# en tejido tumoral CDKN2A
tasa_recurrencia_cdkn2a_t <- data %>%
  group_by(CDKN2A_T) %>%
  summarise(
    n=n(),
    cases = sum(Recurrence == "Presence"),
    follow_up_time = sum(Follow_up_month),
    tasa_incidencia = (cases /follow_up_time)*1000)

tasa_recurrencia_cdkn2a_t

#Tasa de incidencia de recurrencia por estado de metilación 
# en tejido tumoral MGMT
tasa_recurrencia_mgmt_t <- data %>%
  group_by(MGMT_T) %>%
  summarise(
    n=n(),
    cases = sum(Recurrence == "Presence"),
    follow_up_time = sum(Follow_up_month),
    tasa_incidencia = (cases /follow_up_time)*1000)

tasa_recurrencia_mgmt_t

#Tasa de incidencia de recurrencia por estado de metilación 
# en tejido tumoral MLH1
tasa_recurrencia_mlh1_t <- data %>%
  group_by(MLH1_T) %>%
  summarise(
    n=n(),
    cases = sum(Recurrence == "Presence"),
    follow_up_time = sum(Follow_up_month),
    tasa_incidencia = (cases /follow_up_time)*1000)

tasa_recurrencia_mlh1_t



#Creacion de tabla unificada de tasas de incidencia por estado 
#de metilación en tejido tumoral
tabla_tasas_incidencia <- bind_rows(
  
  tasa_recurrencia_cdkn2a_t %>%
    rename(`Estado de metilación` = CDKN2A_T) %>%
    mutate(Gen = "CDKN2A"),
  
  tasa_recurrencia_mgmt_t %>%
    rename(`Estado de metilación` = MGMT_T) %>%
    mutate(Gen = "MGMT"),
  
  tasa_recurrencia_mlh1_t %>%
    rename(`Estado de metilación` = MLH1_T) %>%
    mutate(Gen = "MLH1")
  
) %>%
  select(
    Gen,
    `Estado de metilación`,
    n,
    cases,
    follow_up_time,
    tasa_incidencia
  ) %>%
  rename(
    `Pacientes` = n,
    `Recurrencias` = cases,
    `Persona-meses` = follow_up_time,
    `Tasa por 1000 persona-meses` = tasa_incidencia
  ) %>%
  mutate(
    `Tasa por 1000 persona-meses` =
      round(`Tasa por 1000 persona-meses`, 1),
    `Persona-meses` =
      round(`Persona-meses`, 0)
  )

tabla_tasas_incidencia


tabla_tasas_incidencia_gt <- tabla_tasas_incidencia %>%
  gt(groupname_col = "Gen") %>%
  tab_header(
    title = md("**Tabla 3. Tasa de incidencia de recurrencia según estado de metilación en tejido tumoral**")
  )

gtsave(
  tabla_tasas_incidencia_gt,
  filename = "outputs/tablas/tabla_3_tasas_incidencia_tumoral.png",
  vwidth = 1800,
  vheight = 900,
  expand = 10
)