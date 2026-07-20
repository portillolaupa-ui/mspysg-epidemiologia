library(readxl)
library(dplyr)
library(epiR)

data <- readRDS("data/data_transformada.rds")

#Incidencia acumulada de recurrencia
incidencia_acumulada_rec <- data %>%
  group_by(Recurrence) %>%
  summarise(n=n()) %>%
  mutate(incidence=n/sum(n)*100)

incidencia_acumulada_rec

#Tasa de incidencia de recurrencia
tasa_recurrencia <- data %>%
  summarise(
    cases = sum(Recurrence =="Presence"),
    follow_up_time = sum(Follow_up_month),
    tasa_incidencia = cases/follow_up_time*1000)

tasa_recurrencia

#Tasa de incidencia de recurrencia por estado de metilación 
# en tejido tumoral CDKN2A
tasa_cdkn2a_t <- data %>%
  group_by(CDKN2A_T) %>%
  summarise(
    n=n(),
    cases = sum(Recurrence == "Presence"),
    follow_up_time = sum(Follow_up_month),
    tasa_incidencia = (cases /follow_up_time)*1000)

tasa_cdkn2a_t

#Tasa de incidencia de recurrencia por estado de metilación 
# en tejido tumoral MGMT
tasa_mgmt_t <- data %>%
  group_by(MGMT_T) %>%
  summarise(
    n=n(),
    cases = sum(Recurrence == "Presence"),
    follow_up_time = sum(Follow_up_month),
    tasa_incidencia = (cases /follow_up_time)*1000)

tasa_mgmt_t

#Tasa de incidencia de recurrencia por estado de metilación 
# en tejido tumoral MLH1
tasa_mlh1_t <- data %>%
  group_by(MLH1_T) %>%
  summarise(
    n=n(),
    cases = sum(Recurrence == "Presence"),
    follow_up_time = sum(Follow_up_month),
    tasa_incidencia = (cases /follow_up_time)*1000)

tasa_mlh1_t