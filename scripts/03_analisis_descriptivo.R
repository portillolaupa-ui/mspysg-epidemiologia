library(tidyverse)
library(janitor)
library(gtsummary)

data <- readRDS("data/data_transformada.rds")


# Sexo
tabla_sexo <- data %>%
  tabyl(Sex) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)

tabla_sexo

# Estado cáncer agrupado
tabla_estadio <- data %>%
  tabyl(stage_simple_2) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)

tabla_estadio

# Tabla general descriptiva
tabla_1 <- data %>%
  select(
    Sex,
    stage_simple,
    stage_simple_2,
    Recurrence,
    Survival,
    chemT,
    CDKN2A_N,
    MGMT_N,
    MLH1_N,
    CDKN2A_T,
    MGMT_T,
    MLH1_T,
    Follow_up_month
  ) %>%
  tbl_summary(
    type = list(
      chemT ~ "categorical"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      Follow_up_month ~ "{mean} ({sd})"
    ),
    digits = list(
      all_categorical() ~ 1,
      Follow_up_month ~ 2
    ),
    missing = "ifany",
    label = list(
      Sex ~ "Sexo",
      stage_simple ~ "Estadio clínico",
      stage_simple_2 ~ "Grupo de estadio",
      Recurrence ~ "Recurrencia",
      Survival ~ "Mortalidad",
      chemT ~ "Quimioterapia",
      CDKN2A_N ~ "CDKN2A en tejido normal",
      MGMT_N ~ "MGMT en tejido normal",
      MLH1_N ~ "MLH1 en tejido normal",
      CDKN2A_T ~ "CDKN2A en tejido tumoral",
      MGMT_T ~ "MGMT en tejido tumoral",
      MLH1_T ~ "MLH1 en tejido tumoral",
      Follow_up_month ~ "Tiempo de seguimiento, meses"
    )
  ) %>%
  modify_caption(
    "**Tabla 1. Características descriptivas de los pacientes con cáncer colorrectal**"
  ) %>%
  bold_labels()

tabla_1

