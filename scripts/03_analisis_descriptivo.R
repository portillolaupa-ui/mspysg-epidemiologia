library(tidyverse)
library(janitor)
library(gtsummary)
library(gt)

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
      Follow_up_month ~ "Tiempo de seguimiento, meses"
    )
  ) %>%
  modify_caption(
    "**Tabla 1. Características descriptivas de los pacientes con cáncer colorrectal**"
  ) %>%
  bold_labels()

tabla_1


tabla_1 %>%
  as_gt() %>%
  gt::gtsave(
    filename = "outputs/tablas/tabla_1_caracteristicas_descriptivas.png"
  )


# Tabla general descriptiva de doble entrada
variables_comparacion <- c(
  "Sex",
  "stage_simple_2",
  "chemT",
  "Recurrence",
  "Survival"
)

tabla_cdkn2a_n <- data %>%
  select(
    all_of(variables_comparacion),
    CDKN2A_N
  ) %>%
  tbl_summary(
    by = CDKN2A_N,
    statistic = all_categorical() ~ "{n} ({p}%)",
    digits = all_categorical() ~ 1,
    missing = "ifany",
    type = list(
      chemT ~ "categorical"
    ),
    label = list(
      Sex ~ "Sexo",
      stage_simple_2 ~ "Grupo de estadio",
      chemT ~ "Quimioterapia",
      Recurrence ~ "Recurrencia",
      Survival ~ "Mortalidad"
    )
  )

tabla_cdkn2a_t <- data %>%
  select(
    all_of(variables_comparacion),
    CDKN2A_T
  ) %>%
  tbl_summary(
    by = CDKN2A_T,
    statistic = all_categorical() ~ "{n} ({p}%)",
    digits = all_categorical() ~ 1,
    missing = "ifany",
    type = list(
      chemT ~ "categorical"
    ),
    label = list(
      Sex ~ "Sexo",
      stage_simple_2 ~ "Grupo de estadio",
      chemT ~ "Quimioterapia",
      Recurrence ~ "Recurrencia",
      Survival ~ "Mortalidad"
    )
  )

tabla_mgmt_n <- data %>%
  select(all_of(variables_comparacion), MGMT_N) %>%
  tbl_summary(
    by = MGMT_N,
    statistic = all_categorical() ~ "{n} ({p}%)",
    digits = all_categorical() ~ 1,
    missing = "ifany",
    type = list(chemT ~ "categorical"),
    label = list(
      Sex ~ "Sexo",
      stage_simple_2 ~ "Grupo de estadio",
      chemT ~ "Quimioterapia",
      Recurrence ~ "Recurrencia",
      Survival ~ "Mortalidad"
    )
  )

tabla_mgmt_t <- data %>%
  select(all_of(variables_comparacion), MGMT_T) %>%
  tbl_summary(
    by = MGMT_T,
    statistic = all_categorical() ~ "{n} ({p}%)",
    digits = all_categorical() ~ 1,
    missing = "ifany",
    type = list(chemT ~ "categorical"),
    label = list(
      Sex ~ "Sexo",
      stage_simple_2 ~ "Grupo de estadio",
      chemT ~ "Quimioterapia",
      Recurrence ~ "Recurrencia",
      Survival ~ "Mortalidad"
    )
  )

tabla_mlh1_n <- data %>%
  select(all_of(variables_comparacion), MLH1_N) %>%
  tbl_summary(
    by = MLH1_N,
    statistic = all_categorical() ~ "{n} ({p}%)",
    digits = all_categorical() ~ 1,
    missing = "ifany",
    type = list(chemT ~ "categorical"),
    label = list(
      Sex ~ "Sexo",
      stage_simple_2 ~ "Grupo de estadio",
      chemT ~ "Quimioterapia",
      Recurrence ~ "Recurrencia",
      Survival ~ "Mortalidad"
    )
  )

tabla_mlh1_t <- data %>%
  select(all_of(variables_comparacion), MLH1_T) %>%
  tbl_summary(
    by = MLH1_T,
    statistic = all_categorical() ~ "{n} ({p}%)",
    digits = all_categorical() ~ 1,
    missing = "ifany",
    type = list(chemT ~ "categorical"),
    label = list(
      Sex ~ "Sexo",
      stage_simple_2 ~ "Grupo de estadio",
      chemT ~ "Quimioterapia",
      Recurrence ~ "Recurrencia",
      Survival ~ "Mortalidad"
    )
  )

tabla_metilacion <- tbl_merge(
  tbls = list(
    tabla_cdkn2a_n,
    tabla_cdkn2a_t,
    tabla_mgmt_n,
    tabla_mgmt_t,
    tabla_mlh1_n,
    tabla_mlh1_t
  ),
  tab_spanner = c(
    "**CDKN2A normal**",
    "**CDKN2A tumoral**",
    "**MGMT normal**",
    "**MGMT tumoral**",
    "**MLH1 normal**",
    "**MLH1 tumoral**"
  )
) %>%
  modify_caption(
    "**Tabla 2. Características de los pacientes según estado de metilación, gen y tejido**"
  ) %>%
  bold_labels()

tabla_metilacion


tabla_metilacion %>%
  as_gt() %>%
  gt::gtsave(
    filename = "outputs/tablas/tabla_2_metilacion_por_gen_y_tejido.png",
    vwidth = 3200,
    vheight = 1600,
    expand = 10
  )

# Tabla características según recurrencia

tabla_recurrencia <- data %>%
  select(
    Recurrence,
    stage_simple_2,
    CDKN2A_N,
    CDKN2A_T,
    MGMT_N,
    MGMT_T,
    MLH1_N,
    MLH1_T
  ) %>%
  tbl_summary(
    by = Recurrence,
    statistic = all_categorical() ~ "{n} ({p}%)",
    digits = all_categorical() ~ 1,
    percent = "column",
    missing = "ifany",
    label = list(
      stage_simple_2 ~ "Grupo de estadio",
      CDKN2A_N ~ "CDKN2A en tejido normal",
      CDKN2A_T ~ "CDKN2A en tejido tumoral",
      MGMT_N ~ "MGMT en tejido normal",
      MGMT_T ~ "MGMT en tejido tumoral",
      MLH1_N ~ "MLH1 en tejido normal",
      MLH1_T ~ "MLH1 en tejido tumoral"
    )
  ) %>%
  modify_caption(
    "**Tabla 3A. Estado de metilacion de genes en tejido normal y tumoral según recurrencia**"
  ) %>%
  modify_header(
    label = "**Variable**"
  ) %>%
  bold_labels()

tabla_recurrencia

tabla_recurrencia %>%
  as_gt() %>%
  gt::gtsave(
    filename = "outputs/tablas/tabla_3A_metilacion_por_gen_segun_recurrencia.png"
  )

# Tabla características según mortalidad

tabla_mortalidad <- data %>%
  select(
    Survival,
    stage_simple_2,
    CDKN2A_N,
    CDKN2A_T,
    MGMT_N,
    MGMT_T,
    MLH1_N,
    MLH1_T
  ) %>%
  tbl_summary(
    by = Survival,
    statistic = all_categorical() ~ "{n} ({p}%)",
    digits = all_categorical() ~ 1,
    percent = "column",
    missing = "ifany",
    label = list(
      stage_simple_2 ~ "Grupo de estadio",
      CDKN2A_N ~ "CDKN2A en tejido normal",
      CDKN2A_T ~ "CDKN2A en tejido tumoral",
      MGMT_N ~ "MGMT en tejido normal",
      MGMT_T ~ "MGMT en tejido tumoral",
      MLH1_N ~ "MLH1 en tejido normal",
      MLH1_T ~ "MLH1 en tejido tumoral"
    )
  ) %>%
  modify_caption(
    "**Tabla 3B. Estado de metilación de genes en tejido normal y tumoral según mortalidad**"
  ) %>%
  modify_header(
    label = "**Variable**"
  ) %>%
  bold_labels()

tabla_mortalidad

tabla_mortalidad %>%
  as_gt() %>%
  gt::gtsave(
    filename = "outputs/tablas/tabla_3B_metilacion_por_gen_segun_mortalidad.png"
  )