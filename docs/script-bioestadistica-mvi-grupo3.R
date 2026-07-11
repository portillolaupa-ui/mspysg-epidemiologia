#BASE INDICE DE MASA VENTRICULAR IZQUIERDA - GRUPO 3

# IMPORTACION DE BASES ----------------------------------------------------

MVI <- read.csv("mvi.csv")
IMC <- read.csv2("bmi_OMS.csv")

# INSTALACION DE PAQUETES -------------------------------------------------
install.packages("gtsummary")
library(gtsummary)
library(dplyr)
library(tidyverse)

# CONVERSION DE VARIABLES A VARIABLES "FACTOR" ----------------------------

## CATEGORIZACION DE PRESION ARTERIAL
MVI$bpcat <- factor(MVI$bpcat, levels = c(1,2,3), labels = c("Normal", "Pre - Hipertenso", "Hipertenso"))
str(MVI$bpcat)

## CATEGORIZACION DE GENERO
MVI$gender <- factor(MVI$gender, levels = c(1,2), labels = c("Masculino", "Femenino"))
str(MVI$gender)
tbl_summary(MVI)

## CATEGORIZACION DE IMC

### PASO 1: PASAR LA EDAD (variable AGE) DE AÑOS A MESES
MVI$meses <- MVI$age*12
MVI$meses_entero <- trunc(MVI$meses)

### PASO 2: UNIR LA BASE DE DATOS DE BMI DE LA OMS A LA BASE MVI, COINCIDIENDO EN MESES Y GENERO
Base_MVI <- MVI %>%
            left_join(IMC, by = c("meses_entero","gender"))

### PASO 3: CATEGORIZACION DE IMC
Base_MVI <- Base_MVI %>%
  mutate(IMC_CAT = case_when( bmi < SD3neg ~ "Delgadez Severa",
                              bmi >= SD3neg & bmi < SD2neg ~ "Delgadez",
                              bmi >= SD2neg & bmi <= SD1 ~ "Normal",   
                              bmi > SD1 & bmi <= SD2 ~ "Sobrepeso",
                              bmi > SD2 ~ "Obesidad"))
  str(Base_MVI)

# CREACION DE BASE FINAL DE TRABAJO ---------------------------------------

MVI_FINAL <- select(Base_MVI,lvmht27,bpcat,gender,age,bmi,meses_entero,IMC_CAT)
tbl_summary(MVI_FINAL, 
label = list(
  lvmht27 ~ "Indice de masa ventricular izquierda",
  bpcat ~ "Categoría de presión arterial",
  gender ~ "Sexo",
  age ~ "Edad (años)",
  bmi ~ "Indice de masa corporal",
  meses_entero ~ "Edad (meses)",
  IMC_CAT ~ "Categoría de IMC"
)) %>%
  modify_caption("**Características de las Variables**") %>%
  modify_header(label = "**Variables**")

# CREACION DE GRAFICOS ----------------------------------------------------

## GRAFICO 1 - DISTRIBUCION DE MVI Y PRESION ARTERIAL

### BLOQUE A
MVI_FINAL %>%
  ggplot(aes(x = bpcat, y = lvmht27 , color = bpcat)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 2) +
  labs( x = "Presión Arterial",
        y = "Índice de Masa Ventricular Izquierda",
        title = "Distribución entre el MVI y la Presión Arterial",
        color = NULL) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        legend.position = "top",
        strip.background = element_rect(fill = c("grey"), color = NA),
        panel.background = element_rect(fill = "gray95",color = NA))

### BLOQUE B
MVI_FINAL %>%
  ggplot(aes(x = bpcat, y = lvmht27 , color = bpcat)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 2) +
  labs( x = "Presión Arterial",
        y = "Índice de Masa Ventricular Izquierda",
        title = "Distribución entre el MVI y la Presión Arterial",
        color = NULL) +
  facet_wrap(~ gender) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        legend.position = "top",
        strip.background = element_rect(fill = c("grey"), color = NA),
        panel.background = element_rect(fill = "gray95",color = NA))

## GRAFICO 2 - DISTRIBUCION DE MVI E IMC

MVI_FINAL$IMC_CAT <- factor(MVI_FINAL$IMC_CAT, levels = c("Delgadez Severa","Delgadez","Normal","Sobrepeso",
                                                          "Obesidad"), ordered = TRUE)

### BLOQUE A
MVI_FINAL %>%
  ggplot(aes(x = IMC_CAT, y = lvmht27 , col = IMC_CAT)) +
  geom_boxplot(aes(color = IMC_CAT)) + 
  geom_jitter(width = 0.15, alpha = 0.5, size = 2) + 
  labs(x = "Índice de Masa Corporal", 
       y = "Índice de Masa Ventricular Izquierda",
       title = "Distribución de MVI y el IMC",
       color = NULL) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        legend.position = "top",
        strip.background = element_rect(fill = c("grey"), color = NA),
        panel.background = element_rect(fill = "gray95",color = NA))

### BLOQUE B
MVI_FINAL %>%
  ggplot(aes(x = IMC_CAT, y = lvmht27 , col = IMC_CAT)) +
  geom_boxplot() + 
  geom_jitter(width = 0.15, alpha = 0.5, size = 2) + 
  labs(x = "Índice de Masa Corporal", 
       y = "Índice de Masa Ventricular Izquierda",
       title = "Distribución de MVI y el IMC",
       color = NULL) +
  facet_wrap(.~gender) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        legend.position = "top",
        strip.background = element_rect(fill = c("grey"), color = NA),
        panel.background = element_rect(fill = "gray95",color = NA))

## GRAFICO 3 - DISTRIBUCION DE MVI Y PRESION ARTERIAL SEGUN IMC
MVI_FINAL %>%
  ggplot(aes(x = bpcat, y = lvmht27, col = bpcat)) +
  geom_boxplot() +
  labs(x = "Índice de Masa Ventricular Izquierda", 
       y = "Presiòn Arterial",
       title = "Distribución de MVI y Presión Arterial según IMC",
       color = NULL) +
  facet_wrap(. ~ IMC_CAT, nrow = 1, scales = "free") +
  scale_color_manual(values = c("Normal" = "#1b9e77",
                                "Pre - Hipertenso" = "#d95f02",
                                "Hipertenso" = "#7570b3")) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        legend.position = "top",
        panel.background = element_rect(fill = "#e8f1fa",color = NA))

# EVALUACIÓN DE LA NORMALIDAD ---------------------------------------------

#IMVI CON BPCAT

shapiro.test(MVI_FINAL$lvmht27[MVI_FINAL$IMC_CAT == "Normal"])
shapiro.test(MVI_FINAL$lvmht27[MVI_FINAL$IMC_CAT == "Pre - Hipertenso"])
shapiro.test(MVI_FINAL$lvmht27[MVI_FINAL$IMC_CAT == "Hipertenso"])

# Tabla con resultados de Shapiro–Wilk

library(dplyr)
library(gt)

tabla_normalidad <- MVI_FINAL %>%
  group_by(bpcat) %>%
  summarise(
    W = round(shapiro.test(lvmht27)$statistic, 5),
    p_valor = round(shapiro.test(lvmht27)$p.value, 6)
  ) %>%
  rename("Categoría de Presión Arterial" = bpcat)

tabla_normalidad %>%
  gt() %>%
  tab_header(
    title = md("**Prueba de Normalidad Shapiro–Wilk**")
  )

# Distribución de IMVI y Categoría de la Presión Arterial

tbl_summary(MVI_FINAL, include = c(lvmht27),
            by = bpcat,
            type = all_continuous() ~ "continuous2",
            statistic = all_continuous() ~ c(
              "{mean} ({sd])",
              "{median} ({p25}, {p75})",
              "({min}, {max})")) %>%
  modify_caption("**Distribución del IMVI y las categorías de la Presión Arterial**") %>%
  modify_header(label = "**IMVI**")

#IMVI CON IMC

shapiro.test(MVI_FINAL$lvmht27[MVI_FINAL$IMC_CAT == "Normal"])
shapiro.test(MVI_FINAL$lvmht27[MVI_FINAL$IMC_CAT == "Sobrepeso"])
shapiro.test(MVI_FINAL$lvmht27[MVI_FINAL$IMC_CAT == "Obesidad"])

# Tabla con resultados de Shapiro–Wilk

library(dplyr)
library(gt)

tabla_IMC_normalidad <- MVI_FINAL %>%
  group_by(IMC_CAT) %>%
  summarise(
    W = round(shapiro.test(lvmht27)$statistic, 5),
    p_valor = round(shapiro.test(lvmht27)$p.value, 6)
  ) %>%
  rename("IMC" = IMC_CAT)

tabla_IMC_normalidad %>%
  gt() %>%
  tab_header(
    title = md("**Prueba de Normalidad Shapiro–Wilk**")
  )

# Distribución de IMVI y Categoría de IMC

tbl_summary(MVI_FINAL, include = c(lvmht27),
            by = IMC_CAT,
            type = all_continuous() ~ "continuous2",
            statistic = all_continuous() ~ c(
              "{mean} ({sd})",
              "{median} ({p25}, {p75})",
              "({min}, {max})")) %>%
  modify_caption("**Distribución del IMVI y las categorías de IMC**") %>%
  modify_header(label = "**IMVI**")


# ANÁLISIS INFERENCIAL ----------------------------------------------------


#I. APLICACION DE PRUEBA DE ANOVA PARA DIFERENCIA DE MEDIAS
#Evaluación de normalidad
install.packages("psych")
library(psych)

describeBy(MVI_FINAL$lvmht27, group = MVI_FINAL$bpcat)

shapiro.test(MVI_FINAL$lvmht27[MVI_FINAL$bpcat == "Normal"])
shapiro.test(MVI_FINAL$lvmht27[MVI_FINAL$bpcat == "Pre - Hipertenso"])
shapiro.test(MVI_FINAL$lvmht27[MVI_FINAL$bpcat == "Hipertenso"])

ggplot(MVI_FINAL, aes(x = lvmht27)) +
  geom_histogram(bins = 30, color = "black", fill = "gray") +
  facet_wrap(~ bpcat)

#Evaluación de Homocedasticidad
ggplot(MVI_FINAL,aes(x = bpcat, y = lvmht27)) + geom_boxplot() +
  labs(
    x = "Categoría de presión arterial",
    y = "IMVI"
  ) +
  theme_minimal()

bartlett.test(lvmht27 ~ bpcat, data = MVI_FINAL)

#Prueba ANOVA
modelo1 <- aov(lvmht27 ~ bpcat, data = MVI_FINAL)

summary(modelo1)

#Post-estimación del ANOVA
pairwise.t.test(
  MVI_FINAL$lvmht27,
  MVI_FINAL$bpcat,
  p.adjust.method = "bonferroni"
)


#II. APLICACION DE PRUEBA DE SPEARMAN PARA CORRELACIÓN

#Evaluación de linealidad
plot(MVI_FINAL$bmi, MVI_FINAL$lvmht27,
     xlab = "Índice de Masa Corporal (IMC)",
     ylab = "Índice de Masa Ventricular Izquierda (IMVI)")

abline(lm(MVI_FINAL$lvmht27 ~ MVI_FINAL$bmi),
       col = "red",
       lwd = 2)


#Evaluación de normalidad
describe(MVI_FINAL[, c("bmi", "lvmht27")])

hist(MVI_FINAL$bmi,
     xlab = "IMC",
     main = "Histograma IMC")

hist(MVI_FINAL$lvmht27,
     xlab = "IMVI",
     main = "Histograma IMVI")


shapiro.test(MVI_FINAL$lvmht27)
shapiro.test(MVI_FINAL$bmi)


#Aplicación de Spearman
cor.test(
  MVI_FINAL$bmi,
  MVI_FINAL$lvmht27,
  method = "spearman"
)
