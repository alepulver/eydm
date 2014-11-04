library(pander)
library(ggplot2)
library(plyr)
library(Hmisc)
#library(tabplot)
library(FactoMineR)

load("encuesta.Rda")

categorias_excluidas <- c(
  "iniciales_de_nombre_y_apellido", "edad", "motivo_de_consulta",
  "como_llego_a_este_hospital_recomendacion", "como_llego_a_este_hospital_medios_de_comunicacion", "como_llego_a_este_hospital_derivacion_medica",
  "motivo_de_consulta_medico", "diagnostico_de_egreso", "destino")
seleccion_categoricas <- !(names(encuesta) %in% c(categorias_excluidas, "urgencia"))
seleccion_contingencias <- !(names(encuesta) %in% categorias_excluidas)

source("utils.R")