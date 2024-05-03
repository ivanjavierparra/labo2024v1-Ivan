# Cargar bibliotecas necesarias
library(data.table)
library(ranger)
library(isotone)

# Limpiar la memoria
rm(list = ls())
gc()

# Establecer el directorio de trabajo
setwd("c:\\Users\\Usuario\\Documents\\Universidad\\austral\\2024\\lab1\\TareasHogar\\Tarea-04\\05 - ranger con hiperparametros optimos\\")

# Definir parámetros del modelo
param <- list(
  "num.trees" = 279,
  "mtry" = 50,
  "min.node.size" = 83,
  "max.depth" = 11
)

# Cargar semillas desde el archivo
tabla_semillas <- fread("c:\\Users\\Usuario\\Documents\\Universidad\\austral\\2024\\lab1\\competencia\\datasets\\mis_semillas.txt")
ksemilla_azar <- tabla_semillas[1, semilla]

# Cargar datos de entrenamiento y prueba
dataset <- fread("c:\\Users\\Usuario\\Documents\\Universidad\\austral\\2024\\lab1\\competencia\\datasets\\dataset_pequeno.csv")
dtrain <- dataset[foto_mes == 202107]
dapply <- dataset[foto_mes == 202109]

# Factorizar la variable de respuesta
factorizado <- as.factor(dtrain$clase_ternaria)
dtrain[, clase_ternaria := factorizado]

# Imputar valores perdidos en datos de entrenamiento
dtrain <- na.roughfix(dtrain)
setorder(dtrain, clase_ternaria)

# Ajustar modelo de bosque aleatorio
set.seed(ksemilla_azar)
modelo <- ranger(
  formula = "clase_ternaria ~ .",
  data = dtrain,
  probability = TRUE,
  num.trees = param$num.trees,
  mtry = param$mtry,
  min.node.size = param$min.node.size,
  max.depth = param$max.depth
)

# Preparar datos de prueba
dapply[, clase_ternaria := NULL]
dapply <- na.roughfix(dapply)

# Generar predicciones
prediccion <- predict(modelo, dapply)

# Aplicar regresión isotónica a las probabilidades
probabilidades <- as.vector(prediccion$predictions[, "BAJA+2"])
probabilidades_iso <- isotone::isotone(probabilidades)

# Generar archivo de entrega para Kaggle
entrega <- as.data.table(list(
  "numero_de_cliente" = dapply[, numero_de_cliente],
  "Predicted" = as.numeric(probabilidades_iso)
))
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/KA4310_ISOTONIC/", showWarnings = FALSE)
archivo_salida <- "./exp/KA4310_ISOTONIC/KA4310_001.csv"
fwrite(entrega, file = archivo_salida, sep = ",")