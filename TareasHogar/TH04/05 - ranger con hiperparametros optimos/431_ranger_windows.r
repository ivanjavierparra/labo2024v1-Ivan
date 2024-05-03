# Ranger  una libreria que implementa el algoritmo Random Forest

# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("ranger")
require("randomForest") # solo se usa para imputar nulos

# notar como la suma de muchos arboles contrarresta el efecto de min.node.size=1
# "mtry" = 30, cantidad de variables que evalua para hacer un split
#  generalmente sqrt(ncol(dtrain))
param <- list(
  "num.trees" = 279, # cantidad de arboles
  "mtry" = 50,
  "min.node.size" = 83, # tamaño minimo de las hojas
  "max.depth" = 11 # 0 significa profundidad infinita
)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd("c:\\Users\\Usuario\\Documents\\Universidad\\austral\\2024\\lab1\\TareasHogar\\Tarea-04\\05 - ranger con hiperparametros optimos\\")

# cargo MI semilla, que esta en MI bucket
tabla_semillas <- fread( "c:\\Users\\Usuario\\Documents\\Universidad\\austral\\2024\\lab1\\competencia\\datasets\\mis_semillas.txt" )
ksemilla_azar <- tabla_semillas[ 1, semilla ]  # 1 es mi primer semilla


# cargo los datos donde entreno
dataset <- fread("c:\\Users\\Usuario\\Documents\\Universidad\\austral\\2024\\lab1\\competencia\\datasets\\dataset_pequeno.csv")


dtrain <- dataset[foto_mes == 202107]
dapply <- dataset[foto_mes == 202109]


# genero el modelo de Random Forest con la libreria ranger
set.seed(ksemilla_azar) # Establezco la semilla aleatoria

factorizado <- as.factor(dtrain$clase_ternaria)
dtrain[, clase_ternaria := factorizado]

# imputo los nulos, ya que ranger no acepta nulos
# Leo Breiman, ¿por que le temias a los nulos?
dtrain <- na.roughfix(dtrain)

setorder(dtrain, clase_ternaria) # primero quedan los BAJA+1, BAJA+2, CONTINUA

# genero el modelo de Random Forest llamando a ranger()
modelo <- ranger(
  formula = "clase_ternaria ~ .",
  data = dtrain,
  probability = TRUE, # para que devuelva las probabilidades
  num.trees = param$num.trees,
  mtry = param$mtry,
  min.node.size = param$min.node.size,
  max.depth = param$max.depth
)


# Carpinteria necesaria sobre  dapply
# como quiere la Estadistica Clasica, imputar nulos por separado
# ( aunque en este caso ya tengo los datos del futuro de anteman
#  pero bueno, sigamos el librito de estos fundamentalistas a rajatabla ...
dapply[, clase_ternaria := NULL]
dapply <- na.roughfix(dapply)


# aplico el modelo recien creado a los datos del futuro
prediccion <- predict(modelo, dapply)

# Genero la entrega para Kaggle
entrega <- as.data.table(list(
  "numero_de_cliente" = dapply[, numero_de_cliente],
  "Predicted" = as.numeric(prediccion$predictions[, "BAJA+2"] > 1 / 40),
  "probabilidad" = as.numeric(prediccion$predictions[, "BAJA+2"])
)) # genero la salida

# ordeno entrega por probabilidad descendente para permitir hacer el corte
setorder(entrega, -probabilidad)

# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/KA4310window/", showWarnings = FALSE)
archivo_salida <- "./exp/KA4310window/KA4310_001.csv"

# Genera la salida con distintos valores de envios
cortes <- seq(10000, 11000, by = 100)
for (envios in cortes) {
  entrega[, Predicted := 0L]
  entrega[1:envios, Predicted := 1L]
  
  fwrite(entrega[, list(numero_de_cliente, Predicted)],
         file = paste0('exp/KA4310window/KA4310', "_", envios, ".csv"),
         sep = ","
  )
}
