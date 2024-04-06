# DIRECTORIO -------------------------------------------------------------------

setwd("C:/Users/camil/Documents/StatisticsLocal/R-Proyectos/Icfes/Saber11/Codigos")


# LIBRERIAS --------------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(naniar)
library(ggplot2)
library(plotly)


# CONJUNTOS DE DATOS -----------------------------------------------------------

#file.choose()

# Datos Saber 11 - 2023 1
saber20231 <- data.frame(read_delim("C:\\Users\\camil\\Documents\\StatisticsLocal\\R-Proyectos\\Icfes\\Saber11\\Datasets\\SB11_20231.TXT",
                                    delim = "¬"))

# Datos Saber 11 - 2023 2
saber20232 <- data.frame(read_delim("C:\\Users\\camil\\Documents\\StatisticsLocal\\R-Proyectos\\Icfes\\Saber11\\Datasets\\SB11_20232.TXT",
                                    delim = "¬"))

datos <- c("saber20231", "saber20232")
sapply(datos, function(i) dim(get(i))) # Dimensión de los conjuntos de datos


# ENTENDIMIENTO DE LOS DATOS ---------------------------------------------------

# 1) ADQUISICIÓN

# Los datos fueron descargados en formato txt por parte del Instituto Colombiano
# para la Evaluación de la Educación -Icfes. (marzo de 2024). 
# DataIcfes: Repositorio de Datos Abiertos del Icfes. 04.Saber11 [Conjunto de datos].
# Recuperado de https://bitly.ws/3f3YC


# 2) INTEGRACIÓN

all(colnames(saber20231)==colnames(saber20232))
# Hay presencia de variables posiblemente con nombres diferentes
# Verificación de las variables a partir del diccionario de datos

setdiff(colnames(saber20231), colnames(saber20232))
setdiff(colnames(saber20232), colnames(saber20231))

# "ESTU_GENERACION" no debería estar presente en estos periodos del examén 
# No hay registro de una variable llamada "ESTU_LENGUANATIVA"
# No hay registro de una variable llamada "SEED_CODIGOMEN "
# No hay registro de una variable llamada "SEED_NOMBRE"

saber20231 %>% 
  select("SEED_CODIGOMEN", "SEED_NOMBRE") %>% 
  slice_head(n=5) # Las variables parecen correponder a unidades territoriales

saber20232 %>% 
  select("ESTU_GENERACION", "ESTU_LENGUANATIVA") %>% 
  slice_head(n=5) 

saber20232 %>% 
  select(ESTU_LENGUANATIVA) %>% 
  miss_var_summary() # El 98.3% de los datos son faltantes en la variable "ESTU_LENGUANATIVA"


# Se procede a no tomar en cuenta estas variables
saber20231 <- saber20231 %>% 
  select(!c(SEED_CODIGOMEN, SEED_NOMBRE)) %>% 
  select(sort(colnames(.)))

saber20232 <- saber20232 %>% 
  select(!c(ESTU_GENERACION, ESTU_LENGUANATIVA)) %>% 
  select(sort(colnames(.)))

# Confirmación de las variables
View(data.frame(saber20231 = colnames(saber20231),
           saber20232 = colnames(saber20232),
           Presente = colnames(saber20231) == colnames(saber20232)))

all(colnames(saber20231)==colnames(saber20232))







