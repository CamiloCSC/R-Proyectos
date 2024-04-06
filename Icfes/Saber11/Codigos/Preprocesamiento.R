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
sapply(datos, function(i) dim(get(i))) # Dimension de los conjuntos de datos


# ENTENDIMIENTO DE LOS DATOS ---------------------------------------------------

# 1) INTEGRACIÓN













