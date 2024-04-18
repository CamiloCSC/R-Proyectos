# DIRECTORIO -------------------------------------------------------------------

setwd("C:/Users/camil/Documents/StatisticsLocal/R-Proyectos/Icfes/Saber11/Codigos")


# LIBRERIAS --------------------------------------------------------------------

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(naniar)
library(ggplot2)
library(plotly)
library(sf)
library(mice)


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

mapa_mundo <- read_sf("C:\\Users\\camil\\Documents\\StatisticsLocal\\R-Proyectos\\Icfes\\Saber11\\Datasets\\Country_Shapes\\Country_Shapes.shp")
iso_paises <- data.frame(read_excel("C:\\Users\\camil\\Documents\\StatisticsLocal\\R-Proyectos\\Icfes\\Saber11\\Datasets\\Codigos_ISO_Paises.xlsx"))


# ENTENDIMIENTO DE LOS DATOS ---------------------------------------------------

# 1 ADQUISICIÓN

# Los datos fueron descargados en formato txt por parte del Instituto Colombiano
# para la Evaluación de la Educación -Icfes. (marzo de 2024). 
# DataIcfes: Repositorio de Datos Abiertos del Icfes. 04.Saber11 [Conjunto de datos].
# Recuperado de https://bitly.ws/3f3YC


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


# 1.1 INTEGRACIÓN

saber11 <- saber20231 %>% 
  bind_rows(saber20232)

dim(saber11)

vars <- as.vector(read_csv("~/StatisticsLocal/R-Proyectos/Icfes/Saber11/Info/Variables_Analisis.txt"))

saber11 <- saber11 %>% 
  select(vars$VARIABLES)


# 1.2 ANÁLISIS EXPLORATORIO DE DATOS (EDA)

colnames(saber11)

View(saber11)

# Se cuenta con 563853 registros de 28 variables
dim(saber11)


# ANÁLISIS ESTADÍSTICO DE DATOS FALTANTES
# La variable COLE_BILINGUE es la que presenta la mayor cantidad de datos faltantes
# siendo aproximadamente el 18.4% del total (563.853).
print(miss_var_summary(saber11), n=10)

gg_miss_var(saber11) +
  labs(title = "Distribución de datos faltantes por variable",
       y = "Datos Faltantes")

# Busqueda de otros patrones de datos faltnes tales como:
# NaN, N/A, NA los cuales no estan presentes
miss_scan_count(saber11, list(c("^NA$", "^NaN$", "^N/A$")))

# Se presentan 20 variables de 26 que no presentan datos faltantes lo que corresponde
# al 76.9% del total de variables. Hay 2 variables que presentan 2.501 datos faltantes
# lo que corresponde al 7.69% de las variables aproximadamente.
miss_var_table(saber11)

# Dada la alta proporción de datos faltantes en la variable COLE_BILINGUE, no se
# tomará en cuenta esta variable
# En el caso de las variables FAMI_ESTRATOVIVIENDA, COLE_CARACTER Y ESTU_GENERO
# se optará por la eliminación de estos registros

saber11 <- saber11 %>% 
  select(-COLE_BILINGUE) %>% 
  drop_na(FAMI_ESTRATOVIVIENDA, COLE_CARACTER, ESTU_GENERO)

# En ese orden de ideas las variables PUNT_INGLES y DESEMP_INGLES presentan
# 1.589 datos faltantes en donde se procede a validar si corresponden a los
# mismos registros
miss_var_summary(saber11)

gg_miss_var(saber11) +
  labs(title = "Distribución de datos faltantes por variable",
       y = "Datos Faltantes")

# Efectivamente corresponden a los mismos registros
dim(saber11)
md.pattern(subset(saber11, select = c("PUNT_INGLES", "DESEMP_INGLES")))


# NACIONALIDAD DE LOS PARTICIPANTES
# Los individuos que presentaron el examén provienen de 52 nacionalidades
saber11 %>% 
  distinct(ESTU_NACIONALIDAD) %>% 
  count()

# Al ser una prueba aplicada en Colombia se observa que el 98.48% de personas
# son originarias de dicho pais, seguido de Venezuela (1.47%) y Ecuador (0.01%)
saber11 %>% 
  count(ESTU_NACIONALIDAD) %>% 
  arrange(desc(n)) %>% 
  mutate(porc = round(n/sum(n)*100, 2))

# Presencia de espacios al principio, entre o al final
str_detect(saber11$ESTU_NACIONALIDAD, pattern = "\\s{2,}") %>% sum()
str_detect(saber11$ESTU_NACIONALIDAD, pattern = "\\s$") %>% sum()
str_detect(saber11$ESTU_NACIONALIDAD, pattern = "^\\s") %>% sum()

# Presencia de simbolos
str_detect(saber11$ESTU_NACIONALIDAD, pattern = "[:punct:]") %>% sum()
saber11 %>% 
  select(ESTU_NACIONALIDAD) %>% 
  filter(str_detect(ESTU_NACIONALIDAD, "[:punct:]"))

# Presencia de digitos
str_detect(saber11$ESTU_NACIONALIDAD, pattern = "\\d") %>% sum()

# Transformación de las nacionalidades
saber11 <- saber11 %>% 
  transform(ESTU_NACIONALIDAD = str_to_title(ESTU_NACIONALIDAD))

# Recodificación

saber11 <- saber11 %>% 
  mutate(ESTU_NACIONALIDAD = case_when(ESTU_NACIONALIDAD == "Haiti" ~ "Haití",
                                       ESTU_NACIONALIDAD == "Corea Del Sur" ~ "Corea del Sur",
                                       ESTU_NACIONALIDAD == "Corea Del Norte" ~ "Corea del Norte",
                                       ESTU_NACIONALIDAD == "Países Bajos - Holanda" ~ "Países Bajos",
                                       ESTU_NACIONALIDAD == "Malawi" ~ "Malaui",
                                       .default = ESTU_NACIONALIDAD))

# Integración de código ISO 3166-1 alpha-3
saber11 <- saber11 %>%
  left_join(select(iso_paises, NOMBRE_COMUN, ALFA3), by=c("ESTU_NACIONALIDAD"="NOMBRE_COMUN"))

nac <- mapa_mundo %>% 
  filter(ALFA3 %in% saber11$ALFA3)

# No se presentan nacionalidades "faltantes"
saber11 %>% 
  filter(!ALFA3 %in% nac$ALFA3) %>% 
  select(ESTU_NACIONALIDAD, ALFA3) %>% 
  distinct()

# Distribución geográfica
ggplot() +
  geom_sf(data = mapa_mundo, color="white", fill="grey70") +
  geom_sf(data = nac, color="white", fill="darkblue") +
  labs(title = "Globalización Educativa: Orígenes geográficos de los participantes en la Prueba Saber 11 (2023).",
       x = "Longitud", y = "Latitud")


# SEXO DE LOS PARTICIPANTES

# Las mujeres presentan una participación superior al de los hombres, ya que
# 266.186 (53.9%) presentaron el examén Saber 11, mientras que 227.489 (46.1%) de
# los hombres realizaron dicho examén
sex <- saber11 %>% 
  count(ESTU_GENERO)

plot_ly(type = "pie",
        data = sex,
        values=~n,
        labels=~ESTU_GENERO,
        marker = list(colors = c("pink", "skyblue"),
                      line = list(color = "white", width = 1))) %>% 
  layout(title = "Pie Charts with Subplots", showlegend = T)













