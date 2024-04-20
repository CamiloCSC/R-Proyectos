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
library(PerformanceAnalytics)
library(corrplot)
library(factoextra)
library(FactoMineR)


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
t_colombia <- data.frame(read_excel("C:\\Users\\camil\\Documents\\StatisticsLocal\\R-Proyectos\\Icfes\\Saber11\\Datasets\\TerritoriosColombia.xlsx",
                                    sheet = "CODIGOS2"))
mapa_colombia <- read_sf("C:\\Users\\camil\\Documents\\StatisticsLocal\\R-Proyectos\\Icfes\\Saber11\\Datasets\\Territorios_Colombia_Shapes\\Limite Departamental.shp")

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
  count(ESTU_GENERO); sex

plot_ly(type = "pie",
        data = sex,
        values=~n,
        labels=~ESTU_GENERO,
        marker = list(colors = c("pink", "skyblue"),
                      line = list(color = "white", width = 1))) %>% 
  layout(title = "Distribución de participantes según sexo", showlegend = T)

# PERIODO

# Se observa que el periodo corresponde al año 2023 indeoendientemente del
# semestre de la realización del examén que esta relacionad con el Calendario (A, B)
unique(saber11$PERIODO)

saber11 <- saber11 %>% 
  mutate(PERIODO = 2023)


# ESTU_ESTUDIANTE
# En este caso todos los participantes son estudiantes
saber11 %>% 
  group_by(ESTU_ESTUDIANTE) %>% 
  summarize(Conteo = n())

saber11 %>% 
  distinct(ESTU_ESTUDIANTE)
  

# ESTU_COD_RESIDE_DEPTO y ESTU_COD_RESIDE_MCPIO

# Los estudiantes residen en las 33 unidades territoriales de Colombia y
# en el extranjero, sumando asi 34 unidades territoriales
length(unique(saber11$ESTU_COD_RESIDE_DEPTO))

dim(saber11)

saber11 <- saber11 %>% 
  left_join(t_colombia, by=c("ESTU_COD_RESIDE_DEPTO"="COD_DEPTO", "ESTU_COD_RESIDE_MCPIO"="COD_MUN"))

length(unique(saber11$DEPARTAMENTO))

# Se presentan valores faltantes en las variables territoriales ya que
# se tomaron en cuenta territorios no municipalizados oficialmente
# en este caso unicamente se tomaran territorios municipalizados
# Por otro lado, se especficaran los casos del extranjero

print(miss_var_summary(saber11), n=5)

saber11 <- saber11 %>% 
  mutate(DEPARTAMENTO = ifelse(ESTU_COD_RESIDE_DEPTO=="99999" & is.na(DEPARTAMENTO), "Extranjero", DEPARTAMENTO)) %>% 
  mutate(MUNICIPIO = ifelse(ESTU_COD_RESIDE_MCPIO=="99999" & is.na(MUNICIPIO), "Extranjero", MUNICIPIO))

saber11 <- saber11 %>%
  drop_na(DEPARTAMENTO, MUNICIPIO)
  
saber11 <- saber11 %>% 
  rename(DEPTO_RESIDE = DEPARTAMENTO, MUN_RESIDE = MUNICIPIO)

ggplot(data = saber11, aes(x=DEPTO_RESIDE)) +
  geom_bar(color="blue4", fill="skyblue", stat = "count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

b <- saber11 %>% 
  group_by(ESTU_COD_RESIDE_DEPTO, DEPTO_RESIDE) %>% 
  summarize(Conteo = n()) %>% 
  data.frame()

# Los estudiantes que presentaron el examen en su mayoria viven en la ciudad
# de Bogotá, seguido del departamento de Antioquia, Valle del Cauca y Cundinamarca
plot_ly(data = b,
        type = "bar",
        x = ~DEPTO_RESIDE,
        y = ~Conteo,
        text = ~Conteo) %>% 
  layout(xaxis = list(categoryorder = "total descending"))

length(unique(saber11$MUN_RESIDE))

# Distribucion geografica

colnames(mapa_colombia)
colnames(b)

mapa_dep <- b %>% 
  left_join(select(mapa_colombia, COD_DEPART, geometry), by=c("ESTU_COD_RESIDE_DEPTO"="COD_DEPART"))


ggplot() +
  geom_sf(data = mapa_colombia) +
  geom_sf(data = mapa_dep, aes(geometry=geometry, fill=Conteo)) +
  scale_fill_gradient(low = "white", high = "purple")


# FAMI_ESTRATOVIVIENDA

# Se presentan casos en los que los estudiantes no poseen estrato, con fines
# practicos no se tomaran en cuenta estos casos
saber11 %>% 
  group_by(FAMI_ESTRATOVIVIENDA) %>% 
  summarize(n())

saber11 %>% 
  select(FAMI_ESTRATOVIVIENDA, ESTU_NACIONALIDAD, DEPTO_RESIDE) %>% 
  filter(FAMI_ESTRATOVIVIENDA == "Sin Estrato") %>% 
  slice_head(n=5)

saber11 <- saber11 %>% 
  filter(FAMI_ESTRATOVIVIENDA != "Sin Estrato")

ggplot() +
  geom_bar(data = saber11,
               aes(x = FAMI_ESTRATOVIVIENDA,
                   fill = FAMI_ESTRATOVIVIENDA))

dim(saber11)


# COLE_NATURALEZA

# La naturaleza de los colegios es Oficial (Publico) y No Oficial (Privado)
unique(saber11$COLE_NATURALEZA)

saber11 <- saber11 %>% 
  mutate(COLE_NATURALEZA = str_to_title(COLE_NATURALEZA))

b1 <- saber11 %>% 
  count(COLE_NATURALEZA); b1

ggplot() +
  geom_point(data = b1, aes(x = COLE_NATURALEZA,
                            y = n,
                            fill=COLE_NATURALEZA,
                            color = COLE_NATURALEZA), size=3.5) +
  geom_text(data = b1, aes(x = COLE_NATURALEZA,
                           y = n,
                           label=n))

# COLE_CALENDARIO

# Se observa otro tipo de Calendario diferente al A y B, con lo cual no se
# tomarán en cuenta estos registros
p2 <- saber11 %>% 
  group_by(COLE_CALENDARIO) %>% 
  summarize(Conteo = n()); p2

saber11 <- saber11 %>% 
  filter(COLE_CALENDARIO != "OTRO")

plot_ly(data = p2,
        type="pie",
        values=~Conteo,
        labels=~paste("Calendario", COLE_CALENDARIO)) %>% 
  layout(title="Distribución de estudiantes por tipo de Calendario")


# COLE_CARACTER

saber11 %>% 
  group_by(COLE_CARACTER) %>% 
  summarize(Conteo = n())

saber11 %>% 
  filter(COLE_CARACTER == "NO APLICA") %>% 
  slice_tail(n=5)

saber11 <- saber11 %>% 
  mutate(COLE_CARACTER = str_to_title(COLE_CARACTER))


# COLE_AREA_UBICACION

saber11 %>% 
  group_by(COLE_AREA_UBICACION) %>% 
  summarize(n())


# COLE_JORNADA

b1 <- saber11 %>% 
  group_by(COLE_JORNADA) %>% 
  summarize(Conteo = n())

plot_ly(data = b1,
        type = "bar",
        y = ~COLE_JORNADA,
        x = ~Conteo,
        marker = list(color = "darkgreen"),
        text = ~Conteo) %>% 
  layout(title = list(text = "Cantidad de estudiantes según tipo de jornada", y = 0.98))

saber11 <- saber11 %>% 
  mutate(COLE_JORNADA = str_to_title(COLE_JORNADA))

# Detectamos presencia de espacios al prnicipio o final de los valores 

str_detect(saber11$COLE_JORNADA, pattern = "\\s^") %>% sum()
str_detect(saber11$COLE_JORNADA, pattern = "$\\s") %>% sum()
str_detect(saber11$COLE_JORNADA, pattern = "\\W") %>% sum()


# PUNTAJE GLOBAL

# Los puntajes evidencian coherencia ya que deben estar entre 0 y 500, y
# se encuentran entre 89 y 500 puntos
plot_ly(data = saber11,
        type = "box",
        y = ~PUNT_GLOBAL,
        marker = list(color = "grey55"),
        line = list(color = "grey55"),
        color = I("grey55"))

plot_ly(data = saber11,
        type = "violin",
        x = ~PUNT_GLOBAL,
        box = list(visible = T))


# PUNTAJE POR ASIGNATURA EVALUADA

dim(saber11)

puntaje <- saber11 %>% 
  select(starts_with("PUNT_"), -PUNT_GLOBAL) %>% 
  drop_na() %>% 
  slice_head(n=10000)

chart.Correlation(puntaje, pch = 19)

puntaje <- saber11 %>% 
  select(starts_with("PUNT_"), -PUNT_GLOBAL) %>% 
  drop_na()

glimpse(puntaje)

corrplot(cor(puntaje), method = "number", type = "upper", tl.cex = 0.4, tl.col = "black")

pca <- PCA(puntaje, scale.unit = T, graph = F)

pca$eig
pca$var$coord
pca$var$contrib
fviz_screeplot(pca)

plot.PCA(pca, axes = c(1, 2), choix = "var")




