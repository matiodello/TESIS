#################### EXPLORACIÓN JOAQUÍN PAREDES ################################


#################### Carga de librerías ------------------------------------------------------

## PARA LA VERSIÓN FINAL HAY QUE DETALLAR QUÉ LIBRERÍAS FUERON UTILIZADAS Y PARA QUÉ PROPOSITO EN ESPECÍFICO.

library(tibble)
library(tidyverse)
library(dplyr)
library(lubridate) # Manejo y transformación de datos FECHA
library(hrbrthemes) # temas adicionales, componentes de temas y utilidades para 'ggplot2'
library(viridis) # escalas de color ggplot
library(funModeling) # caja de herramientas de análisis exploratorio de datos y preparación de datos
library(tidytext) # preparación de datos en formato texto
library(stringr) # Funciones diseñadas para hacer que trabajar con cadenas 
library(ggplot2) # Paquete de visualizaciones
library(scales) # infraestructura de escalado interna utilizada por ggplot2
library(igraph) # Análisis y Visualización de Redes
library(ggraph) # Implementación de gramática de gráficos para gráficos y redes
library(rtweet) # Cliente R para acceder a las API REST y stream de Twitter
library(quanteda) # Paquete R para administrar y analizar datos textuales 
library(stringi) # Herramientas de procesamiento de cadenas de caracteres/texto/lenguaje natural para la búsqueda de patrones 
library(vroom) # leer y escribir datos (como 'csv', 'tsv' y 'fwf') rápidamente
library(readr) # leer y escribir datos (como 'csv', 'tsv' y 'fwf') rápidamente
library(readxl) # leer archivos de Excel
library(knitr) # generación de informes dinámicos en R utilizando técnicas de programación literaria.
library(openxlsx) # leer, escribir y editar archivos xlsx
library(simplevis) # Visualizaciones ggplot simplificadas
library(widyr) #Encontrar correlaciones entre textos
library(ggraph) #plottear redes/mapas
library(igraph) #plottear redes/mapas
library(xlsx)
library(janitor)

### CARGA DE BBDD:


# Cargamos el data set limpio donde se encuentran todas las publicaciones referidas al Caso JOAQUÍN PAREDES:

paredes_media <- read_csv("joaquin_paredes.csv")






# Exploración y limpieza --------------------------------------------------



glimpse(paredes_media)


## Se observan gran cantidad de valores nulos asique se procede a realizar una limpieza:

paredes_media <- paredes_media[!is.na(paredes_media$`Vertical 1`),]


df <- clean_names(paredes_media)


######### CRONOLOGÍA --------------------------------------------------------------


  
  
## Primero agregamos una nueva variable que sea propiamente la fecha:
  

df$date <- as.Date(df$fecha)               


# Luego generamos otra variable que contenga solamente año y mes para  poder agrupar por años
df$mes_anio <- format(df$date, "%m/%Y")


# Ahora vamos a ver la fecha mínima y máxima para tener desde y hasta cuándo van las fechas de nuestros datos.

fechas_min.max <- df %>% 
  summarise(FECHA_MAX = max(date),
            FECHA_MIN = min(date))

## Encontramos que las fechas van desde ocurrido el asesinato "2020-10-25" hasta "2022-06-08" 


## El siguiente paso es observar cómo es la distribución de publicaciones según fecha:

## Primero se crea una tabla para agrupar por mes y año la cantidad y el porcentaje que representa cada fecha en el total de publicaciones:

tab2 <- df %>% 
  group_by(mes_anio) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = round(n/sum(n)*100, 2))

## Luego se grafica a través de un gráfico de líneas:

ggplot(data = tab1, aes(x = tipo_delito, y = perc))+
  geom_bar(stat = "identity", fill = "maroon", width = 0.7)+
  geom_text(aes(label = perc), size = 5, hjust = 0.5, vjust = -0.25)+
  theme_minimal()+
  labs(title = "Distribucion de tipos de violencia",
       x = "",y = "Porcentaje",
       caption = "Instrumenta Data Analysis")+
  ylim(c(0,55))




