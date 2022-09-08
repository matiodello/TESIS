#################### EXPLORACIÓN BLAS CORREA ################################


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
library(janitor)
library(treemap)


### CARGA DE BBDD:


# Cargamos el data set limpio donde se encuentran todas las publicaciones referidas al Caso Blas Correa:

blas_media <- read_xlsx("blas_correas.xlsx")

## Corpus con palabras "vacías":

my_stopwords <- read_csv("my_stopwords.csv")

# Exploración y limpieza --------------------------------------------------



glimpse(blas_media)


## Se observan gran cantidad de valores nulos asique se procede a realizar una limpieza:

blas_media <- blas_media[!is.na(blas_media$`Vertical 1`),]


## Los nombres de las variables contienen caracteres especiales. El paquete "Janitor" nos permite normalizar los nombres de todas las variables:

blas_media <- clean_names(blas_media)

## Comprobamos:
colnames(blas_media)




df <- blas_media %>% 
  select(vertical_1:link_del_comentario)



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

## Ordenamos la tabla de acuerdo a la cantidad:

tab2 <- arrange(tab2, desc(n))




# PROCESAMIENTO DE LENGUAJE -----------------------------------------------


## Tokkenizamos por palabras para quedarnos con las unidades mínimas de lenguaje y poder analizar cuáles son las palabras más utilizadas en:

## POSTEOS

tokken_posteos <- df %>% unnest_tokens(word, cuerpo_del_post) %>% 
  anti_join(my_stopwords) %>%
  filter(!str_detect(`word`, paste(c("blas", "correas"), collapse = '|'))) %>% 
  count(word, sort = TRUE)


## CUERPO DE NOTICIA

tokken_noticias <- df %>% unnest_tokens(word, cuerpo_de_la_noticia) %>% 
  anti_join(my_stopwords) %>%
  filter(!str_detect(`word`, paste(c("blas", "correas", "eldoce.tv"), collapse = '|'))) %>% 
  count(word, sort = TRUE)


## Visualizaciones:

## Reducimos la muestra a las 20 principales palabras:

tokken_posteos_1 <- tokken_posteos %>% 
  top_n(20)

## Graficamos a través de un histograma:

tokken_posteos_1 %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot( aes(x=word, y=n)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()


## Repetimos el proceso para el corpus de noticias:

tokken_noticias_1 <- tokken_noticias %>% 
  top_n(20)

## Graficamos a través de un histograma:

tokken_noticias_1 %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot( aes(x=word, y=n)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()


# RED DE RELACIONES POSTEOS -------------------------------------------------------


noticias_oracion <- df %>% 
  unnest_tokens(oracion, cuerpo_de_la_noticia, token = "sentences")


noticias_df <- noticias_oracion %>%
  filter(str_detect(oracion, paste(c("crimen"),collapse = '|')))

noticias_dfm <- noticias_df %>%
  unnest_tokens(word, oracion, drop = FALSE) %>%
  filter(!word %in% my_stopwords$word,
         !word %in% str_remove_all(my_stopwords$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^[0-9]*$"),
         !str_detect(word, "minutes")) %>%
  count(`oracion`, word) %>%
  cast_dfm(`oracion`, word, n)

# Nos quedamos con un número finito de palabrs sobre las que realizar las relacioness
top_words_timelines <- names(topfeatures(noticias_dfm, 20))


#Hacemos una matriz de palabras (palabras que aparecen juntas en el mismo texto/oracion): fcm hace una matriz de correlacion de palabras
words_timelines_fcm <- fcm(noticias_dfm)

#Visualizamos las relaciones
words_fcm <- fcm_select(words_timelines_fcm, pattern = top_words_timelines) #Me quedo con las palabras 
#que estan en el top 50

library(quanteda.textplots)

textplot_network(words_fcm, min_freq = 0.1, edge_color = "#EF6030", edge_alpha = 0.5, edge_size = 1)
