## OPERATIVIZACIÓN DE VARIABLES:





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
library(janitor) #Limpieza de nombres
library(editData)


### CARGA DE BBDD:


# Cargamos el data set limpio donde se encuentran todas las publicaciones referidas al Caso Blas Correa:

blas_media <- read_csv("D:/UNVM/Rstudio - TESIS/TESIS/CORREAS/blas_correas.csv")




## Corpus con palabras "vacías":


my_stopwords <- read_csv("D:/UNVM/Rstudio - TESIS/TESIS/my_stopwords.csv")


# Exploración y limpieza --------------------------------------------------



glimpse(blas_media)


## Se observan gran cantidad de valores nulos asique se procede a realizar una limpieza:

blas_media <- blas_media[!is.na(blas_media$`Vertical 1`),]


## Los nombres de las variables contienen caracteres especiales. El paquete "Janitor" nos permite normalizar los nombres de todas las variables:

blas_media <- clean_names(blas_media)

## Comprobamos:
colnames(blas_media)


## Encontramos que existen columnas sin datos ni varables:

df_blas <- blas_media %>% 
  select(vertical_1:comentarios_28)


# Comprobamos y filtramos observaciones repetidas a través de la función Duplicated():


df_blas <- df_blas[!duplicated(df_blas$noticia_link), ]


df_blas <- distinct(df_blas, noticia_link, .keep_all = TRUE)

rm(blas_media)



######### CRONOLOGÍA --------------------------------------------------------------




## Primero agregamos una nueva variable que sea propiamente la fecha:


df_blas$date <- as.Date(df_blas$fecha)               


# Luego generamos otra variable que contenga solamente año y mes para  poder agrupar por años
df_blas$mes_anio <- format(df_blas$date, "%m/%Y")


# Ahora vamos a ver la fecha mínima y máxima para tener desde y hasta cuándo van las fechas de nuestros datos.

fechas_min.max.blas <- df_blas %>% 
  summarise(FECHA_MAX = max(date),
            FECHA_MIN = min(date))

## Encontramos que las fechas van desde ocurrido el asesinato "2020-10-25" hasta "2022-06-08" 


## El siguiente paso es observar cómo es la distribución de publicaciones según fecha:


# FRECUENCIA DE PUBLICACIÓN -----------------------------------------------



## Primero se crea una tabla para agrupar por mes y año la cantidad y el porcentaje que representa cada fecha en el total de publicaciones:

tabla_freq.publicacion.blas <- df_blas %>% 
  group_by(mes_anio) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = round(n/sum(n)*100, 2))

## Ordenamos la tabla de acuerdo a la cantidad:

tabla_freq.publicacion.blas <- arrange(tabla_freq.publicacion.blas, desc(n))



tabla_freq.publicacion.blas %>%
  mutate(mes_anio = fct_reorder(mes_anio, n)) %>%
  ggplot( aes(x=mes_anio, y= perc)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("fecha") +
  ylab("porcentaje")+
  theme_bw()



## Esta dimensión, la cual mide la distribución de publicaciones totales sobre el caso Blas Correas del ElDoce.Tv en el tiempo, nos señala que el mayor porcentaje de publicaciones se encuentran en el mes de sucedido el hecho mientras que el segundo período de mayor ritmo de publicación es exactamente un año después, Agosto del 2021. 

## queda pendiente observar la distribución por semana o día quizás.


# INTERACCIÓN -------------------------------------------------------------



## Agrupación de interacción según fecha:

tabla_interaccion.fecha.blas <- df_blas %>% 
  group_by(mes_anio) %>% 
  summarise(MIN_INTERAC = min(interacciones),
            MAX_INTERAC = max(interacciones),
            MEDIA_INTERACC = mean(interacciones))

## La interacción puede ser definida como la suma de todas las reacciones (me gusta, me divierte, me enoja, etc + comentarios + compartidos)



publicacion_interaccion.blas <- df_blas %>% 
  group_by(noticia_link) %>% 
  summarise(interacciones = sum(interacciones, na.rm=TRUE)) %>% 
  arrange(desc(interacciones))



# COMENTARIOS -------------------------------------------------------------



publicaciones_comentarios.blas <- df_blas %>% 
  group_by(noticia_link) %>% 
  summarise(comentarios_23 = sum(comentarios_23, na.rm=TRUE)) %>% 
  arrange(desc(comentarios_23))



# COMPARTIDOS -------------------------------------------------------------

publicaciones_compartidos.blas <- df_blas %>% 
  group_by(noticia_link) %>% 
  summarise(compartidos = sum(compartidos, na.rm=TRUE)) %>% 
  arrange(desc(compartidos))



## Se concluye que las publicaciones con mayor volumen de compartidos y con mayor volumen de interacciones totales corresponden a la misma publicación:

# https://www.facebook.com/eldocetv/posts/pfbid02hJKJcQWYuDfqmGXQoRrvirqUU7SXiVBUToyreY2FbmGJnSBiAZVVKRquf4hyyqJul

## "Movilización en la ciudad de Córdoba. El comunicado difundido convoca "para marchar en silencio y de forma pacífica", y pide por el uso de barbijo y el cumplimiento de la distancia social." 10/08/2020


## En cuanto a las publicaciones con mayor volumen de comentarios también corresponde a una publicación referida a movilizaciones por pedidos de justicia:

# " #JusticiaPorBlas: marcha a una semana del crimen. Marcha por Blas A una semana del crimen del adolescente a manos de policías, Córdoba pidió #JusticiaPorBlas." 13/08/2020




# EMOCIONALIDADES ---------------------------------------------------------

ME_GUSTA.blas <- df_blas %>% 
  group_by(noticia_link) %>% 
  summarise(me_gusta = sum(me_gusta, na.rm=TRUE)) %>% 
  arrange(desc(me_gusta))

# https://www.facebook.com/eldocetv/posts/pfbid02hJKJcQWYuDfqmGXQoRrvirqUU7SXiVBUToyreY2FbmGJnSBiAZVVKRquf4hyyqJul

## "Movilización en la ciudad de Córdoba. El comunicado difundido convoca "para marchar en silencio y de forma pacífica", y pide por el uso de barbijo y el cumplimiento de la distancia social." 10/08/2020


SIMPATIA.blas <- df_blas %>% 
  group_by(noticia_link) %>% 
  summarise(simpatia = sum(simpatia, na.rm=TRUE)) %>% 
  arrange(desc(simpatia))

# https://www.facebook.com/eldocetv/posts/pfbid02LN7pb9NFHgaXwa11DCnKSEjzSmhexAh9aZmpPQaWNbYJnHhuEycK69YFrpKW3Bzcl

## ""Es una señal de que Blas está conmigo" Según los médicos, la mujer quedó embarazada días antes del crimen de su hijo. Habló en exclusiva con El Doce y quiere justicia." 30/09/2020


ASOMBRO.blas <- df_blas %>% 
  group_by(noticia_link) %>% 
  summarise(asombro = sum(asombro, na.rm=TRUE)) %>% 
  arrange(desc(asombro))


# https://www.facebook.com/eldocetv/posts/pfbid02MEFDJAnoQjaGfSf1pEQsDdWmB1KN12NtKrNgMQUXWpv6bYdjkphrAAAyj5sZHSaLl

## "[URGENTE] Detuvieron a una de las policías que acompañaba a los dos agentes que mataron a Blas Correas. Confesó que plantó el arma" 12/08/2020


DIVERSION.blas <- df_blas %>% 
  group_by(noticia_link) %>% 
  summarise(diversion = sum(diversion, na.rm=TRUE)) %>% 
  arrange(desc(diversion))

# https://www.facebook.com/eldocetv/posts/pfbid032f75E2wVUEonURBMtE3xG2RYeyhMhqdEjmgC14Z86cVyKDNiTfFYnqfYeVeKCk3tl

## "Juan Cruz siente culpa. El psiquiátra del amigo de Blas Correas, que manejaba el auto, dio detalles de cuál es el estado emocional." 10/08/2020


DESCREIMIENTO.blas <- df_blas %>% 
  group_by(noticia_link) %>% 
  summarise(descreimiento = sum(descreimiento, na.rm=TRUE)) %>% 
  arrange(desc(descreimiento))

# https://www.facebook.com/eldocetv/posts/pfbid02jjXP9Dk3ShYXf3fRMTkpjg5UP8v2d1mgeUHFUHkXeQazcwLEW3qF4MJQ1AMVNPpvl

## "Fernanda rompió el silencio. Javier Alarcón es uno de los tres agentes imputados por el crimen de Blas, "Mi marido no es una persona que sale a matar gente inocente", dijo la mujer." 13/08/2020


TRISTEZA.blas <- df_blas %>% 
  group_by(noticia_link) %>% 
  summarise(tristeza = sum(tristeza, na.rm=TRUE)) %>% 
  arrange(desc(tristeza))

# https://www.facebook.com/eldocetv/posts/pfbid0v92BRcwttEfGquckCCT629y5cax7b2bBuRmQFpyHxRNSxe6uoUhveHLLkArXsRGSl

## " El dolor del periodista por la muerte de Valentino Blas Correas. Mario Massaccesi contó que el joven es familiar suyo y en medio de la tristeza por lo ocurrido pidió justicia. También lo recordó con un emotivo posteo en sus redes sociales." 7/08/2020

# https://www.facebook.com/eldocetv/videos/598407367498733/

##  " #JusticiaPorBlas: marcha a una semana del crimen. Marcha por Blas A una semana del crimen del adolescente a manos de policías, Córdoba pidió #JusticiaPorBlas." 13/08/2020


ENOJO.blas <- df_blas %>% 
  group_by(noticia_link) %>% 
  summarise(enojo = sum(enojo, na.rm=TRUE)) %>% 
  arrange(desc(enojo))

# https://www.facebook.com/eldocetv/posts/pfbid02MEFDJAnoQjaGfSf1pEQsDdWmB1KN12NtKrNgMQUXWpv6bYdjkphrAAAyj5sZHSaLl

## "[URGENTE] Detuvieron a una de las policías que acompañaba a los dos agentes que mataron a Blas Correas. Confesó que plantó el arma" 12/08/2020






# PROCESAMIENTO DE LENGUAJE - NOTICIAS ------------------------------------

tokken_noticas.blas <- df_blas %>% 
  unnest_tokens(word, cuerpo_de_la_noticia) %>% 
  filter(!word %in% my_stopwords$word,
         !word %in% str_remove_all(my_stopwords$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^[0-9]*$"),
         !str_detect(word, "eldoce.tv")) %>% 
  count(word, sort = TRUE)


bigramas_noticas.blas <- df_blas %>% 
  unnest_tokens(word, cuerpo_de_la_noticia) %>% 
  filter(!word %in% my_stopwords$word,
         !word %in% str_remove_all(my_stopwords$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^[0-9]*$"),
         !str_detect(word, "eldoce.tv")) %>% 
  count(word, sort = TRUE)

bigramas_noticas.blas <- df_blas %>%
  unnest_tokens(bigram, cuerpo_de_la_noticia, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% my_stopwords$word,
         !word2 %in% my_stopwords$word) %>% 
  unite(bigram, word1, word2, sep = " ") 

bigramas_noticas.blas <- bigramas_noticas.blas %>% 
  select(etiqueta_3, bigram)

tabl1 <- bigramas_noticas.blas %>% 
  count(bigram, sort = TRUE) %>% 
  ungroup()


# PROCESAMIENTO DE LENGUAJE - POSTEOS -------------------------------------


tokken_posteos.blas <- df_blas %>% 
  unnest_tokens(word, cuerpo_del_post) %>% 
  filter(!word %in% my_stopwords$word,
         !word %in% str_remove_all(my_stopwords$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^[0-9]*$"),
         !str_detect(word, "eldoce.tv")) %>% 
  count(word, sort = TRUE)

bigramas_posteos.blas <- df_blas %>%
  unnest_tokens(bigram, cuerpo_del_post, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% my_stopwords$word,
         !word2 %in% my_stopwords$word) %>% 
  unite(bigram, word1, word2, sep = " ") 

bigramas_posteos.blas <- bigramas_posteos.blas %>% 
  select(etiqueta_3, bigram)

tabl2 <- bigramas_posteos.blas %>% 
  count(bigram, sort = TRUE) %>% 
  ungroup()




# PROCESAMIENTO DE LENGUAJE - COMENTARIOS (GRAL) --------------------------

tokken_coments.blas <- df_blas %>% 
  unnest_tokens(word, comentarios_28) %>% 
  filter(!word %in% my_stopwords$word,
         !word %in% str_remove_all(my_stopwords$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^[0-9]*$"),
         !str_detect(word, "eldoce.tv")) %>% 
  count(word, sort = TRUE)


bigramas_comentarios.blas <- df_blas %>%
  unnest_tokens(bigram, comentarios_28, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% my_stopwords$word,
         !word2 %in% my_stopwords$word) %>% 
  unite(bigram, word1, word2, sep = " ") 

bigramas_comentarios.blas <- bigramas_comentarios.blas %>% 
  select(etiqueta_3, bigram)

tabl3 <- bigramas_comentarios.blas %>% 
  count(bigram, sort = TRUE) %>% 
  filter(str_detect(bigram, "[a-z]"),
        !str_detect(bigram, "^[0-9]*$")) %>% 
  ungroup()





## Después de hacer la tokkenización general, tokkenizamos por publicación específicas.

tokken_coments.blas_1 <- df_blas %>% 
  filter(noticia_link == "https://www.facebook.com/eldocetv/videos/598407367498733/") %>% 
  unnest_tokens(word, comentarios_28) %>% 
  filter(!word %in% my_stopwords$word,
         !word %in% str_remove_all(my_stopwords$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^[0-9]*$"),
         !str_detect(word, "eldoce.tv")) %>% 
  count(word, sort = TRUE)

