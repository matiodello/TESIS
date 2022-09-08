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
library(janitor) # Limpieza de variables
library(editData) #an RStudio addin for editing a ‘data.frame’ or a ‘tibble’.



### CARGA DE BBDD:


# Cargamos el data set limpio donde se encuentran todas las publicaciones referidas al Caso Blas Correa:

joaquin_paredes <- read_csv("joaquin_paredes.csv")

## Corpus con palabras "vacías":

my_stopwords <- read_csv("D:/UNVM/TESIS/TESIS_RSTUDIO/my_stopwords.csv")

# Exploración y limpieza --------------------------------------------------



glimpse(joaquin_paredes)


## Se observan gran cantidad de valores nulos asique se procede a realizar una limpieza:

joaquin_paredes <- joaquin_paredes[!is.na(joaquin_paredes$`Vertical 1`),]


## Los nombres de las variables contienen caracteres especiales. El paquete "Janitor" nos permite normalizar los nombres de todas las variables:

joaquin_paredes <- clean_names(joaquin_paredes)

## Comprobamos:
colnames(joaquin_paredes)


## Encontramos que existen columnas sin datos ni varables:

df_paredes <- joaquin_paredes %>% 
  select(vertical_1:comentarios_28) %>% 
  filter(diario == "ElDoceTv")



######### CRONOLOGÍA --------------------------------------------------------------




## Primero agregamos una nueva variable que sea propiamente la fecha:


df_paredes$date <- as.Date(df_paredes$fecha)               


# Luego generamos otra variable que contenga solamente año y mes para  poder agrupar por años
df_paredes$mes_anio <- format(df_paredes$date, "%m/%Y")


# Ahora vamos a ver la fecha mínima y máxima para tener desde y hasta cuándo van las fechas de nuestros datos.

fechas_min.max.paredes <- df_paredes %>% 
  summarise(FECHA_MAX = max(date),
            FECHA_MIN = min(date))

## Encontramos que las fechas van desde ocurrido el asesinato "2020-10-25" hasta "2022-06-08" 


## El siguiente paso es observar cómo es la distribución de publicaciones según fecha:


# FRECUENCIA DE PUBLICACIÓN -----------------------------------------------



## Primero se crea una tabla para agrupar por mes y año la cantidad y el porcentaje que representa cada fecha en el total de publicaciones:

tabla_freq.publicacion.paredes <- df_paredes %>% 
  group_by(mes_anio) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = round(n/sum(n)*100, 2))

## Ordenamos la tabla de acuerdo a la cantidad:

tabla_freq.publicacion.paredes <- arrange(tabla_freq.publicacion.paredes, desc(n))



tabla_freq.publicacion.paredes %>%
  mutate(mes_anio = fct_reorder(mes_anio, n)) %>%
  ggplot( aes(x=mes_anio, y= perc)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("fecha") +
  ylab("porcentaje")+
  theme_bw()



## Esta dimensión, la cual mide la distribución de publicaciones totales sobre el caso Joaquín Paredes del ElDoce.Tv en el tiempo, nos señala que el mayor porcentaje de publicaciones se encuentran en el mes de sucedido el hecho mientras que el segundo período de mayor ritmo de publicación es exactamente un año después, Octubre del 2021. 

## El tercer valor corresponde a Diciembre de 2020. PENDIENTE LA OBSERVACIÓN DE ESTE DATO

## queda pendiente observar la distribución por semana o día quizás.


# INTERACCIÓN -------------------------------------------------------------



## Agrupación de interacción según fecha:

tabla_interaccion.fecha.paredes <- df_paredes %>% 
  group_by(mes_anio) %>% 
  summarise(MIN_INTERAC = min(interacciones),
            MAX_INTERAC = max(interacciones),
            MEDIA_INTERACC = mean(interacciones))

## La interacción puede ser definida como la suma de todas las reacciones (me gusta, me divierte, me enoja, etc + comentarios + compartidos)



publicacion_interaccion.paredes <- df_paredes %>% 
  group_by(noticia_link) %>% 
  summarise(interacciones = sum(interacciones, na.rm=TRUE)) %>% 
  arrange(desc(interacciones))


# https://www.facebook.com/eldocetv/posts/pfbid034yKfWcTF2usBdFrzWpf4wTLb5LLTsR4ouV63Dqsy3RgqT3Ktbk9kEUTvK2JP3qNsl

## "Crece el dolor. Llorando y con la foto en la mano, su abuelo habló con El Doce y pidió justicia. Del sueño de ser jugador de fútbol a ser asesinado por la espalda." 26/10/2020


# COMENTARIOS -------------------------------------------------------------



publicaciones_comentarios.paredes <- df_paredes %>% 
  group_by(noticia_link) %>% 
  summarise(comentarios_23 = sum(comentarios_23, na.rm=TRUE)) %>% 
  arrange(desc(comentarios_23))


# https://www.facebook.com/eldocetv/posts/pfbid034yKfWcTF2usBdFrzWpf4wTLb5LLTsR4ouV63Dqsy3RgqT3Ktbk9kEUTvK2JP3qNsl

## "Crece el dolor. Llorando y con la foto en la mano, su abuelo habló con El Doce y pidió justicia. Del sueño de ser jugador de fútbol a ser asesinado por la espalda." 26/10/2020




# https://www.facebook.com/eldocetv/posts/pfbid038LvC7bBbkDGTsYBYTWjsgJ8jfwPWZdi64J11iptgohJSmEw6qcy7dA2ay9J6FMsQl

## "Cuando la Policía no actúa se le reclama y cuando actuó también se la señala" La mujer, hermana de uno de los policías detenidos, afirmó que hubo un enfrentamiento antes de la muerte del adolescente." 26/10/2020


# COMPARTIDOS -------------------------------------------------------------

publicaciones_compartidos.paredes <- df_paredes %>% 
  group_by(noticia_link) %>% 
  summarise(compartidos = sum(compartidos, na.rm=TRUE)) %>% 
  arrange(desc(compartidos))

# https://www.facebook.com/watch/?v=1353973594935264

## Dolor por Joaquín. Se vivieron horas de mucha tristeza y bronca en Paso Viejo, luego de que policías asesinaran al chico de 15 años. Toda la info sobre el caso que conmociona a Córdoba. 26/10/2020 




# EMOCIONALIDADES ---------------------------------------------------------

ME_GUSTA.paredes <- df_paredes %>% 
  group_by(noticia_link) %>% 
  summarise(me_gusta = sum(me_gusta, na.rm=TRUE)) %>% 
  arrange(desc(me_gusta))

# https://www.facebook.com/eldocetv/posts/pfbid034yKfWcTF2usBdFrzWpf4wTLb5LLTsR4ouV63Dqsy3RgqT3Ktbk9kEUTvK2JP3qNsl

## "Crece el dolor. Llorando y con la foto en la mano, su abuelo habló con El Doce y pidió justicia. Del sueño de ser jugador de fútbol a ser asesinado por la espalda." 26/10/2020


SIMPATIA.paredes <- df_paredes %>% 
  group_by(noticia_link) %>% 
  summarise(simpatia = sum(simpatia, na.rm=TRUE)) %>% 
  arrange(desc(simpatia))

# https://www.facebook.com/eldocetv/posts/pfbid02LN7pb9NFHgaXwa11DCnKSEjzSmhexAh9aZmpPQaWNbYJnHhuEycK69YFrpKW3Bzcl

## ""Cuando la Policía no actúa se le reclama y cuando actuó también se la señala" La mujer, hermana de uno de los policías detenidos, afirmó que hubo un enfrentamiento antes de la muerte del adolescente." 26/10/2020


ASOMBRO.paredes <- df_paredes %>% 
  group_by(noticia_link) %>% 
  summarise(asombro = sum(asombro, na.rm=TRUE)) %>% 
  arrange(desc(asombro))


# https://www.facebook.com/eldocetv/posts/pfbid0qzRsNChzKuRmNiXSaeCVjNJoE4CF6oPyi1oD2AfBd4LntGLQn3Pzeok8oMqCXFbHl

## "Información exclusiva de El Doce A poco de cumplirse una semana del crimen de Joaquín Paredes, las pruebas toxicológicas complican la situación de los policías detenidos." 31/10/2020


DESCREIMIENTO.paredes <- df_paredes %>% 
  group_by(noticia_link) %>% 
  summarise(descreimiento = sum(descreimiento, na.rm=TRUE)) %>% 
  arrange(desc(descreimiento))

# https://www.facebook.com/eldocetv/posts/pfbid0F3GhkHKMw7NQ6xY4PFgfJv1PKUtJniJ478kupPPjp9fZT7KLHw5nSVA8iXzSADJTl

## "El norte cordobés, tierra de pocas oportunidades. “Es deslomarse y terminar destruido a los 40 años o entrar a la Policía", planteó una docente de la localidad adonde un balazo policial mató a Joaquín Paredes. Su reflexión, en esta nota." 27/10/2020


TRISTEZA.paredes <- df_paredes %>% 
  group_by(noticia_link) %>% 
  summarise(tristeza = sum(tristeza, na.rm=TRUE)) %>% 
  arrange(desc(tristeza))

# https://www.facebook.com/eldocetv/posts/pfbid034yKfWcTF2usBdFrzWpf4wTLb5LLTsR4ouV63Dqsy3RgqT3Ktbk9kEUTvK2JP3qNsl

## "Crece el dolor. Llorando y con la foto en la mano, su abuelo habló con El Doce y pidió justicia. Del sueño de ser jugador de fútbol a ser asesinado por la espalda." 26/10/2020


ENOJO.paredes <- df_paredes %>% 
  group_by(noticia_link) %>% 
  summarise(enojo = sum(enojo, na.rm=TRUE)) %>% 
  arrange(desc(enojo))

# https://www.facebook.com/eldocetv/posts/pfbid038LvC7bBbkDGTsYBYTWjsgJ8jfwPWZdi64J11iptgohJSmEw6qcy7dA2ay9J6FMsQl

## ""Cuando la Policía no actúa se le reclama y cuando actuó también se la señala" La mujer, hermana de uno de los policías detenidos, afirmó que hubo un enfrentamiento antes de la muerte del adolescente." 26/10/2020




# PROCESAMIENTO DE LENGUAJE - NOTICIAS ------------------------------------

tokken_noticas.paredes <- df_paredes %>% 
  unnest_tokens(word, cuerpo_de_la_noticia) %>% 
  filter(!word %in% my_stopwords$word,
         !word %in% str_remove_all(my_stopwords$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^[0-9]*$"),
         !str_detect(word, "eldoce.tv")) %>% 
  count(word, sort = TRUE)







# PROCESAMIENTO DE LENGUAJE - POSTEOS -------------------------------------


tokken_posteos.paredes <- df_paredes %>% 
  unnest_tokens(word, cuerpo_del_post) %>% 
  filter(!word %in% my_stopwords$word,
         !word %in% str_remove_all(my_stopwords$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^[0-9]*$"),
         !str_detect(word, "eldoce.tv")) %>% 
  count(word, sort = TRUE)






# PROCESAMIENTO DE LENGUAJE - COMENTARIOS (GRAL) --------------------------

tokken_coments.paredes <- df_paredes %>% 
  unnest_tokens(word, comentarios_28) %>% 
  filter(!word %in% my_stopwords$word,
         !word %in% str_remove_all(my_stopwords$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^[0-9]*$"),
         !str_detect(word, "eldoce.tv")) %>% 
  count(word, sort = TRUE)






