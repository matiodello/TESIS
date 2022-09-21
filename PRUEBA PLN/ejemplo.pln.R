## PRUEBA DE MODELADO DE TÓPICOS:


install.packages("udpipe") # las hemos instalado en capítulos anteriores
install.packages(c("topicmodels", "stopwords" )) # las usaremos por primera vez



# PRE - PROCESAMIENTO DE CORPUS -------------------------------------------

library(tidyverse) # para manipular en general
library(tidytext) # para convertir los objetos a formatos requeridos / devueltos por LDA

noticias <- readr::read_csv("https://raw.githubusercontent.com/gastonbecerra/curso-intro-r/main/data/noticias_tm.csv") %>%
  mutate(id=1:n())
glimpse(noticias) # miramos la estructura de la base




## Para poder completar nuestros análisis primeros realizaremos varias tareas de preprocesamiento:

##Haremos un análisis morfosintático para determinar los distintos componentes de la oración; Reduciremos las palabras a sus lemmas, formas básicas de las palabras, sin género ni conjugación; Descartaremos algunas palabras comunes, quedándonos sólo con las más significativas.

## Para estas tareas trabajaremos con la librería UdPipe, desarrollada por el Instituto de linguistica formal y aplicada de la Universidad de la República Checa, que tiene un modelo para procesar texto en castellano, y que usamos en el capítulo anterior.



library(udpipe) # la cargamos
modelo_sp <- udpipe::udpipe_download_model('spanish') # descarga el modelo y guarda la referencia  
modelo_sp$file_model # refrencia al modelo descargado
modelo_sp <- udpipe_load_model(file = modelo_sp$file_model) # cargamos el modelo en memoria


noticias_anotadas <- udpipe_annotate( 
  object = modelo_sp, # el modelo de idioma
  x = noticias$txt, # el texto a anotar, 
  doc_id = noticias$id, # el id de cada oracion (el resultado tendrá 1 palabra x fila)
  trace = 20
) %>% as.data.frame(.) # convertimos el resultado en data frame



