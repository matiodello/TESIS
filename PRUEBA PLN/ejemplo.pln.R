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


## Al igual que en el capítulo anterior, usaremos la información de upos para filtrar las palabras que podrían ser más signficativas: adjetivos, verbos, y sustantivos. Aquí omitimos los adverbios, ya que no nos interesan las posibles modificaciones del sentido entre palabras cercanas, como negaciones o amplificaciones. Además introduciremos otro filtro: eliminaremos palabras muy comunes en el lenguaje, que dificilmente puedan ayudarnos a identificar un campo semántico. Para eso recurrimos a un diccionario de palabras comunes, del pack stopwords, y eliminaremos esos registros con filter. Además, incluimos un conjunto de verbos ad-hoc para ser eliminados.


library(stopwords)
noticias_anotadas2 <- noticias_anotadas %>% 
  filter(upos=="ADJ"| upos=="VERB"| upos=="NOUN") %>% # filtramos por tipo de palabra
  select( doc_id, lemma ) %>% # seleccionamos solo las columnas que nos interesan, esto no es necesario
  filter(!lemma %in% stopwords::stopwords(language = "es")) %>% # filtrar las que no están en la tabla de stopwords
  filter(!lemma %in% c("ser", "decir", "tener", "haber", "estar", "hacer", "ver", "leer","comentar","ir")) %>% # filtramos verbos muy comunes
  filter(!lemma %in% c("año","dia","vez")) # filtramos palabras típicas del género de los documentos
glimpse(noticias_anotadas2)



# 3.2 Vectorizado del texto -----------------------------------------------

## Comúnmente, los modelos de machine learning son entrenados con datos estructurados en forma de tablas. Cuando trabajamos con texto debemos construir estas tablas a partir de las palabras del documento con el que estemos trabajando. Esto lo hacemos con el vectorizado.



## Aquí hemos reducido cada oración a una “bolsa de palabras,” que ha resignado el contexto de formulación de las expresiones verbales, perdiendo el orden. Nos quedamos entonces sólo con un vocabulario general que, para cada oración, anota la frecuencia de aparición con 1 y 0, es decir, con datos que son interpretables por una computadora y que nos pueden servir para entrenar un modelo de machine learning.

## con la función count() es muy fácil armar un vector, si usamos como inputs el id del documento y las palabras. Luego, podemos convertir nuestra tabla de distribución de palabras en este tipo de objeto utilizando la función cast_dtm de la librería tidytext.


noticias_dtm <- noticias_anotadas2 %>%
  count(doc_id, lemma, sort = TRUE) %>% # contamos palabras x documento
  cast_dtm(doc_id, lemma, n) # convertimos a vector
noticias_dtm


## El objeto tipo DocumentTermMatrix nos informa la cantidad de documentos y la cantidad de palabras distintas, y nos indica un % de palabras que aparecen 0 veces en un documento (Sparsity).



# 3.3 Modelado de tópicos con LDA -----------------------------------------


## 3.3.1 Sobre el modelo LDA
## Para construir los tópicos usaremos el modelo Latent Dirichlet Allocation, a través del pack topicmodels. Este modelo genera tópicos proponiendo una cierta distribución de todas las palabras del corpus, y calcula la distribución de estos tópicos en cada documentos.

## En términos gráficos (Blei (2012)):


# {https://bookdown.org/gaston_becerra/curso-intro-r/images/blei2012.png}


## LDA

## Lo interesante de esta manera de operativizar los temas, es que cada tópico puede ser entendido como un campo semántico, un conjunto de palabras que suelen correlacionar en distintos documentos. Luego, en el momento del análisis de estos resultados, buscaremos inferir un tema a partir de las palabras que más contribuyen a cada tópico. E.g., podríamos inferir de un tópico en el que contribuyen fuertemente los términos “venta,” “producto” y “comprador” al tema “comercio.” Según uno de los autores del modelo, la interpretabilidad de la mayoría de los temas es el resultado de “la estructura estadística del lenguaje y cómo interactúa con los supuestos probabilísticos específicos de LDA” (D. Blei, 2012, p. 79).

## A la vez, las palabras no son exclusivas de un tópico sino que cruzan todos los tópicos con una “contribución” relativa. Esto es justamente lo que nos interesa ya queremos comparar distintas maneras de “contextualizar” al mismo término (“big data”) a través de distintos tópicos, caracterizados por el uso de ciertas otras palabras.



# 3.3.2 Aplicar LDA -------------------------------------------------------


## Vamos a construir el modelo con la función LDA. Una decisión importante, que debe ser introducida como un parámetro para realizar los análisis, es el número de tópicos a generar. Empecemos por un número criterioso, rápido para testear, y fácil de examinar, y volvamos sobre este problema.


library(topicmodels)
k_topics <- 6 # numero de topicos
noticias_tm <- topicmodels::LDA(
  noticias_dtm, # vector de terminos por documentos
  k = k_topics, # cantidad de topicos
  method = "Gibbs", # metodo de sampleo de los documentos
  control = list(seed = 1:5, nstart=5, verbose=1000))


noticias_tm


## Ahora vamos a exportar los resultados en los 2 formatos que nos interesa explorar, utilizando la función tidy, y especificando la qué probabilidades que nos interesan:

# beta: probabilidad topico x palabra;
# gamma: probabilidad topico x documento;

noticias_tm_beta <- tidy(noticias_tm, matrix = "beta")
noticias_tm_gamma <- tidy(noticias_tm, matrix = "gamma")
glimpse(noticias_tm_beta)


glimpse(noticias_tm_gamma)



# 3.4 Interpretar el modelo -----------------------------------------------


