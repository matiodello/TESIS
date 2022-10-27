## PRUEBA ANÁLISIS DE CONTEXTO - PAREDES


paredes_oracion <- df_paredes %>% 
  select(mes_anio, comentarios_28) %>% 
  unnest_tokens(oracion, comentarios_28, token = "sentences")



## Luego vamos a seleccionar aquellas oraciones que contengan las palabras sobre las que me interesa observar el contexto

coment_justicia <- paredes_oracion %>%
  filter(str_detect(oracion, paste(c("justicia"),collapse = '|')))

coment_policia <- paredes_oracion %>%
  filter(str_detect(oracion, paste(c("policía", "policías", "policia", "policias"),collapse = '|')))



## Ahora sí, observamos el contexto de estas oraciones. Primero, tokenizamos por palabra. Después, hacemos un count de cada una de las palabras para saber la cantidad de veces que se repiten, luego de eso, limpiamos palabras vacías con un antijoin y por último le aplicamos la función cast_dfm del paquete quanteda. Este transforma nuestro marco de datos en una matriz. 

## Una matriz puede ser descrita como vectores multidimensionales. Al igual que un vector, únicamente puede contener datos de un sólo tipo, pero además de largo, tienen más dimensiones.

## En un sentido estricto, las matrices son una caso especial de un array, que se distingue por tener específicamente dos dimensiones, un “largo”" y un “alto”. Las matrices son, por lo tanto, una estructura con forma rectangular, con renglones y columnas.


library(quanteda)
library(dplyr)
library(readr)
library(quanteda.textplots)





policia_dfm <- coment_policia %>%
  unnest_tokens(word, oracion, drop = FALSE) %>%
  filter(!str_detect(word, paste(c("^http", "t.co"),collapse = '|'))) %>%
  count(`oracion`, word) %>%
  anti_join(my_stopwords) %>%
  cast_dfm(`oracion`, word, n)



## Nos quedamos con un número finito de palabras sobre las que realizar las relaciones

top_words_timelines <- names(topfeatures(policia_dfm, 10)) 

## Hacemos una matriz de palabras (palabras que aparecen juntas en el mismo texto/oracion): fcm hace una matriz de correlacion de palabras

words_timelines_fcm <- fcm(policia_dfm)

## Visualizamos las relaciones

words_fcm <- fcm_select(words_timelines_fcm, pattern = top_words_timelines) #Me quedo con las palabras  que estan en el top 10

textplot_network(words_fcm, min_freq = 0.1, edge_color = "#008037", edge_alpha = 0.5, edge_size = 0.8, omit_isolated = TRUE)

## Las redes de co-ocurrencia son la interconexión colectiva de términos basados en su presencia emparejada dentro de una unidad de texto especificada. Las redes se generan conectando pares de términos usando un conjunto de criterios que definen la co-ocurrencia.


## Cuál es la diferencia entre redes de relación y de co-ocurrencia? Las redes de relación especifican nodos y relación pudiendo indicar cómo es flecha y hacia donde se relacionan los términos (variables). Las redes de co-ocurrencia resultan más útiles para analizar contextos en lenguaje natural.





## esto te genera un documento con palabras/oraciones asociadas a una keyword:

kwic.paredes <- kwic(df_paredes$comentarios_28, pattern = "polic*")


     
