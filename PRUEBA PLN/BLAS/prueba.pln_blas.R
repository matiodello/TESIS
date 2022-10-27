## prueba PNL en BLAS




library(udpipe) # la cargamos
modelo_sp <- udpipe::udpipe_download_model('spanish') # descarga el modelo y guarda la referencia  
modelo_sp$file_model # refrencia al modelo descargado
modelo_sp <- udpipe_load_model(file = modelo_sp$file_model) # cargamos el modelo en memoria




# nos quedamos con las variables pertinentes y creamos una columna para identificar ID
df_blas <- df_blas %>% 
  select(-c(vertical_1, vertical_2, region, personalidad_destacada, x10, diario, si))%>%
  mutate(id=1:n())



blas_anotadas <- udpipe_annotate( 
  object = modelo_sp, # el modelo de idioma
  x = df_blas$comentarios_28, # el texto a anotar, 
  doc_id = df_blas$id, # el id de cada oracion (el resultado tendrá 1 palabra x fila)
  trace = 20
) %>% as.data.frame(.) # convertimos el resultado en data frame



library(stopwords)
blas_anotadas2 <- blas_anotadas %>% 
  filter(upos=="ADJ"| upos=="VERB"| upos=="NOUN") %>% # filtramos por tipo de palabra
  filter(!lemma %in% stopwords::stopwords(language = "es")) %>% # filtrar las que no están en la tabla de stopwords
  filter(!lemma %in% c("ser", "decir", "tener", "haber", "estar", "hacer", "ver", "leer","comentar","ir")) %>% # filtramos verbos muy comunes
  filter(!lemma %in% c("año","dia","vez")) # filtramos palabras típicas del género de los documentos
glimpse(blas_anotadas2)






noticias_dtm <- blas_anotadas2 %>%
  count(doc_id, lemma, sort = TRUE) %>% # contamos palabras x documento
  cast_dtm(doc_id, lemma, n) # convertimos a vector
noticias_dtm
