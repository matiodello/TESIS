
# ANÁLISIS COMENTARIOS - MARCHA -------------------------------------------



comentarios.marcha <- read_excel("D:/UNVM/Rstudio - TESIS/TESIS/COMENTARIOS/BLAS/blas_1.xlsx")

comentarios.marcha <- comentarios.marcha [-1,]


comentarios.marcha <- comentarios.marcha %>%
  mutate(id=1:n()) 
  


library(udpipe) # la cargamos
modelo_sp <- udpipe::udpipe_download_model('spanish') # descarga el modelo y guarda la referencia  
modelo_sp$file_model # refrencia al modelo descargado
modelo_sp <- udpipe_load_model(file = modelo_sp$file_model) # cargamos el modelo en memoria


comentarios.marcha <- udpipe_annotate( 
  object = modelo_sp, # el modelo de idioma
  x = comentarios.marcha$Comment, # el texto a anotar, 
  doc_id = comentarios.marcha$id, # el id de cada oracion (el resultado tendrá 1 palabra x fila)
  trace = 20
) %>% as.data.frame(.)






library(stopwords)
comentarios.marcha <- comentarios.marcha %>% 
  filter(upos=="ADJ"| upos=="VERB"| upos=="NOUN") %>%
  filter(!lemma %in% stopwords::stopwords(language = "es")) %>% # filtrar las que no están en la tabla de stopwords
  filter(!lemma %in% c("ser", "decir", "tener", "haber", "estar", "hacer", "ver", "leer","comentar","ir")) %>% # filtramos verbos muy comunes
  filter(!lemma %in% c("año","dia","vez")) # filtramos palabras típicas del género de los documentos
glimpse(comentarios.marcha)



tabla1 <- comentarios.marcha %>% 
count(lemma, sort = TRUE)



comentarios_dtm <- comentarios.marcha %>%
  count(doc_id, lemma, sort = TRUE) %>% # contamos palabras x documento
  cast_dtm(doc_id, lemma, n) # convertimos a vector
comentarios_dtm




library(topicmodels)
k_topics <- 2 # numero de topicos
comentarios_tm <- topicmodels::LDA(
  comentarios_dtm, # vector de terminos por documentos
  k = k_topics, # cantidad de topicos
  method = "Gibbs", # metodo de sampleo de los documentos
  control = list(seed = 1:5, nstart=5, verbose=1000))


comentarios_tm


comentarios_tm_beta <- tidy(comentarios_tm, matrix = "beta")
comentarios_tm_gamma <- tidy(comentarios_tm, matrix = "gamma")
glimpse(comentarios_tm_beta)






comentarios_tm_beta %>% # principales términos en cada tópico
  group_by(topic) %>%
  top_n(15) %>%
  ungroup() %>%
  arrange(topic, -beta) %>% # vamos a mostrarlo como grafico
  ggplot(aes(x=reorder(term, (beta)),y=beta)) + 
  geom_col() +
  facet_wrap(~topic, scales = "free_y") +
  coord_flip()
