### PRUEBA NLP 27.10.22

## teniendo en cuenta nuestro cuadro de operativización donde las variables a observar son: 

# 1) EL POST (etiqueta = de qué se trata la noticia/post)

# 2) FECHA DE PUBLICACIÓN

# 3) FREQ DE PUBLICACIÓN

# 4) INTERACCIÓN TOTAL

# 5) N° DE COMENTARIOS 

# 6) N° COMPARTIDOS

# 7) REACCIONES


## Lo que hay que hacer es analizar el contenido en relación a las variables!


install.packages("tm")
install.packages("NLP")
library(NLP)
library(tm)
stopwords("spanish")

install.packages("SnowballC")
library(SnowballC)



## con la tabla1 agrupamos los post por temática, vemos su cantidad total y podemos filtrar por eso:

tabla1 <- df_blas %>% 
  group_by(etiqueta_3) %>% 
  count(etiqueta_3, sort = TRUE)


tokkem_aniversario <- df_blas %>%
  filter(etiqueta_3 == "Aniversario del caso") %>% 
  mutate(id = mes_anio) %>% 
  unnest_tokens(word, comentarios_28) %>%
  anti_join(my_stopwords) %>%
  anti_join(nombres) %>% 
  count(id, word, sort = TRUE)



# TM focus ----------------------------------------------------------------

# transformamos a vector

tokken_corpus <- VCorpus(VectorSource(df_blas$comentarios_28))

# todo a minúsculas:

corpus_clean <- tm_map(tokken_corpus, content_transformer(tolower))

# eliminamos números:
corpus_clean <- tm_map(corpus_clean, removeNumbers)

# eliminamos pal vacías:

corpus_clean <- tm_map(corpus_clean,removeWords, stopwords("spanish"))


corpus_clean <- tm_map(corpus_clean, removePunctuation)

corpus_clean <- tm_map(corpus_clean, stemDocument, "spanish")

corpus_clean <- tm_map(corpus_clean, stripWhitespace)




sms_dtm2 <- DocumentTermMatrix(corpus_clean,
                               control = list(
                                 tolower = TRUE,
                                 removeNumbers = TRUE,
                                 stopwords = TRUE,
                                 removePunctuation = TRUE,
                                 stemming = TRUE
                               )
)
findFreqTerms(sms_dtm2)


inspect(sms_dtm2)


datos<-data.frame(inspect(sms_dtm2))
datos







