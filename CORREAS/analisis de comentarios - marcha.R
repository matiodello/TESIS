library(tidytext)
library(tm)
library(stopwords)
library(udpipe)
library(syuzhet)
library(stringi)
library(parallel)








# ANÁLISIS COMENTARIOS - MARCHA -------------------------------------------



comentarios.marcha <- read_excel("D:/UNVM/Rstudio - TESIS/TESIS/COMENTARIOS/BLAS/blas_1.xlsx")


comentarios.marcha <- clean_names(comentarios.marcha)

colnames(comentarios.marcha)

comentarios.marcha <- comentarios.marcha [-1,]

comentarios.marcha <- comentarios.marcha %>%
  select(name_click_to_view_profile, profile_id, date, likes, comment) %>% 
  mutate(id=1:n()) 


tabla1 <- comentarios.marcha %>% 
  count(name_click_to_view_profile, profile_id, sort = TRUE)



comentarios.marcha <- comentarios.marcha %>% 
  mutate(comment=str_replace_all(comment, "https\\S*", "")) %>% # hipervínculos
  mutate(comment=str_replace_all(comment, "@\\S*", "")) %>% # menciones
  mutate(comment=str_replace_all(comment, "[\r\n\t]", "")) %>% # separadores
  mutate(comment=removeNumbers(comment)) %>% # números
  mutate(comment=removePunctuation(comment)) %>%  # puntuacion
  mutate(comment=str_squish(comment))


tokken_coments.blas <- comentarios.marcha %>% 
  unnest_tokens(word, comment) %>% 
  filter(!word %in% my_stopwords$word,
         !word %in% str_remove_all(my_stopwords$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^[0-9]*$"),
         !str_detect(word, "eldoce.tv")) %>% 
  count(word, sort = TRUE)







bigramas_marcha <- comentarios.marcha %>%
  unnest_tokens(bigram, comment, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% my_stopwords$word,
         !word2 %in% my_stopwords$word) %>% 
  unite(bigram, word1, word2, sep = " ") 



 

tabl3 <- bigramas_marcha %>% 
  count(bigram, sort = TRUE) %>% 
  filter(!bigram == "NA NA")

tokken_coments.blas %>% 
  summarise(suma = sum(n))
