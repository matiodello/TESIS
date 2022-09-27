library(tidytext)
library(tm)
library(stopwords)
library(udpipe)
library(syuzhet)
library(stringi)
library(parallel)








# ANÁLISIS COMENTARIOS - 1 -------------------------------------------



paredes_coments.1 <- read_excel("D:/UNVM/Rstudio - TESIS/TESIS/COMENTARIOS/PAREDES/paredes_1.xlsx")



paredes_coments.1 <- clean_names(paredes_coments.1)

colnames(paredes_coments.1)

comentarios.marcha <- comentarios.marcha [-1,]

paredes_coments.1 <- paredes_coments.1 %>%
  select(name_click_to_view_profile, profile_id, date, likes, comment) %>% 
  mutate(id=1:n()) 


tabla1 <- paredes_coments.1 %>% 
  count(name_click_to_view_profile, profile_id, sort = TRUE)



paredes_coments.1 <- paredes_coments.1 %>% 
  mutate(comment=str_replace_all(comment, "https\\S*", "")) %>% # hipervínculos
  mutate(comment=str_replace_all(comment, "@\\S*", "")) %>% # menciones
  mutate(comment=str_replace_all(comment, "[\r\n\t]", "")) %>% # separadores
  mutate(comment=removeNumbers(comment)) %>% # números
  mutate(comment=removePunctuation(comment)) %>%  # puntuacion
  mutate(comment=str_squish(comment))


tokken_coments.blas <- paredes_coments.1 %>% 
  unnest_tokens(word, comment) %>% 
  filter(!word %in% my_stopwords$word,
         !word %in% str_remove_all(my_stopwords$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^[0-9]*$"),
         !str_detect(word, "eldoce.tv")) %>% 
  count(word, sort = TRUE)







bigramas_paredes_coment <- paredes_coments.1 %>%
  unnest_tokens(bigram, comment, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% my_stopwords$word,
         !word2 %in% my_stopwords$word) %>% 
  unite(bigram, word1, word2, sep = " ") 





tabl3 <- bigramas_paredes_coment %>% 
  count(bigram, sort = TRUE) %>% 
  filter(!bigram == "NA NA")

