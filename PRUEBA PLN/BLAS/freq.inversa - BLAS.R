## Prueba PLN 25/10/22  - FRECUENCIA INVERSA DE PALABRAS


nombres <- read.xlsx("Nombres.xlsx")



## frecuencia inversa de palabras para los 3 agostos. La freq inversa lo que hace es, en vez de contar palabras y sacar su frecuencia absoluta, toma en consideración todas las palabras y las divide por el número total de palabras o sea una freq relativa, esto nos da la posibilidad de bajar la representatividad de palabras muy comunes y aumentar las palabras importantes


# FREQ - CORPUS DE NOTICIAS -----------------------------------------------




libros_words <- df_blas %>%
  mutate(id = mes_anio) %>% 
  unnest_tokens(word, cuerpo_de_la_noticia) %>%
  anti_join(my_stopwords) %>%
  count(id, word, sort = TRUE)

libros_total_words <- libros_words %>% 
  group_by(id) %>% 
  summarise(total = sum(n))


libros_words <- left_join(libros_words, libros_total_words)



libros_tf_idf <- libros_words %>%
  bind_tf_idf(word, id, n)

libros_tf_idf




libros_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

## Vamos a ver una visualización de estas palabras de alta tf-idf

library(forcats)

libros_tf_idf %>%
  group_by(id) %>%
  filter(id == "08/2020" | id == "08/2021" | id == "08/2022") %>% 
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~id, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)





# FREQ - POST ------------------------------------------------------




libros_words <- df_blas %>%
  mutate(id = mes_anio) %>% 
  unnest_tokens(word, cuerpo_del_post) %>%
  anti_join(my_stopwords) %>%
  count(id, word, sort = TRUE)

libros_total_words <- libros_words %>% 
  group_by(id) %>% 
  summarise(total = sum(n))


libros_words <- left_join(libros_words, libros_total_words)



libros_tf_idf <- libros_words %>%
  bind_tf_idf(word, id, n)

libros_tf_idf




libros_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

## Vamos a ver una visualización de estas palabras de alta tf-idf

library(forcats)

libros_tf_idf %>%
  group_by(id) %>%
  filter(id == "08/2020" | id == "08/2021" | id == "08/2022") %>% 
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~id, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)




# FRQ - COMENTARIOS -------------------------------------------------------

typeof(nombres$Nombre)



libros_words <- df_blas %>%
  mutate(id = mes_anio) %>% 
  unnest_tokens(word, cuerpo_del_post) %>%
  anti_join(my_stopwords) %>% 
  count(id, word, sort = TRUE)


libros_words <- anti_join(nombres$Nombre)



libros_total_words <- libros_words %>% 
  group_by(id) %>% 
  summarise(total = sum(n))


libros_words <- left_join(libros_words, libros_total_words)



libros_tf_idf <- libros_words %>%
  bind_tf_idf(word, id, n)

libros_tf_idf




libros_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

## Vamos a ver una visualización de estas palabras de alta tf-idf

library(forcats)

libros_tf_idf %>%
  group_by(id) %>%
  filter(id == "08/2020" | id == "08/2021" | id == "08/2022") %>% 
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~id, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
