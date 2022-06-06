rm(list=ls())
library(syuzhet)
library(rtweet)
library(twitteR)
library(dplyr)
library(wordcloud)
library(tidyverse)
library(tidyr)
library(corpus)
library(knitr)
library(readr)
library(textdata)
library(NLP)
library(tm)
library(textclean)
library(quanteda)
#install.packages("tm")
df.revocacion<-read_lines('3discos.txt')
df.revocacion<-data.frame(df.revocacion)

##################PROCESO DE LIMPIEZA PRIMER PASO####
#Quitamos los caracteres no alfanuméricos
df.revocacion<- gsub("[^a-zA-Z0-9 ]", "", df.revocacion)
head(df.revocacion,5)

#Quitamos la puntuación
df.revocacion<- gsub("[[:punct:]]", "", df.revocacion)
head(df.revocacion,5)

#Quitamos NAs
df.revocacion<- df.revocacion[!is.na(df.revocacion)]
head(df.revocacion,5)

#Limpieza de espacios, tabuladores, etc.
df.revocacion<-gsub("[ \t]{2,}", "", df.revocacion)
df.revocacion<-gsub("^\\s+|\\s+$", "", df.revocacion)
head(df.revocacion,5)

#Pasamos a minúsculas todo el texto
df.revocacion<-tolower(df.revocacion)
head(df.revocacion,5)

#Creamos un corpus
#Es necesario hacer un Corpus de tm para realizar otras
#operaciones de minería de texto.

df.revocacion_corpus <- Corpus(VectorSource(df.revocacion))

#Eliminar palabras comunes: Stopwords
#En español también podemos ver las palabras comunes y eliminarlas. Vemos las palabras incluidas en la función
stopwords(language='en')


#La siguiente línea le dice a la función tm_map que
#elimine las palabras (removeWords) de corpus_clean que
#están en stopwords() del listado de palabras españolas.
df.revocacion_corpus<- tm_map(df.revocacion_corpus, removeWords, stopwords(language ="english"))
inspect(df.revocacion_corpus[1:10])

#Tokenizar
texto_palabras<-get_tokens(df.revocacion_corpus)
head(texto_palabras)


#Sentimientos
emocion.df <- get_nrc_sentiment(char_v=texto_palabras, language = "english")
barplot(
  colSums(prop.table(emocion.df[, 9:10])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  col = rainbow(2),
  cex.names = 0.9,
  main = "Analisis de sentimientos de los 3 discos",
  xlab="emociones", ylab = NULL)

barplot(
  colSums(prop.table(emocion.df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  col = rainbow(18),
  cex.names = 0.9,
  main = "Analisis de sentimientos de los 3 discos",
  xlab="emociones", ylab = NULL)

################################################33
rm(list=ls())
#Datos
nov_raw <- read_lines('3discos.txt')	
str(nov_raw)

#Creaci?n de p?rrafos
diez <- rep(1:ceiling(length(nov_raw)/10), each = 10) #Hacer grupos de 10
diez <- diez[1:length(nov_raw)]
nov_text <- cbind(diez, nov_raw) %>% data.frame() #Combinar y convertir a DF
nov_text <- aggregate(formula = nov_raw ~ diez, #Concatenar los renglones
                      data = nov_text,
                      FUN = paste,
                      collapse = " ")
nov_text <- nov_text %>% select(nov_raw) %>% as.matrix #Transformar a matrix

dim(nov_text) #Dimensi?n de nov_text

#Limpieza del texto
nov_text <- gsub("[[:cntrl:]]", " ", nov_text) #Regular expressions
nov_text <- tolower(nov_text) #Convertir a min?sculas
nov_text <- removeWords(nov_text, words = stopwords("english")) #Eliminar Stopwords
nov_text <- removePunctuation(nov_text) #Eliminar puntuaci?n
nov_text <- removeNumbers(nov_text) #Eliminar n?meros
nov_text <- stripWhitespace(nov_text) #Eliminar espacios vac?os

#An?lisis del Corpuse
nov_corpus <- Corpus(VectorSource(nov_text))
nov_corpus

#M?s limpieza
nov_text <- removeWords(nov_text, words = c("yeah", "ooh", "like", "got", "aint", "know", "now", "nowadays", "entonces", "aunque", "don", "do?a", "-"))

nov_corpus <- nov_text %>% VectorSource() %>% Corpus()
nov_ptd <- nov_corpus %>% tm_map(PlainTextDocument)

#Matriz de t?rminos
nov_tdm <- TermDocumentMatrix(nov_corpus)
nov_tdm

#Frecuencia de palabras
nov_mat <- as.matrix(nov_tdm)
dim(nov_mat)

#Suma de renglones ordenados de mayor a menor
nov_mat <- nov_mat %>% rowSums() %>% sort(decreasing = TRUE)
nov_mat <- data.frame(palabra = names(nov_mat), frec = nov_mat)


#Palabras m?s frecuentes
nov_mat[1:20, ]

#Gr?fica de frecuencias
nov_mat[1:10, ] %>%
  ggplot(aes(palabra, frec)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = frec)) + 
  coord_flip() + 
  labs(title = "las 10 palabras mas frecuentes en los 3 discos",  x = "Palabras", y = "Numero de usos")

nov_mat %>%
  mutate(perc = (frec/sum(frec))*100) %>%
  .[1:10, ] %>%
  ggplot(aes(palabra, perc)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = round(perc, 2))) + 
  coord_flip() +
  labs(title = "las 10 palabras mas frecuentes en el disco ETERNAL ATAKE", x = "Palabras", y = "Porcentaje de uso")

#Asosiaci?n entre palabras
findAssocs(nov_tdm, terms = c("money", "home", "baby", "know"), corlimit = .25)

#Eliminar t?rminos dispersos
nov_new <- removeSparseTerms(nov_tdm, sparse = .95)

nov_tdm #T?rminos originales
nov_new #T?rminos nuevos

#Convertir a matrix
nov_new <- nov_new %>% as.matrix()

#Matriz de distancia
nov_new <- nov_new / rowSums(nov_new) #Estandarizaci?n

nov_dist <- dist(nov_new, method = "euclidian") #Matriz de distancia euclideana 

#Agrupamiento
nov_hclust <-  hclust(nov_dist, method = "ward.D")

#Gr?fico
plot(nov_hclust, main = "Dendrograma de ETERNAL ATAKE", sub = "", xlab = "")

#Enfatizar en los grupos
plot(nov_hclust, main = "Dendrograma de ETERNAL ATAKE", sub = "", xlab = "")
rect.hclust(nov_hclust, k = 10, border="blue")


#######################################################################################3
rm(list=ls())

library(tidytext)
library(tidyverse)
library(igraph)
library(ggraph)
library(corpus)
library(textclean)
library(lubridate)
text<-read_lines('3discos.txt');text
text<-as.data.frame(text)
texto_clean <- text %>%
mutate(text_clean = text %>%
replace_non_ascii() %>%
replace_html(symbol = F) %>% # remove html tag
str_replace_all("[0-9]", " ") %>%
str_replace_all("https", " ") %>%
str_replace_all("number", " ") %>%
str_replace_all("[-|]", " ") %>% # replace "-" with space
tolower() %>% #lowercase
replace_symbol() %>%
replace_word_elongation() %>% # lengthen shortened word
str_replace_all("[[:punct:]]", " ") %>% # remove punctuation
str_squish() %>% # remove double whitespace
str_trim() # remove whitespace at the start and end of the text
)

library(stopwords)
stop_words <- stopwords::stopwords(language = "en")
stop_words <- as.data.frame(stop_words)
colnames(stop_words) <- c("word")
word <- c( 'sir', 'mr', 'miss', 'mrs', 'NA')
custom_stopwords <- tibble(word)
stop_words_c <- stop_words %>%
bind_rows(custom_stopwords)
bigrams <- texto_clean %>% select(text_clean) %>%
unnest_tokens(bigram, text_clean, token = "ngrams", n = 2)

bigram_counts<-bigrams %>%
separate(bigram, into = c("uno", "dos"), sep = " ") %>%
filter(!uno %in% stop_words_c$word) %>%
filter(!uno %in% NA) %>%
filter(!dos %in% stop_words_c$word) %>%
filter(!dos %in% NA) %>%
unite(bigram, uno, dos , sep = " ") %>%
count(bigram, sort = TRUE) %>%
na.omit()

#----------------------------Brigramas con m?s frecuencia----------------------
bigram_counts
head(bigram_counts )
#------------------------------nube de bigramas-----------------------------
library(wordcloud)
bigram_counts %>%
with(wordcloud(words=bigram,
freq=n,
max.words = 60,
scale = c(2,1),
rot.per = 0.3,
random.order = FALSE,
colors=brewer.pal(6,"Dark2")))

#----------------------------Generaci?n del bigrama--------------------------
set.seed(1)
bigram_graph <- bigram_counts %>%
filter(n >= 10) %>%
graph_from_data_frame()
ggraph(bigram_graph, layout = "fr") +
geom_edge_link(arrow = arrow(type = "closed", length = unit(.075, "inches"))) +
geom_node_point() +
geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
theme_void()
