
#--------------------librerias---------------------------
library(rtweet)
library(tidytext)
library(dplyr)
library(tidyverse)
library(wordcloud)
library(reshape2)
library(RColorBrewer)
library(tm)
#install.packages(textdata)

#cargar los datos 
text <- read.table(file = '3discos.txt', sep = '\t', encoding = "UTF-8")
text1 <- data.frame(text)


head(text)
str(text1)
names(text1)

tweets_token <- unnest_tokens(tbl=text1,
                              output = "word",
                              input = "V1",
                              token = "words") #token = Definicinó de la unidad de texto, algunas de estas pueden ser
							# words (palabras individuales), sentences (oraciones), paragraphs (párrafos) 
							#o ngrams (secuencia de palabras consecutivas).


dim(text) #dimension del texto

#-----------------------Eliminacion de stopwords----------------------------------------
tweets_token <- anti_join(x=tweets_token,
                          y=stop_words,
                          by="word")
#------------------------contar palabras con mayor frecuencia---------------------------
count(tweets_token,
      word,
      sort = TRUE) #Ordena de mayor a menor

#---------------------Se observan palabras frecuentes que no sirven para el análisis, se pueden remover con la función filter().--------

tweets_token <- filter(tweets_token, word!="amp,sin,el,la,de" & word!="https" & word!="t.co" & word!="its" & word!="dont")

lista_stopwords <- c("yeah", "ooh", "like", "im","got", "aint", "know", "now", "nowadays", "entonces", "aunque", "don", "do?a", "-")
#------------Se añade el término amp al listado de stopwords
lista_stopwords <- c(lista_stopwords, "amp")
#------------Se filtran las stopwords
tweets_token <- tweets_token %>% filter(!(tweets_token %in% lista_stopwords))

dim(tweets_token) #dimencsion de tokens
#----------------Palabras mas frecuentes dentro del texto--------------------
tweets_token %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_text(aes(label=n), hjust= -0.2) +
  geom_col() +
  xlab(NULL) +
  coord_flip()+
  theme_minimal()
#-----------------El paquete tidytext contiene varios léxicos, algunos de estos son bing y nrc------------------------------------

###bing: Este léxico clasifica las palabras según el sentimiento, puede ser negativo o positivo.
###nrc: Este léxico clasifica las palabras segun el sentimiento (positivo y negativo) 
###y la emoción (anticipation, disgust, fear, joy, sadness, surprise y trust).
###Se pueden descargar estos vocabularios con la función get_sentiments() 
###Nota:(en algunos casos les van a pedir que acepten la licencia de uso).
#install.packages("textdata")
library(textdata)
get_sentiments("bing")
get_sentiments("nrc")

#Al tener el conjunto de datos de forma ordenada (una palabra por fila)
#se puede asignar el sentimiento a cada una de las palabras mediante la función inner_join().
tw_bing <- tweets_token %>%
  inner_join(get_sentiments("bing"))

tw_nrc <- tweets_token %>%
  inner_join(get_sentiments("nrc"))

tw_bing %>%
  count(word,sentiment,sort=TRUE)

tw_nrc %>%
  count(word,sentiment,sort=TRUE)

#-----------------Visualizacion de sentimientos----------------

tw_bing %>%                                   #Empezamos con el léxico bing
  count(word,sentiment,sort=TRUE) %>%         #Contamos palabras según el sentimiento      
  group_by(sentiment) %>%                     #Agrupamos por variable "sentiment"     
  top_n(15) %>%                               #Seleccionamos el top 15            
  ungroup() %>%                               #Siempre conviene desagrupar luego de un agrupado     
  mutate(word=reorder(word,n)) %>%            #Reordenamos la variable "word" según variable "n"        
  ggplot(aes(word,n,fill=sentiment))+         #Fill asigna un color a cada factor de "sentiment"        
  geom_col(show.legend = FALSE)+              #Ocultamos la leyenda
  geom_text(aes(label=n), hjust= 1.2) +       #Agregamos una etiqueta a los valores del eje
  facet_wrap(~sentiment,scales = "free_y") +  #Facetamos según "sentiment"  
  coord_flip() +                              #Invertimos los ejes
  xlab(NULL)    


#-----------------Clasificacion sentimientos y emociones------------------
unique(tw_nrc$sentiment)

tw_nrc %>%
  filter(sentiment!="negative" & sentiment!="positive") %>%
  count(word,sentiment,sort=TRUE) %>%             
  group_by(sentiment) %>%                        
  top_n(15) %>%                                                     
  ungroup() %>%                                   
  mutate(word=reorder(word,n)) %>%                
  ggplot(aes(word,n,fill=sentiment))+           
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=n), hjust= 0) +
  facet_wrap(~sentiment,scales = "free_y")+  
  coord_flip() +
  xlab(NULL)


tw_nrc %>%
  filter(sentiment=="negative" | sentiment=="positive") %>%
  count(word,sentiment,sort=TRUE) %>%             
  group_by(sentiment) %>%                        
  top_n(15) %>% #Seleccionamos el top 15                                                    
  ungroup() %>%                                   
  mutate(word=reorder(word,n)) %>%                
  ggplot(aes(word,n,fill=sentiment))+           
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=n), hjust= 0) +
  facet_wrap(~sentiment,scales = "free_y")+  
  coord_flip() +
  xlab(NULL)
#----------------------Nube de palabras-----------------------
tweets_token %>%
  count(word) %>%
  with(wordcloud(words=word,
                 freq=n,
                 max.words = 250,
                 scale = c(3,1),
                 rot.per = 0.3,
                 random.order = FALSE,
                 colors=brewer.pal(6,"Dark2")))

#---------------------Nube de palabras con sentimiento positivo----------------------
tw_bing%>%
  count(word,sentiment) %>%
  filter(sentiment=="positive") %>%  
  with(wordcloud(words=word,
                 freq=n,
                 max.words = 250,
                 scale = c(3,1),
                 rot.per = 0.3,
                 random.order = FALSE,
                 colors=brewer.pal(6,"Dark2")))


#---------------Ahora según el sentimiento negativo------------------------

tw_bing%>%
  count(word,sentiment) %>%
  filter(sentiment=="negative") %>%  
  with(wordcloud(words=word,
                 freq=n,
                 max.words = 250,
                 scale = c(3,1),
                 rot.per = 0.3,
                 random.order = FALSE,
                 colors=brewer.pal(6,"Dark2")))

#---------------comparacion de sentimientos positivos y negativos--------------------

tw_bing %>%
  count(word,sentiment,sort=TRUE) %>%
  acast(word~sentiment,value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red","green"), 
                   max.words = 300,
                   title.size = 2)

#-------Se puede realizar la misma nube, pero con las emociones como dimensiones del léxico nrc.------------

tw_nrc %>%
  count(word,sentiment,sort=TRUE) %>%
  filter(sentiment!="positive" & sentiment!="negative") %>% 
  acast(word~sentiment,value.var = "n", fill = 0) %>%
  comparison.cloud(title.size =1)




sentimientos_valencia <- (emocion.df$negative *-1) + emocion.df$positive
simple_plot(sentimientos_valencia)

