#_______ K-MEANS_____
rm(list=ls())
library(pacman)

p_load(tidyverse,cluster,factoextra,NbClust,dplyr,tools,MVN,
cluster,carData,car,rJava,xlsx,plotly,fpc,readr,MultBiplotR,ggplot2)

## Reconocimiento de la matriz de datos
Pokemon <- read_csv("Pokemon.csv")

#-------------------------------------
## Exploracion de matriz
#-------------------------------------
dim(Pokemon)
str(Pokemon)
colnames(Pokemon)

#####
##cambiar los tipos de las variables
####
nombre<-factor(Pokemon$Name)
Tipo<-factor(Pokemon$Type_1,
             levels= c("Grass","Fire","Water","Bug","Normal","Poison","Electric",
                       "Ground","Fairy","Fighting","Flying","Psychic","Rock",
                       "Steel","Ice","Dragon","Dark","Ghost"))
Generacion<-factor(Pokemon$Generation)
legendario<-factor(Pokemon$Legendary)

HP<-as.numeric(Pokemon$HP,strict = TRUE)
Attack<-as.numeric(Pokemon$Attack,strict = TRUE)
Defense<-as.numeric(Pokemon$Defense,strict = TRUE)
Sp_Atk<-as.numeric(Pokemon$Sp_Atk,strict = TRUE)
Sp_Def<-as.numeric(Pokemon$Sp_Def,strict = TRUE)
Speed<-as.numeric(Pokemon$Speed,strict = TRUE)

X<-data.frame(nombre,HP,Attack,Defense,Sp_Atk,Sp_Def,Speed)
BASE=X

## volver factor variable cualitativa
BASE$nombre=as.factor(BASE$nombre)

## Volvemos "Estado" al marco de los datos
DATOS = data.frame(BASE,row.names=BASE$nombre)

## Eliminacion de la variables 
DATOS[, 1] <- NULL

#1.- Separacion de filas y columnas.
dim(DATOS)
n<-dim(DATOS)[1]
p<-dim(DATOS)[2]

# 2.- Estandarizacion univariante.
X.s<-scale(DATOS)

## Escalar la base de datos
datos.scale = scale (DATOS)


##  Matrix de distancia
Mdistancia = get_dist(datos.scale,method = "manhattan")

##  Estimar el numero de cluster
numCluster = NbClust(data=DATOS, method = "median", distance = "euclidean", diss=NULL,  min.nc=2, max.nc=6,index="alllong")
En este resultados me dicen que mi mejor numero de clouster seria 2

## Clusters K-medoids
set.seed(10)
method_Cluster = eclust(Mdistancia,FUNcluster = "kmeans", k = 2, nstart = 25, graph = F)


#	B) Graficar clusters Kmeans	

Kmedoids =   fviz_cluster(method_Cluster, data = Mdistancia,  main = "Agrupamiento de Pokemons",
     repel=F,star.plot=F,ellipse = T, ellipse.type="euclid" ,ellipse.level = 0.95,ellipse.alpha=.45,
     palette="npg",ggtheme = theme_minimal(),show.clust.cent=T,pointsize = .8,labelsize = 11,font.tickslab=c(12, "bold", "darkcyan"),
     font.x = c(12, "italic", "deepskyblue4"),font.y = c(12, "plain", "deepskyblue4"))+ 
theme(panel.background = element_rect(fill = "azure")) +
theme(plot.background = element_rect(fill = "white"))+
theme(panel.border = element_rect(colour = "darkcyan", fill=NA, size=1.5))+
     theme(plot.title = element_text(size= 14,  vjust=.75, color="deepskyblue4", lineheight=1,face="bold",hjust = 0.5  ))+
     theme(plot.caption = element_text(size = 12,color = "deepskyblue4",hjust=1))+
     theme(plot.subtitle= element_text(size = 13,color = "salmon",hjust=1,face="plain"))+
labs(subtitle = "Cluster K-medoids", caption = "Elaboración: Sergio Ortiz")
Kmedoids

# 3.- Algoritmo k-medias (2 grupos) cantidad de subconjuntos aleatorios que se escogen para realizar los calculos de algoritmo.
Kmeans.3<-kmeans(DATOS, 2, nstart=25)

# centroides
Kmeans.3$centers

# cluster de pertenencia
Kmeans.3$cluster


# 4.- SCDG
SCDG<-sum(Kmeans.3$withinss)


# 5.- Clusters
cl.kmeans<-Kmeans.3$cluster


# 6.- Scatter plot con la division de grupos obtenidos (se utiliza la matriz de datos centrados).
col.cluster<-c("blue", "red")[cl.kmeans]
pairs(X.s, col=col.cluster, main="k-means", pch=19)





# 1.- Generacion de los calculos
dist.Euc<-dist(X.s, method = "euclidean")
Sil.kmeans<-silhouette(cl.kmeans, dist.Euc)

#2.- Generacion del grafico
plot(Sil.kmeans, main="Silhouette for k-means", 
col="blue")







