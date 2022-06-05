
# K-MEANS

# Cargar la matriz de datos.
# Se cnsieran las medianas y busca k objetos representativos
X<-as.data.frame(state.x77)

#X
#-------------------------------------
#     Transformacion de datos
#-------------------------------------

#1.- Transformación de las variables x1,x3 y x8 con la función de logaritmo.

X[,1]<-log(X[,1])
colnames(X)[1]<-"Log-Population"

X[,3]<-log(X[,3])
colnames(X)[3]<-"Log-Illiteracy"

X[,8]<-log(X[,8])
colnames(X)[8]<-"Log-Area"

#---------------------------------
#    Metodo k-means
#---------------------------------

#1.- Separación de filas y columnas.
dim(X)
n<-dim(X)[1]
p<-dim(X[2])

# 2.- Estandarización univariante.
X.s<-scale(X)

# 3.- Algoritmo k-medias (6 grupos)
# nstar es cantidad de subconjuntos aleatorios que se escogen
# para realizar los cálculos de algoritmo.
# el 3 es el número de clouster o de agrpupaciones, en este caso se utilizan 3
Kmeans.6<-kmeans(X.s, 6, nstart=25)

# centroides
Kmeans.6$centers

# cluster de pertenencia
Kmeans.6$cluster


# 4.- SCDG
#hasta aquí llego el minimo de scdg la idea es llegar a 0 
SCDG<-sum(Kmeans.6$withinss)
SCDG

# 5.- Clusters
cl.kmeans<-Kmeans.6$cluster
cl.kmeans

# 6.- Scatter plot con la division de grupos
# obtenidos (se utiliza la matriz de datos centrados)
col.cluster<-c("blue", "red", "green","brown","darkblue","cyan")[cl.kmeans]
pairs(X.s, col=col.cluster, main="k-means", pch=19)

#-----------------------------------------
#  Visualización con las dos componentes principales
#-----------------------------------------

# Instalar apaquete
install.packages("cluster")
library(cluster) 

# Gráfico
clusplot(X.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(X.s)$score[,1:2],
     labels=rownames(X.s), pos=1, col="blue")

#-----------------------------------------
#  Visualización con las dos componentes principales
#-----------------------------------------

# Gráfico
clusplot(X.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(X.s)$score[,1:2],
     labels=rownames(X.s), pos=1, col="blue")

# De aqui se puede tomar la descicio para aumentar el numero de clousters

#-------------------------------------
#  Silhouette
#--------------------------------------
# Representacion gráfica de la eficacia de
# clasificación de una observación dentro de un
# grupo.

# 1.- Generación de los cálculos
dist.Euc<-dist(X.s, method = "euclidean")

# El cl.kmeans es dode se se encuentran los closters 
Sil.kmeans<-silhouette(cl.kmeans, dist.Euc)

#2.- Generación del gráfico

# Los ultimos números de la derecha son la probabilidad si es bajo es decir que la clasificacion es baja 
plot(Sil.kmeans, main="Silhouette for k-means", 
     col="blue")


