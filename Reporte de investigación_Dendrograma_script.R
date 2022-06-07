# Dendrograma 

# Instalar paquetes
install.packages("readr")
library(readr)
install.packages("cluster.datasets")
library(cluster.datasets)
install.packages("dendextend")
library(dendextend)
install.packages("factoextra")
library(factoextra)
install.packages("ggplot2")
library(ggplot2)
install.packages("igraph")
library(igraph)
install.packages("stats")
library(stats)
install.packages("factoextra")
library(factoextra)
install.packages("scales")
library(scales)
install.packages("ggsci")
library(ggsci)
install.packages("cluster")
library(cluster)
install.packages("factoextra")
library(factoextra)

# Matriz de datos 
Netflix_subscription_fee_Dec_2021 <- read_csv("Netflix subscription fee Dec-2021.csv")

# Se renombra la matriz 
NsfC2=Netflix_subscription_fee_Dec_2021

# Exploraci?n de la matriz 
dim(NsfC2) # Dimensi?n
str(NsfC2) # Tipo de variables
colnames(NsfC2) # Nombre de las variables 
anyNA(NsfC2) # Presencia de NA

# Tratamiento de la matriz 
head(NsfC2)

# C?lculo de la matriz de distancia de Mahalonobis
dist.NsfC2<-dist(NsfC2[,3:6])

# Convertir los resultados del c?lculo de la distancia a una matriz de datos y me indique 3 digitos.
round(as.matrix(dist.NsfC2)[3:6, 3:6],3)

# Calculo del dendrograma
dend.NsfC2<-as.dendrogram(hclust(dist.NsfC2))

# Generacion del dendrograma
plot(dend.NsfC2)

# Agregar etiquetas al gr?fico
NsfC2.country=NsfC2
NsfC2.country=NsfC2.country[,-1]


#------------------------------
#  Modificar el dendrograma
#-------------------------------

# Guardar las etiquetas en un objeto "L"
L=labels(dend.NsfC2)
labels(dend.NsfC2)=NsfC2$Country[L]

# cambiar el tama?o de las etiquetas
dend.NsfC2 %>%set(what="labels_col", "blue") %>%#Colores etiquetaset(what="labels_cex", 0.7) %>%
  plot(main="Figura 1- Cuota de suscripción de Netflix en diferentes países ")

# Dendrograma en forma de árbol filogenético
Phylo = fviz_dend(dend.NsfC2, cex = 0.8, lwd = 0.8, k = 4,
                  rect = TRUE,k_colors = "jco",rect_border = "jco",
                  rect_fill = TRUE,type = "phylogenic")Phylo%>%
  plot(main="Figura 2- Cuota de suscripción de Netflix en diferentes países ")

# Clustering CLARA

#Matriz con selección de variables
datosCLARA <- cbind(NsfC2$`No. of TV Shows`, NsfC2$`No. of Movies`, 
                    NsfC2$`Cost Per Month - Basic ($)`, NsfC2$`Cost Per Month - Basic ($)`,
                    NsfC2$`Cost Per Month - Standard ($)`, NsfC2$`Cost Per Month - Premium ($)`)
colnames(datosCLARA) <- c("A", "B", "C", "D","E","F")
head(datosCLARA)
clara_clusters <- clara(x = datosCLARA, k = 2, metric = "manhattan", stand = TRUE,
                        samples = 17, pamLike = TRUE)
clara_clusters

fviz_cluster(object = clara_clusters, ellipse.type = "t", geom = "point",
             pointsize = 2.5) +
  theme_bw() +
  labs(title = "Figura 3- Clustering CLARA") +
  theme(legend.position = "none")

# Citar R 
citation()

#Citar paquetes
citation("cluster.datasets")




