## Dendrograma

## Instalar paquete 
install.packages("cluster.datasets")
library(cluster.datasets)

data("all.mammals.milk.1956")
AMM=all.mammals.milk.1956

## Se renombra la matriz 
head(AMM)

# Cálculo de la matriz de distancia de Mahalonobis
dist.AMM<-dist(AMM[,2:6])

# Convertir los resultados del 
# cálculo de la distancia a una matriz de datos y
# me indique 3 digitos.

round(as.matrix(dist.AMM)[1:6, 1:6],3)

# Cálculo del dendrograma
dend.AMM<-as.dendrogram(hclust(dist.AMM))

# Generación del dendrograma
plot(dend.AMM)

# Agregar etiquetas al gráfico

AMM.nombres=AMM
rownames(AMM.nombres)= AMM.nombres$name
AMM.nombres=AMM.nombres[,-1]

# Construimos de nuevo el gráfico
plot(as.dendrogram(hclust(dist(AMM.nombres))))


#  Modificar el dendrograma

## Instalar paquete 
install.packages("dendextend")
library(dendextend)

# Guardar las etiquetas en un objeto "L"
L=labels(dend.AMM)
labels(dend.AMM)=AMM$name[L]

# Cambiar el tamaño de las etiquetas
dend.AMM %>%
  set(what="labels_col", "blue") %>% #Colores etiqueta
  set(what="labels_cex", 0.8) %>%
  plot(main="Dendrograma de mamíferos")


## Instalar paquete
install.packages("circlize")
library("circlize")

# Dendograma de círculo
circlize_dendrogram(dend.AMM,labels_track_height=NA,
                    dend_track_height=0.1)

