## Dendrograma

## Instalar paquete 
install.packages("cluster.datasets")
library(cluster.datasets)

#Matriz de datos
data("mammal.dentition")

## Se renombra la matriz 
MD=mammal.dentition
MD
head(MD)

# C�lculo de la matriz de distancia de Mahalonobis
dist.MD<-dist(MD[,2:5])

# Convertir los resultados del 
# c�lculo de la distancia a una matriz de datos y
# me indique 3 digitos.
round(as.matrix(dist.MD)[2:5, 2:5],3)

# C�lculo del dendrograma
dend.MD<-as.dendrogram(hclust(dist.MD))

# Generaci�n del dendrograma
plot(dend.MD)

# Agregar etiquetas al gr�fico
MD.nombres=MD
rownames(MD.nombres)= MD.nombres$name
MD.nombres=MD.nombres[,-1]

# Construimos de nuevo el gr�fico
plot(as.dendrogram(hclust(dist(MD.nombres))))

#  Modificar el dendrograma

## Instalar paquete 
install.packages("dendextend")
library(dendextend)

# Guardar las etiquetas en un objeto "L"
L=labels(dend.MD)
labels(dend.MD)=MD$name[L]

# Cambiar el tama�o de las etiquetas
dend.MD %>%
  set(what="labels_col", "blue") %>% #Colores etiqueta
  set(what="labels_cex", 0.8) %>%
  plot(main="Para cada animal el n�mero de dientes en cada grupo principal")






