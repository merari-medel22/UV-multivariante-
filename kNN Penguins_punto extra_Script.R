#K-vecinos próximos

# Caragar libreria 
library(MASS)

# Cargar los datos PENGUINS
library(readxl)
penguins_1_ <- read_excel("penguins (1).xlsx")
Z<-as.data.frame(penguins_1_)
colnames(Z)

# Definir la matriz de datos y la variable respuesta
# Con las clasificaciones
x<-Z[,4:5]
y<-Z[,8]

# Se definen las variables y observaciones
n<-nrow(x)
p<-ncol(x)

# Gráfico scatter plot
# Creación de un vector de colores
head(y)
col.iris<-c("blue","green","orange")[y]
pairs(x, main="Data set Penguins, argo_pico_mm (azul), grosor_pico_mm(verde)", 
      pch=19, col = 4:5)


# kNN

# Cargar libreria 
library(class)

# Se fija una "semilla" para tener valores iguales
set.seed(1001)

# Creación de los ciclos para k=1 hasta k=20. Selecciona el valor de k que tenga el error mas bajo.

# Inicialización de una lista vacia de tamaño 20
knn.class<-vector(mode="list",length=20)
knn.tables<-vector(mode="list", length=20)

# Clasificaciones erroneas
knn.mis<-matrix(NA, nrow=20, ncol=1)
knn.mis

for(k in 1:20){
  knn.class[[k]]<-knn.cv(x,y,k=k)
  knn.tables[[k]]<-table(y,knn.class[[k]])
  # la suma de las clasificaciones menos las correctas
  knn.mis[k]<- n-sum(y==knn.class[[k]])
}
knn.mis

# Número optimo de k-vecinos
which(knn.mis==min(knn.mis))
knn.tables[[12]]

# El más eficiente es k=14
# Se señala el k mas eficiente
k.opt<-12
knn.cv.opt<-knn.class[[k.opt]]
head(knn.cv.opt)

# Tabla de contingencia con las clasificaciones buenas y malas
knn.tables[[k.opt]]

# Cantidad de observaciones mal clasificadas
knn.mis[k.opt]

# Error de clasificación (MR)
knn.mis[k.opt]/n

# Gráfico de clasificaciones correctas y erroneas
col.knn.iris<-c("red","green")[1*(y==knn.cv.opt)+1]
pairs(x, main="Clasificación kNN de Penguins",
      pch=19, col=col.knn.iris)

