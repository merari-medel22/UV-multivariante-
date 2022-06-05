
# ANÁLISIS DISCRIMINANTE LINEAL (LDA)

# Cargar paquetería 
library(MASS)

# Se cargan los datos iris
Z<-as.data.frame(iris)
colnames(Z)
Z$Species
z1<-Z[71,]
summary(Z$Species)
z1

# Se define la matriz de datos y la variable respuesta con las clasificaciones.
x<-Z[,1:4]
y<-Z[,5]

# Definir como n y p el número de flores y variables
n<-nrow(x)
p<-ncol(x)

# Se aplica el Análisis discriminante lineal (LDA)
# Cross validation (cv): clasificaci?n optima
lda.iris<-lda(y~.,data=x, CV=TRUE)

# lda.iris$class contiene las clasificaciones hechas por CV usando LDA.
lda.iris$class

# Creación de la tabla de clasificaciones buenas y malas
table.lda<-table(y,lda.iris$class)
table.lda

# Proporción de errores
mis.lda<- n-sum(y==lda.iris$class)
mis.lda/n

# scater plot
# Buenas clasificaciones en negro y malas en rojo
col.lda.iris<-c("indianred1","black")[1*(y==lda.iris$class)+1]
pairs(x,main="Buena clasificación (negro), mala clasificación (rojo)",
      pch=19,col=col.lda.iris)

# Probabilidad de pertenencia a uno de los tres grupos
lda.iris$posterior

# Gráfico de probabilidades

plot(1:n, lda.iris$posterior[,1],
     main="Probabilidades a posteriori",
     pch=20, col="blue",
     xlab="Número de observaciones", ylab="Probabilidades")
points(1:n,lda.iris$posterior[,2],
       pch=20, col="green")
points(1:n,lda.iris$posterior[,3],
       pch=20, col="orange")

