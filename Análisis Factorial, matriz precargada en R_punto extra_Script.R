# Análisis factorial matriz precargada en R_Punto extra 

#Instalar paquete 
install.packages("datos")
library(datos)

# Selección de datos 
datos::mtautos
autos <- mtautos
View(autos)
str(autos)

###Preparación de la matriz
library(dplyr)

autos1 <- select(autos, -forma, -transmision) # Forma simple 1
View(autos1)


# Análisis factorial

#1.- Lectura de la matriz de datos
x<-as.data.frame(autos1)
View(x)
head(x)

## Separa n (automoviles) y p (variables)
n<-dim(x)[1]
p<-dim(x)[2]

## Scater plot para la visualización de variables originales
pairs(x, col="red", pch=19, main="matriz original")


#---------------------------------
#   Transformación de alguna varibles
#----------------------------------

# Nota: Como las variables tiene diferentes unidades
# de medida, se va a implementar la matriz de
# correlaciones para estimar la matriz de carga

#-------------------------------------
#   Reduccion de la dimensionalidad 
##  Análsis Factorial de componentes principales (PCFA)
#-----------------------------------

# Calcular la matriz de medias y de correlaciones
# Matriz de medias
mu<-colMeans(x)
mu

#Matriz de correlaciones
R<-cor(x)
R
head(R)

# Reducción de la dimensionalidad mediante Análisis factorial de componentes principales (PCFA).

## Calcular los valores y vectores propios.

eR<-eigen(R)

## Valores propios
eR$values

## Vectores propios
eR$vectors


##- Valores propios
eigen.val<-eR$values
eigen.val

## Vectores propios
eigen.vec<-eR$vectors
eigen.vec

## Calcular la proporcion de variabilidad

prop.var<-eigen.val/sum(eigen.val)
prop.var

## Calcular la proporción de variabilidad acumulada
prop.var.acum<-cumsum(eigen.val)/sum(eigen.val)
prop.var.acum


#-------------------------------
# Estimacion de la matriz de carga
#---------------------------------

# Nota: se estima la matriz de carga usando los 
# autovalores y autovectores.
# se aplica la rotación varimax

# Primera estimación de Lamda mayuscula
onal formada por la raiz cuadrada de los primeros 3 autovalores.

L.est.1<-eigen.vec[,1:3] %*% diag(sqrt(eigen.val[1:3]))
L.est.1

# Rotación varimax
L.est.1.var<-varimax(L.est.1)
L.est.1.var


#----------------------------
# Estimación de la matriz de los errores
#-----------------------------
#1.- Estimación de la matriz de perturbaciones
Psi.est.1<-diag(diag(R-as.matrix(L.est.1.var$loadings)%*% t(as.matrix(L.est.1.var$loadings))))
Psi.est.1

# 2.- Se utiliza el método Análisis de factor principal (PFA)
# para estimación de autovalores y autovectores
RP<-R-Psi.est.1
RP

# Calculo de la matriz de autovalores y autovectores
eRP<-eigen(RP)

# Autovalores
eigen.val.RP<-eRP$values
eigen.val.RP

# Autovectores
eigen.vec.RP<-eRP$vectors
eigen.val.RP

# Proporcion de variabilidad
prop.var.RP<-eigen.val.RP/ sum(eigen.val.RP)
prop.var.RP

# Proporcion de variabilidad acumulada
prop.var.RP.acum<-cumsum(eigen.val.RP)/ sum(eigen.val.RP)
prop.var.RP.acum


# Estimación de la matriz de cargas con rotación varimax
L.est.2<-eigen.vec.RP[,1:3] %*% diag(sqrt(eigen.val.RP[1:3]))
L.est.2

# Rotacion varimax
L.est.2.var<-varimax(L.est.2)
L.est.2.var

# Estimación de la matriz de covarianzas de los errores.
Psi.est.2<-diag(diag(R-as.matrix(L.est.2.var$loadings)%*% t(as.matrix(L.est.2.var$loadings))))
Psi.est.2

#----------------------------
#   Obtencion de los scores de ambos métodos
#------------------------------

# PCFA
FS.est.1<-scale(x)%*% as.matrix(L.est.1.var$loadings)
FS.est.1

# PFA
FS.est.2<-scale(x)%*% as.matrix (L.est.2.var$loadings)
FS.est.2

# graficamos ambos scores
par(mfrow=c(2,1))

# Factor I y II
pl1<-plot(FS.est.1[,1], FS.est.1[,2], xlab="primer factor",
          ylab="segundo factor", main="scores con factor I y II con PCFA",
          pch=19, col="orange")

# Factor I y III
pl2<-plot(FS.est.1[,1], FS.est.1[,3], xlab="Primer factor",
          ylab="Tercer factor", main="scores con factor I y III con PCFA",
          pch=19, col="orange")

# Factor II y III
pl3<-plot(FS.est.1[,2], FS.est.1[,3], xlab="Segundo factor",
          ylab="Tercer factor", main="scores con factor II y III con PCFA",
          pch=19, col="blue")

# graficamos ambos scores con nombres de nuestras variables
par(mfrow=c(2,1))

# Factor I y II
pl1<-plot(FS.est.1[,1], FS.est.1[,2], xlab="primer factor",
          ylab="segundo factor", main="scores con factor I y II con PCFA",
          pch=19, col="blue")
text(FS.est.1[,1], FS.est.1[,2], labels = rownames(x), pos=4, col="blue")

# Factor I y III
pl2<-plot(FS.est.1[,1], FS.est.1[,3], xlab="Primer factor",
          ylab="Tercer factor", main="scores con factor I y III con PCFA",
          pch=19, col="blue")
text(FS.est.1[,1], FS.est.1[,3], labels = rownames(x), pos=4, col="blue")

