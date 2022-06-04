
#_______________ BIPLOT _____________-

# Instalacion de paquetes
install.packages("MultBiplotR")
library(MultBiplotR)

# Reconocimiento de la matriz de datos
load("Vinos.rda")
BD<-Vinos

#-------------------------------------
# Exploracion de matriz
#-------------------------------------
dim(BD)
str(BD)
colnames(BD)


#---------------------------------
#  Graficos de exploracion
#----------------------------------
BX1<-BoxPlotPanel(BD[,4:9], nrows=2, groups=BD$denomina)
BX1

BX2<-BoxPlotPanel(BD[,4:9], nrows=2, groups=BD$grupo)
BX2

#--------------------------------
#  Filtrado de variables
#--------------------------------

# 1.- Seleccion de variables numericas
X<-BD[,4:21]

# 2.- Generacion Plot
PL1<-plot(X[,1:9])
PL2<-plot(X[,10:18])

#----------------------------
#   Reduccion de la dimensionalidad
#-------------------------------

#1.- ACP
# Scaling= 
# 1: datos orginales, 
# 2: Resta la media global del conjunto de los datos, 
# 3: Doble centrado (agricultura / interaccion de resuduales)
# 4: Centrado por columnas (variables con misma escala)
# 5: Estandarizado por columnas 

acpvino<-PCA.Analysis(X,Scaling = 5)
summary(acpvino)

# Presentacion de tablas (markdown)
summary(acpvino, latex=TRUE)

#2.- Contenido del objeto acpvino
names(acpvino)

#3.- Generacion del grafico
# Sin caja
acp1<-plot(acpvino, ShowBox=FALSE)

# screeplot con barras 
acp2<-princomp(X, cor=TRUE, score=TRUE)
plot(acp2)

# Grafico circular de correlacion
acp3<-plot(acpvino, CorrelationCircle=TRUE, 
           ShowAxis=TRUE,  CexInd=1.5)

# agregar grupos al biplot
# definido por usuario
acpvino1<-AddCluster2Biplot(acpvino, ClusterType="us", 
                          Groups = BD$grupo)

# Grafico con poligonos
# CexInd= tamaño de los argumentos
acp4<-plot(acpvino1, PlotClus=TRUE, 
           ClustCenters=TRUE, margin=0.05, 
           CexInd=0.7, ShowBox=TRUE)

# grafico con elipses
acp5<-plot(acpvino1, PlotClus=TRUE, ClustCenters=TRUE, 
     margin=0.05, CexInd=0.7, TypeClus="el", 
     ShowBox=F)

# grafico con estrellas
acp6<-plot(acpvino1, PlotClus=TRUE, ClustCenters=TRUE, 
     margin=0.05, CexInd=0.7, TypeClus="st", 
     ShowBox=TRUE)


#------------------------------------
#  Biplot
#-------------------------------------

# alpha= 
#  0:GH
#  1:JK
#  2:HJ
# Predeterminado JK
bipvino<-PCA.Biplot(X, Scaling = 5)
summary(bipvino)

# Valores propios
bipvino$EigenValues
# screeplot
SC<-barplot(bipvino$EigenValues)

# Vectores propios
bipvino$EV

# Tabla de inercias
Inercias<-data.frame(paste("Eje",1:length(bipvino$EigenValues)),
                     bipvino$EigenValues, bipvino$Inertia, 
                     bipvino$CumInertia)

colnames(Inercias)<-c("Eje", "Valor Propio", 
                     "Inercia", "Inercia acumulada")

# Markdown
library(knitr)
kable(Inercias)

# tabla contribucion de columnas
kable(bipvino$ColContributions)

# Grafico
plot(bipvino, ShowBox=TRUE)

# Prolongacion de vectores linea recta
BP1<-plot(bipvino, mode="s", 
     margin=0.1, ShowBox=TRUE)

# Prolongacion de vectores con flechas y linea punteada
BP2<-plot(bipvino, mode="ah", margin=0.05, 
     ShowBox=TRUE)

# Grafico circular correlaciones 
GC<-CorrelationCircle(bipvino)

# Grafico contribuciones de los vectores
# Calidad de representacion eje 1, 2 y 1+2
ColContributionPlot(bipvino, AddSigns2Labs = FALSE)

# Proyeccion individuos sobre una variable 
# dp= selecciona la variable
BP3<-plot(bipvino, dp=2, mode="s", 
     ColorVar=c("blue", rep("grey",17)),
     ShowBox=TRUE)

#Proyeccion de ind sobre todas las variables
# PredPoints= individuo
BP4<-plot(bipvino, PredPoints=1, mode="s", 
     ColorVar=1:18, ShowBox=TRUE)

# Agregar cluster Jerarquico con datos originales
# metodo ward.D
bipvino=AddCluster2Biplot(bipvino, NGroups=4, 
                          ClusterType="hi", 
                          method="ward.D", 
                          Original=TRUE)

# Cluster aplicado al biplot
clusBP<-plot(bipvino, PlotClus=TRUE,ShowAxis=TRUE)
clusBP