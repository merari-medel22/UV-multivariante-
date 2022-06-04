
# Análisis Canonico


# Instalar paqueterias
library(tidyverse)

#   Preparacion de la matriz

# Se utiliza la matriz penguins

# Exploracion de la matriz

dim(penguins)
colnames(penguins)
str(penguins)
anyNA(penguins)

# Escalamiento de la matriz

# Generación de variables X
X <- penguins %>% 
  select(grosor_pico_mm, largo_pico_mm) %>%
  scale()
head(X)

# Generación de variables Y
Y <- penguins %>%
  select(largo_aleta_mm,masa_corporal_g) %>%
  scale()
head(Y)

#----------------------------------
# Análisis canónico con un par de variables
#----------------------------------

# Instalar paquete
install.packages("CCA")
library(CCA)

# Análisis
ac<-cancor(X,Y)

# Visualización de la matriz X
ac$xcoef

# Visualización de la matriz Y
ac$ycoef

# Visualización de la correlación canónica
ac$cor

# Obtención de la matriz de variables canonicas
# Se obtiene multiplicando los coeficientes por cada una de las variables (X1 y Y1)
ac1_X <- as.matrix(X) %*% ac$xcoef[, 1]
ac1_Y <- as.matrix(Y) %*% ac$ycoef[, 1]

#Visualización de los primeros 20 datos
ac1_X[1:20,]
ac1_Y[1:20,]

# Correlación canónica entre variable X1 y Y1
cor(ac1_X,ac1_Y)

# Verificación de la correlación canónica
assertthat::are_equal(ac$cor[1], 
                      cor(ac1_X,ac1_Y)[1])


# Análisis canónico con dos pares de variables

# Cálculo de las variables X2 y Y2
ac2_X <- as.matrix(X) %*% ac$xcoef[, 2]
ac2_Y <- as.matrix(Y) %*% ac$ycoef[, 2]

# Agregamos las variables generadas a la matriz original de penguins
ac_df <- penguins %>% 
  mutate(ac1_X=ac1_X,
         ac1_Y=ac1_Y,
         ac2_X=ac2_X,
         ac2_Y=ac2_Y)

# Visualización de los nombres de las variables
colnames(ac_df)

# Generación del gráfico scater plot para la visualización de X1 y Y1
ac_df %>% 
  ggplot(aes(x=ac1_X,y=ac1_Y))+
  geom_point(color="indianred1")

# Generación de un boxplot
ac_df %>% 
  ggplot(aes(x=especie,y=ac1_X, color=especie))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)+
  ggtitle("Variable Canónica X1 contra Especie")

# Interpretación: se observa una correlacion entre la variable canónica X1 y la variable latente Especie
ac_df %>% 
  ggplot(aes(x=especie,y=ac1_Y, color=especie))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)+
  ggtitle("Variable Canónica Y1 contra Especie")

ac_df %>% 
  ggplot(aes(x=ac1_X,y=ac1_Y, color=especie))+
  geom_point()+
  ggtitle("Variable Canónica X1 contra Y1")

# Scarter plot con las variables canonicas X2 y Y2 separadas por género.

ac_df %>% 
  ggplot(aes(x=ac2_X,y=ac2_Y, color=genero))+
  geom_point()+
  ggtitle("Variable Canónica X2 contra Y2")

#Interpretación: No se identifica correlación entre el conjunto de variables X2 y Y2 separadas por género.
