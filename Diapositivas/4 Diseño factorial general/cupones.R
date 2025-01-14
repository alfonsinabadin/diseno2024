# Dise�o factorial general. Ejercicio propuesto 2
library(readxl)
# Lectura de datos
plan <- read_excel("cupones.xlsx")
# Definici�n de factores
plan$aviso <- as.factor(plan$aviso)
plan$frecuencia <- as.factor(plan$frecuencia)
plan$aviso.r [plan$aviso == "media"] <- c("1. Media") 
plan$aviso.r [plan$aviso == "completa"] <- c("2. Completa")
plan$aviso.r [plan$aviso == "doble"] <- c("3. Doble") 
medias.aviso <- tapply(plan$cupones, plan$aviso.r, mean)
medias.frecuencia<-tapply(plan$cupones, plan$frecuencia, mean)
plot(medias.aviso,type="b")
plot(medias.frecuencia,type="b")
interaction.plot(x.factor=plan$frecuencia, trace.factor=plan$aviso.r, 
                 response=plan$cupones)
library(daewr)
# Test de no aditividad de Tukey
# Se prepara un data frame que contiene en la primera columna, la varible 
# respuesta, y luego, los factores 
plan1 <- data.frame(plan$cupones, plan$aviso, plan$frecuencia)
mod0 <- Tukey1df(plan1) 
# al no rechazar la hip�tesis de no aditividad, se plantea el anova sin
# interacci�n
mod1 <- aov(plan$cupones~plan$aviso.r+plan$frecuencia)
library(phia)
grafica <- interactionMeans(mod1)
grafica
plot(grafica)
