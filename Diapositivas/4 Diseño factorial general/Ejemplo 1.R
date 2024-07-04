 #
 # Cargar los siguientes paquetes para leer y guardar archivos excel
 #
library(readxl)
library(writexl)
 #
 # Construcción del plan experimental y aleatorización de los ensayos
 #
 # La función expand.grid construye un "data frame" con las combinaciones
 # de los elementos de los vectores A(temperatura) y B(material) 
plan <- expand.grid(A=c(15, 70, 125), B=c("M1", "M2", "M3")) 
 #
 # Generación del orden en el que se realizarán los ensayos -aleatorización-
 # se fija la "semilla" para la aleatorización en el siguiente paso
set.seed(23897) 
 # se genera una permutación aleatoria de los números 1 a 36 que indica el orden
 # en el que se realizará cada ensayo
orden <- sample(36)
 # construcción del plan completo: cuatro réplicas y orden aleatorio de los ensayos
 #
 # plan con las 4 réplicas y el orden aleatorio de las pruebas
 #
ensayo <- 1:36
planr <- data.frame(ensayo,rbind(plan, plan, plan, plan),orden) 
 # escritura del plan en cuaderno excel, si fuera necesario
write_xlsx(planr,"plan_baterias.xlsx")
 # Luego se realizan los ensayos y se cargan en la hoja excel, 
 # y se procede a la lectura del plan con los resultados
resultados<-read_xlsx("plan_baterias_n.xlsx")
 # Para realizar el ANOVA, se definen como factores, las columnas que contienen
 # los niveles de los mismos para cada ensayo
 #
resultados$A <- as.factor(resultados$A)
resultados$B <- as.factor(resultados$B)
# Medidas descriptivas
# Cálculo de promedio y desviación estándar de duración para cada
# temperatura ensayada
medias.temp <- tapply(resultados$duracion, resultados$A, mean)
desvest.temp <- tapply(resultados$duracion, resultados$A, sd)
medias.mat <- tapply(resultados$duracion, resultados$B, mean)
desvest.mat <- tapply(resultados$duracion, resultados$B, sd)
descri.temp <-cbind(medias.temp,desvest.temp)
descri.mat <- cbind(medias.mat,desvest.mat)
descri.temp # muestra media y desviación estándar para cada temperatura ensayada
descri.mat # muestra media y desviación estándar para cada material ensayado
media.trat <- tapply(resultados$duracion,list(resultados$A,resultados$B),mean)
media.trat # muestra la duración media para cada tratamiento
desvest.trat <- tapply(resultados$duracion,list(resultados$A,resultados$B),sd)
desvest.trat # muestra las desviaciones estandar para cada tratamiento
 # Gráficos de puntos
 # Duración según temperaturas ensayadas
stripchart(resultados$duracion ~ resultados$A, vertical=TRUE, pch=19,
           xlab="Temperatura",
           ylab= "Duración", 
           main="Duración de baterías según temperatura")
points(medias.temp,col="red" ,pch=18)
lines(medias.temp,col="red")
 # Duración según tipo de material ensayado
stripchart(resultados$duracion ~ resultados$B, vertical=TRUE, pch=19,
           xlab="Material",
           ylab= "Duración", 
           main="Duración de baterías según Tipo de material")
points(medias.mat, col="red", pch=18)
lines(medias.mat,col="red")
################################################################
# Otra forma de presentar los gráficos de efectos principales
#
plot(medias.mat,type="b",
     ylab="duración",
     xlab="Material")
#
plot(medias.temp,type="b",
     ylab="duración",
     xlab="temperatura")
################################################################

#
# Interacción doble
#
interaction.plot(x.factor=resultados$A, trace.factor=resultados$B, 
                 response=resultados$duracion,
                                   ylab="duracion", 
                                   xlab="temperatura", 
                                   trace.lab="material",
                                   main="Efecto de la temperatura según tipo 
                                  de material")
#
# Análisis de la variancia
mod1 <- aov(resultados$duracion~resultados$A*resultados$B)
summary(mod1)
#
 #library(lattice)
medias <- model.tables(mod1, type="means",se=T)
# otra forma de obtener gráficos
library(phia)
grafica <- interactionMeans(mod1)
grafica
plot(grafica)
#
# verificación de supuestos
#
plot(mod1, which=2, id.n=0, pch=19, 
     main="Gráfico de probabilidad normal", caption="" )

plot(mod1, which=1, id.n=0, pch=19, add.smooth=F, 
     main="Residuos vs. ajustados", caption="" )

plot(resultados$orden, residuals(mod1), pch=19, 
     main="Gráfico de residuos versus orden")

shapiro.test(mod1$residuals)
