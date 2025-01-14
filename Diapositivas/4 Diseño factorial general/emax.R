library(readxl)
# lectura de los datos
# factores: di�metro y calidad
# respuesta: Emax
emax <- read_excel("Emax.xlsx")
emax$di�metro <- as.factor(emax$di�metro)
emax$calidad <- as.factor(emax$calidad)
# anova
mod1 <- aov(Emax~di�metro*calidad, data=emax)
# valores medios para los tratamientos
model.tables(mod1,type="means",se=T)
# gr�fico de interacciones
interaction.plot(emax$di�metro,emax$calidad,emax$Emax,fun=mean)
# gr�fico de efectos principales e interacciones
# valores ajustados y error est�ndar
library(phia)
grafica <- interactionMeans(mod2)
grafica
plot(grafica)
# Estudio de efectos de dispersi�n
# creaci�n de emax1: es emax con el agregado de 
# residuos al cuadrado(resi2)
emax1<-cbind(emax,resi2=mod1$residuals*mod1$residuals)
# anova de los residuos al cuadrado
mod3 <- aov(resi2~di�metro*calidad, data=emax1)

