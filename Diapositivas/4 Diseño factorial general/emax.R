library(readxl)
# lectura de los datos
# factores: diámetro y calidad
# respuesta: Emax
emax <- read_excel("Emax.xlsx")
emax$diámetro <- as.factor(emax$diámetro)
emax$calidad <- as.factor(emax$calidad)
# anova
mod1 <- aov(Emax~diámetro*calidad, data=emax)
# valores medios para los tratamientos
model.tables(mod1,type="means",se=T)
# gráfico de interacciones
interaction.plot(emax$diámetro,emax$calidad,emax$Emax,fun=mean)
# gráfico de efectos principales e interacciones
# valores ajustados y error estándar
library(phia)
grafica <- interactionMeans(mod2)
grafica
plot(grafica)
# Estudio de efectos de dispersión
# creación de emax1: es emax con el agregado de 
# residuos al cuadrado(resi2)
emax1<-cbind(emax,resi2=mod1$residuals*mod1$residuals)
# anova de los residuos al cuadrado
mod3 <- aov(resi2~diámetro*calidad, data=emax1)

