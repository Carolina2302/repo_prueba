library(readxl)
Tarea<-read_excel('Tarea.xlsx', sheet = 'Reclasificación-LIGA COLOM-2018')
View(Tarea)

#Diagrama de dispersion
plot(Tarea)
summary(Tarea)

#Creacion del modelo
#regresion lineal
#Y~x
attach(Tarea)
fit<-lm(formula = POSICIÓN~GF+GC+PT,data =Tarea)
summary(fit)
#H0= Bo=0
#H1: Bo=~0
#No se rechaza H0, no aparecen ***, No es significativo, Bo=0 en GF y GC
fit<-lm(formula = POSICIÓN~PT,data =Tarea)
summary(fit)

#Como no hay intercepto la recta ajustada es:
#Y=0,14479X 
#R^2:0.4964 = la recta ajustada explica de buena-regular manera la relación entre x y y

anova(fit)
#Ho: B1=0
#Ho:B1=~0
#Con una confianza del 95%, p~valor < alfa por lo que se rechaza Ho, y se considera que el 
#modelo es significativo, por lo tanto se puede decir que hasta el momento el modelo es válido,
#es decir la Posición se debe a los puntos totales. 


#Supuesto#Graficas
par(mfrow=c(2,2))
plot(fit)

#SUPUESTOS

#Normalidad
#H0=Los residuos se comportan normalmente con media=0 con varianza estimada de 76,07
#H1=Los residuos No se comportan normalmente con media=0 con varianza estimada de 76,07
#La varinza estimada la tomo del anova Mean Sq de los residuales
shapiro.test(fit$residuals)
#Como P-value>0,05 no se rechaza H0


#Independencia
library(car)
durbinWatsonTest(fit)
#Ho: Los residuos son independientes.
#H1: Los residuos son dependientes.
# Aplicando una prueba de DurbinWatson, con una confianza del 95% y un pvalor=0 menor a 
#alfa, se rechaza Ho, por lo que se puede decir que los residuos son dependientes.


#Homocedasticidad
ncvTest(fit)
#H0=los residuos son homocedasticos
#H1=los residuos no son homocedasticos
#Con una confianza del 95% y un P=0,11608 mayor al alfa, no se rechaza Ho, por lo tanto se
#puede decir que los residuos cuenta con varianza constante


#EL MODELO NO ES VALIDO, ya que solo cumple con 2 supuestos