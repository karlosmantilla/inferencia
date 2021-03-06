
load('Datos/estudiantes.RData')
edades<-poblacion[,c(2,5,15)]
head(edades)

library(samplingbook) # Para calcular el tama�o de la muestra

tama�o<-sample.size.prop(e=0.05, P = 0.5, N = length(edades$EDAD), level = 0.95)$n # Vamos a usar P = 0.5
muestra<-sample(1:length(edades$EDAD), size = tama�o, replace = FALSE) # Dise�amos un M.A.S.
mas.edades<-edades[muestra,] # Tenemos la muestra
head(mas.edades)

# Media Poblacional
round(mean(edades$EDAD),1)

# Media Muestral
round(mean(mas.edades$EDAD),1)

N<-length(edades$EDAD) # El tama�o de la poblaci�n
round(var(edades$EDAD),3) # Varianza sin correcci�n

round(var(edades$EDAD)*((N-1)/N),3) # Varianza Corregida

#install.packages('rafalib')
library(rafalib)
round(popvar(edades$EDAD),3)

round(var(mas.edades$EDAD),3)

#install.packages("bbmle")
#install.packages("stats4")
library(stats4) # para la funci�n mle
library(bbmle) # para la funci�n mle2

x<-edades$EDAD # Para trabajar con los datos muestrales que requerimos
NegLogLik = function(mu,sigma){-sum(dnorm(x,mu,sigma,log = TRUE))}

m.mu<-mean(mas.edades$EDAD) # Media
m.sigma<-sd(mas.edades$EDAD) # Desviaci�n Est�ndar

EMV1 = mle(NegLogLik, start = list(mu=m.mu, sigma=m.sigma))
summary(EMV1) # Para visualizar los valores

EMV2 = mle2(NegLogLik,start = list(mu=m.mu,sigma=m.sigma), data = list(x)) # La otra funci�n
summary(EMV2)

# La gr�fica de la distribuci�n normal est�ndar
library(RcmdrMisc)
x <- seq(-3, 3, length.out=1000)  
plotDistr(x, dnorm(x, mean=0, sd=1), cdf=FALSE, xlab="x", ylab="Density", main=paste("Distribuci�n Normal Est�ndar"), 
  regions=list(c(-1.96, 1.96)), col=c('#BEBEBE', '#BEBEBE'), legend.pos='topright')

# Primero los descriptivos b�sicos:
summary(mas.edades$EDAD)

sd(mas.edades$EDAD)

media <- mean(na.omit(mas.edades$EDAD)) # Pedimos la media
desv <- sd(na.omit(mas.edades$EDAD)) # La desviaci�n est�ndar
N <- length(na.omit(mas.edades$EDAD)) # El tama�o v�lido de la muestra
error.est <- desv/sqrt(N) # Calculamos el error est�ndar
error <- 2*error.est # Fijamos Z=2 para indicar un nivel de confianza de 95%
lim.inf <- media-error # Límite inferior del intervalo
lim.sup <- media+error # Límite superior del intervalo

# Guardamos todos los datos generados en un objeto data frame
resultado1 <- data.frame(media, desv, N, error.est, error, lim.inf, lim.sup)
resultado1

library(TeachingDemos) # Para usar la funci�n z.test()

z.test(mas.edades$EDAD, stdev = sd(mas.edades$EDAD), mu = mean(edades$EDAD))

# La Gr�fica de la distribuci�n t-student:
curve(dt(x, df = 30), from = -3, to = 3, lwd = 3, ylab = "y", main = "Distribuci�n t-student")
ind <- c(1, 2, 3, 5, 10)
for (i in ind) curve(dt(x, df = i), -3, 3, add = TRUE,col=i)

media <- mean(na.omit(mas.edades$EDAD))
desv <- sd(na.omit(mas.edades$EDAD))
N <- length(na.omit(mas.edades$EDAD))
error.est <- desv/sqrt(N)
error <- qt(0.975, df= N-1) * error.est # Usar el cuantil 0.975 de t
lim.inf <- media-error
lim.sup <- media+error

resultado2 <- data.frame(media, desv, N, error.est, error, lim.inf, lim.sup)
resultado2

t.test(mas.edades$EDAD, mu = mean(edades$EDAD))

library(descr)
freq(mas.edades$SEXO_BIOLOGICO, plot = TRUE)

cat <- ifelse(mas.edades$SEXO_BIOLOGICO=="HOMBRE", 1, 0)
prop.table(table(cat))

p <- mean(cat)
p # Esta es la proporci�n de estudiantes hombres

n <- length(cat) # Tama�o de la muestra 
error.est.p <- sqrt((p*(1-p))/n) # Error est�ndar de la propoci�n
error.p <- 2 * error.est.p # Usamos Z = 2 para indicar un nivel de confianza del 95%
lim.inf.p <- p - error.p 
lim.sup.p <- p + error.p

result.p <- data.frame(p, n, error.est.p, error.p, lim.inf.p, lim.sup.p)
result.p

library(Hmisc)
binconf(x = 218, n = 379)

library(plotrix)
plotCI(mean(mas.edades$EDAD), ui = lim.sup, li = lim.inf, xlab = 'Edades', ylab = 'Intervalo de Confianza', pch = 19)
