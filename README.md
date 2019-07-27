# Precios-Oferta
Trabajo Analítica Predictiva-Precios Oferta Energía Eléctrica

install.packages("readr")
install.packages("dplyr")
# Cargar el paquete
library("readr")
library(dplyr)

library(astsa)        # Applied Statistical Time Series Analysis
library(TSA)          # Time Series Analysis
library(forecast)     # Forecasting Functions for Time Series and Linear Models
library(urca)         # Unit Root and Cointegration Tests for Time Series Data
library(moments)      # Moments, cumulants, skewness, kurtosis and related tests
library(normtest)     # Tests for Normality
library(fUnitRoots)



###Lectura de datos, importacion del archivo con todas las generadoras, se decide aplicar filtro con Guavio, San Carlos y Chivor con base en su capacidad efectiva, son las de mayor capacidad
Datos<-read.csv("C:/Users/Usuario/Desktop/Especialización/Analítica predictiva/Trabajo/Entregable/Precio_Oferta.csv",sep =";", header = TRUE,   skip = 0)

View(Datos)


###Extracción de la columna Precio de Oferta declarado
Generador <- Datos$SAN.CARLOS
mode(Generador)


####CONVERTIR DATOS GUAVIO EN TIPO SERIE DE TIEMPO CON TS
Generador.ts <- ts(Generador, start = c(2018,1),frequency = 365)
print(Generador.ts)

plot.ts(Generador.ts,main = "Serie de tiempo Chivor", col = "red")


####Estadistica descriptiva
#Guavio
Generador_summary<-summary(Generador)
print(Generador_summary)


#Generador
mean(Generador)     # mean
median(Generador)   # median
var(Generador)      # variance
sd(Generador)       # standar deviation
min(Generador)      # minimum
max(Generador)      # maximum


quantile(Generador, probs = seq(0, 1, 0.25), names = TRUE, type = 7) # quartiles


# Sesgo and curtosis (in the "moments" package)
skewness(Generador) # skewness
kurtosis(Generador) # curtosis


# Prueba de Portmanteau para no autocorrelación en los datos (sirve para medir la correlación entre los datos La autocorrelación puede reducir la exactitud de un modelo predictivo)
###1) Si V alor p < 0.05, se rechaza H0.
###2) Si V alor p > 0.05, no se rechaza H0.
####a funci ´on de autocorrelaci ´on estimada ??^(k)
###H0 : ???k, ??^(k) = 0

###Por lo tanto se se rechaza Ho,es decir no hay autocorrelacion en los datos 
Box.test(Generador, lag=7, type = "Ljung-Box", fitdf = 0)


######autorima########################
modelo_Generador<-auto.arima(Generador.ts)
summary(modelo_Generador)


############Estacionariedad########################
#####debido a que la serie no es estacionaria (compportamiento normal) , no se puede sostener el supuesto de que ha sido generada por un proceso con varianza constante en el tiempo, la soluci´on es transformar la serie mediante alg´un m´etodo que estabilice la varianza



# Grafico de los datos

#ln Tomando logaritmos a la serie (la transformaci´on Box-Cox m´as utilizada para las series econ´omicas o log de los datos) se observa que la amplitud de estos ciclos se ha homogeneizado y que la variabilidad de la serie es mucho m´as estable en el tiempo (la figuraderecha del gr´afico). Se puede concluir que la serie estacionaria en varianza

# ACF de los datos (¿es estacionaria la serie?) Uso de la función Acf () en el paquete de "pronóstico"

###La serie no es estacionaria en media cuando presenta tendencia o varios tramos con medias diferentes.si la serie no es estacionaria en media se puede lograr la estacionariedad transform´andola tomando diferencias (diferenciando la serie)

Acf(Generador, main="ACF Generador")


### Diferencia estacionaria para lograr estacionariedad en media y varianza.

###par() para poner dos graficos juntos
par(mfcol=c(1,2))

par
##Generador##
par(mfcol=c(1,2))

par

ts.plot(diff(Generador),xlab="", ylab="", main="Primera diferenciación del Precio Generador")   # Primera diferenciación de los datos

ts.plot(diff(log(Generador)), xlab="", ylab="", main="Primera diferencia con Log Generador", col= "red") # Log Precio 


##ACF##
#Generador
par(mfcol=c(1,2))

par
Acf(diff(Generador), main="ACF Generador")
Acf(diff(log(Generador)), main="ACF Log Generador ")


###con los graficos se concluye que con la primera diferenciación se obtiene una serie ya que el correlagrama decrece r´apidamente hacia cero
##de esta forma d=1
#Una vez determinado el orden de diferenciaci´on d, se tiene la transformaci´on estacionaria de la serie Zt = (1 ??? L)d Yt que puede representarse mediante un proceso ARMA(p, q) estacionario.

####################### INDENTIFICACIÓN DEL MODELO #######################
# ACF y PACF el PRECIO (identificacion del modelo)

##Generador
par(mfcol=c(1,2))

par

Acf(diff(log(Generador)),  main="ACF Simple Generador", ylim=c(-0.5,0.5))
Pacf(diff(log(Generador)), main="ACF Parcial Generador", ylim=c(-0.5,0.5))


# Para incluir el intercepto usando la función arima (), entonces:
#Guavio
fit_1 <- arima(diff(log(Generador)), order = c(2,1,3), include.mean = TRUE) # ARMA(1,0,0) on the first difference of the log(GDP)
fit_2 <- arima(diff(log(Generador)), order = c(1,1,2), include.mean = TRUE) # ARMA(0,0,2) on the first difference of the log(GDP)
summary(fit_1)
summary(fit_2)


# # The same models but including the constant (drift) term. NOTE: use the sarima() function in the "astsa" package
#Guavio
fit_1 <- sarima(log(Generador), p=2, d=1, q=3, no.constant=FALSE)  # ARIMA(1,1,0)
fit_2 <- sarima(log(Generador), p=1, d=1, q=2, no.constant=FALSE)  # ARIMA(0,1,2)
fit_1
fit_2


# # Convert ARMA process to infinite MA process
# psi <- ARMAtoMA(ar=fit_1$fit$coef[1], ma=0, 10)
# dev.off()
# plot(psi, type="o", ylab = expression(psi[j]), xlab = "j", las=1)

# The models can also be estimated by using the Arima() function in the "forecast" package
#Guavio
fit_1 <- Arima(log(Generador), order = c(2,1,3), include.constant = TRUE) # ARIMA(2,1,3) (p,d,q) autorregresiva, integrada (diferencias diff) y media móvil
fit_2 <- Arima(log(Generador), order = c(1,1,2), include.constant = TRUE) # ARIMA(1,1,2) (p,d,q) autorregresiva, integrada (diferencias diff) y media móvil
summary(fit_1)
summary(fit_2)


# # Convert ARMA process to infinite MA process
# psi <- ARMAtoMA(ar=fit_1$fit$coef[1], ma=0, 10)
# dev.off()
# plot(psi, type="o", ylab = expression(psi[j]), xlab = "j", las=1)

# The models can also be estimated by using the Arima() function in the "forecast" package
#Chivor
fit_1 <- Arima(log(Generador), order = c(2,1,3), include.constant = TRUE) # ARIMA(1,1,0) (p,d,q) autorregresiva, integrada (diferencias diff) y media móvil
fit_2 <- Arima(log(Generador), order = c(1,1,2), include.constant = TRUE) # ARIMA(0,1,2) (p,d,q) autorregresiva, integrada (diferencias diff) y media móvil
summary(fit_1)
summary(fit_2)

# Convert ARMA process to infinite MA process###################### 
#Chivor
psi_ar <- ARMAtoMA(ar=fit_1$coef[1],   ma=3, lag.max=7)
psi_ma <- ARMAtoMA(ma=fit_2$coef[1:2], ar=1, lag.max=7)
par(mfcol=c(1,2))
plot(psi_ar, type="o", ylab = expression(psi[j]), xlab = "j", las=1, main="ARIMA(2,1,3) Chivor")
plot(psi_ma, type="o", ylab = expression(psi[j]), xlab = "j", las=1, main="ARIMA(1,1,2) Chivor")

######################### Diagnostic checking #########################
#Generador
e_1 <- residuals(fit_1) # also e <- fit_1$residuals
e_2 <- residuals(fit_2)


# Standardized residuals: residuals/sigma. NOTE: use the standard() function in the "TSA" package
#Generador
sr_1 <- rstandard(fit_1)  # also 
sr_1 <- residuals(fit_1)/sqrt(fit_1$sigma2) 
sr_2 <- rstandard(fit_2)
sr_2 <- residuals(fit_2)/sqrt(fit_2$sigma2)


# Plot of residuals
#Generador
par(mfcol=c(3,2))
ts.plot(sr_1, xlab="t", ylab="Standardized residuals", main="ARIMA(3,1,2) Guavio sr 3")
Acf(sr_1, main="Acf Generador sr 1")
Pacf(sr_1, main="Pacf Generador sr 1")
ts.plot(sr_2, xlab="t", ylab="Standardized residuals", main="ARIMA(0,1,2) Guavio sr 4")
Acf(sr_2, main="Acf Generador sr 2")
Pacf(sr_2, main="Pacf Generador sr 2")


# Descriptive statistics of residuals
#Generador
summary(sr_1); summary(sr_2)
sd(sr_1); sd(sr_2)
skewness(sr_1); skewness(sr_2)
kurtosis(sr_1); kurtosis(sr_2)


# Pormanteau test
#Generador
Box.test(sr_1, lag=20, type="Box-Pierce", fitdf=2) # where fitdf = p+q
Box.test(sr_2, lag=20, type="Ljung-Box",  fitdf=3)

# Histogram
#Generador
par(mfcol=c(1,2))
hist(sr_1, prob=TRUE, ylim=c(0,0.6), xlab = "Standardized residuals", main="ARIMA(2,1,3) sr1 Generador")
lines(seq(min(sr_1),max(sr_1),length=100), dnorm(seq(min(sr_1),max(sr_1),length=100),mean(sr_1), sd(sr_1)), col=2)
#
hist(sr_2, prob=TRUE, ylim=c(0,0.45), xlab = "Standardized residuals", main="ARIMA(2,1,1) sr2 Generador")
lines(seq(min(sr_2),max(sr_2),length=100), dnorm(seq(min(sr_2),max(sr_2),length=100),mean(sr_2), sd(sr_2)), col=2)


# Jarqueâ???"Bera test for normality in the innovations
# In the "moments" package
#Generador
jarque.test(as.vector(sr_1))
jarque.test(as.vector(sr_2))

# In the "normtest" package
#Generador
jb.norm.test(sr_1)
jb.norm.test(sr_2)

# QQ plot
#Generador
par(mfcol=c(1,2))
qqnorm(sr_1, main="ARIMA(1,1,0) sr1 Chivor")
abline(0,1, col=2)
qqnorm(sr_2, main="ARIMA(0,1,2) sr2 Chivor")
abline(0,1, col=2)

# Kolmogorov-Smirnov Tests
#Chivor
ks.test(sr_1,"pnorm", mean(sr_1), sd(sr_1))   # Kolmogorov-Smirnoff test
ks.test(sr_2,"pnorm", mean(sr_2), sd(sr_2))

############################# Forecasting #############################
#Generador
Generador_forecasts <- forecast(fit_1, h=7) # correspond to nine quarters (i.e. one year and one quarter)


summary(Generador_forecasts)
dev.off()
plot(Generador_forecasts, ylab="log Generador", main="")

n <- length(Generador_forecasts$x)
h <- 7
X <- matrix(NA,n+h,3)
X[n+1:h,1] <- exp(Generador_forecasts$lower[,2])
#X[1:n,2] <- x   #Sin utilidad
X[n+1:h,2] <- exp(Generador_forecasts$mean)
X[n+1:h,3] <- exp(Generador_forecasts$upper[,2])

ts.plot(X, lty=c(2,1,2), col=c(2,1,2), ylab="GDP", main="")
abline(v=n, lty=2)

ts.plot(cbind(exp(Generador_forecasts$lower[,2]), exp(Generador_forecasts$mean), exp(Generador_forecasts$upper[,2])), lty=c(2,1,2), col=c(2,1,2))

#Export con la Predicción de los precios
Pronostico = round(exp(Generador_forecasts$mean),digits = 2)
write.table(Pronostico, file="C:/Users/Usuario/Desktop/Especialización/Analítica predictiva/Trabajo/Entregable/Pronostico_Precios_SanCarlos.csv", sep = ";")
mode(Pronostico)

acf(Generador_forecasts$residuals, lag.max=20)
Box.test(Generador_forecasts$residuals, lag=20, type = "Box-Pierce", fitdf = 0)
Box.test(Generador_forecasts$residuals, lag=20, type = "Ljung-Box", fitdf = 0)
plot.ts(Generador_forecasts$residuals)            # make time plot of forecast errors



#Particion de los datos
install.packages("caret")
library(caret)


Generador.training.ids <- createDataPartition(Datos$SAN.CARLOS, p=0.8, list = F)

Datos.training <- Datos[Generador.training.ids,]
View(Datos.training)
Datos.validation <- Datos[-Generador.training.ids,]


Generador_Training <- Datos.training$SAN.CARLOS ###Modificar según el generador a elegir



#Prediccion
Prediccion_PreciosSanCarlos_Part <- predict(Datos.training$SAN.CARLOS)  ###Modificar según el generador a elegir
Prediccion_PreciosSanCarlos_Part = round((Prediccion_PreciosChivor_Part),digits = 2)
write.table(Prediccion_PreciosChivor_Part, file="C:/Users/Usuario/Desktop/Especialización/Analítica predictiva/Trabajo/Entregable/Pronostico_Part_Precios_Chivor.csv", sep = ";")
