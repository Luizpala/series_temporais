
## [Parte 1](#)

```r
#=============================
# SERIES TEMPORAIS - PARTE 1 
#=============================
# Importando a serie temporal 
dados = read.csv("C:/Users/Desktop/^BVSP.csv")

# pacotes necessarios
library("lmtest")
library("randtests")
library("forecast")

#========
# Definindo a serie com o comando ts
y = ts(dados$Close)

# Grafico
plot(y, ylab = "Fechamento BVSP")

# definindo a escala de tempo para dias 
data = as.Date(dados$Date,tryFormats = c("%Y-%m-%d"))

# novo grafico com a data
plot(data, y, ylab = "Fechamento BVSP", type = "l",
     col = "steelblue", xlab = "Data")

#========
# funcoes acf e pacf
acf(y)
pacf(y)
#========
# analise de tendencia
# ho: a serie tem tendencia
cox.stuart.test(y)

# removendo a tendencia da serie
y1 = diff(y, lag = 1)
plot(y1, ylab = "Série diferenciada")

# ou ainda
plot(data[2:length(data)], y1, type = "l",  ylab = "Série diferenciada",
     xlab = "data")

# funcoes acf e pacf de y1
acf(y1)
pacf(y1)

#=============================
# Modelo AR(1)
m1 = arima(y, order = c(1,1,0))
coeftest(m1)

#========
# verificando os residuos do modelo
resid = m1$residuals
acf(resid)

# teste de Ljung–Box
Box.test(resid, lag = 24, type = c("Ljung-Box"), fitdf = 1)

#=============================
# aumentando a ordem do modelo para Ar(7) com defasagens em 1 e 7

m2 = arima(y, order = c(7, 1, 0),
           fixed = c(NA, 0, 0, 0, 0, 0, NA))
coeftest(m2)

#========
# verificando os residuos do modelo
resid = m2$residuals
acf(resid)

# teste de Ljung–Box
Box.test(resid, lag = 24, type = c("Ljung-Box"), fitdf = 2)

# aic e log-verossimilhanca do modelo 2
m2$loglik
BIC(m2)
AIC(m2)

# previsoes
plot(data, y, ylab = "Fechamento BVSP", type = "l",
     col = "steelblue", xlab = "Data")
plot(forecast(m2, h = 15, level = 95))

# para ver as previsoes
forecast(m2, h = 15, level = 95)
#=============================
#=============================
rm(list=ls())
#=============================
#=============================
# MODELOS SAZONAIS

# pacotes necessarios
library("lmtest")
library("randtests")
library("forecast")
library("uroot")

# Monthly Airline Passenger Numbers 1949-1960
y = AirPassengers
plot(y)

# aplicando o log
y = log(y)

# funcoes acf e pacf
acf(y)
pacf(y)

# analise de tendencia e sazonalidade, respectivamente.
Box.test(y)
ch.test(y) # null hypothesis of a stable seasonal pattern

# funcao que ajuda escolher o numero de diferencas sazonais
nsdiffs(y, test = c("seas"))

# logo usaremos 1 diferenca para remover tendencia e 1 sazonal
y1 = diff(y, lag = 1)
y2 = diff(y1, lag = 12)

# grafico da serie diferenciada
plot(y2, type = "l")

# acf e pacf de y2 com funcoes do forecast
Acf(y2, lag.max = 20)
Pacf(y2, lag.max = 26)

# modelos
m1 = arima(y, order = c(0,1,1), seasonal = c(1,1,0))
coeftest(m1)

#========
# verificando os residuos do modelo
resid = m1$residuals
acf(resid)

# teste de Ljung–Box
Box.test(resid, lag = 24, type = c("Ljung-Box"), fitdf = 2)

# grafico da previsao
plot(forecast(m1, h = 25, level = 95))
#=============================
```
