## Parte 2

```r
#=============================
# SERIES TEMPORAIS - PARTE 2 - VOLATILIDADE 
rm(list=ls())
#=============================
# Importando a serie temporal 
dados = read.csv("C:/Users/DESICET/Desktop/^BVSP.csv")

# pacotes necessarios
library("lmtest")
library("randtests")
library("forecast")
library("stochvol")
library("fGarch")
library("rugarch")
#========
# Definindo a serie com o comando ts
y = ts(dados$Close)

# definindo a escala de tempo para dias 
data = as.Date(dados$Date,tryFormats = c("%Y-%m-%d"))

# criando o retorno
ret = logret(y)

# grafico do retorno
plot(data[2:length(data)], ret, xlab = "Período",
     ylab = "log-retorno", type = "l")

# acf e pacf do retorno
acf(ret)
pacf(ret)
#========
# modelando a media do retorno

m = arima(ret, order = c(7,0,0), include.mean = F,
          fixed = c(NA, NA, 0, 0, 0, 0, NA))
coeftest(m)

# verificando os residuos do modelo
resid = m$residuals
acf(resid, lag.max = 15)

# teste de Ljung–Box
Box.test(resid, lag = 24, type = c("Ljung-Box"), fitdf = 2)

# modelo ajustado ARMA(7), com def. 1, 2 e 7.

# acf do residuo quadrado 
acf(resid^2)

# teste do multiplicador de lagrange
library("aTSA")
arch.test(m) # h1: heteroscedastic

#-----------------------------
# modelos garch (normal)

# modelo garch(1, 1)
options(scipen = 99)
m_garch = garchFit(resid ~ garch(1,1),
                   cond.dist = "norm", data = resid,
                   include.mean = F)
print(m_garch)

# residuos padronizados do modelo
res_garch = residuals(m_garch, standardize = TRUE)
acf(res_garch)
acf(res_garch^2)

# alguns graficos do ajuste
plot(m_garch, which = 2)
plot(m_garch, which = 13)

# limpando
rm(m_garch, res_garch)
#----------
#----------
# com o pacote rugarch (recomendo)

# passo 1: especificando o modelo
espec = ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0,0)),
                   distribution.model = "norm",
                   fixed.pars = list(mu = 0))
# passo 2: estimando os parametros
m_garch = ugarchfit(data = resid, spec = espec)

# resumo do modelo
print(m_garch)
coef(m_garch)

# residuos padronizados do modelo
res_garch = residuals(m_garch, standardize = TRUE)
acf(res_garch)
acf(res_garch^2)

# alguns graficos do ajuste
plot(m_garch, which = 9)
plot(m_garch, which = 3)
plot(m_garch, which = 12)

# previsao da volatilidade em t + h
prev = ugarchforecast(m_garch, n.ahead = 20)
plot(prev, which = 3)

# plot da previsao do retorno em t + h
plot(prev, which = 1)

# limpando
rm(m_garch, res_garch, espec, prev)
#-----------------------------
#-----------------------------
# modelos garch (t assimetrico)

# passo 1: especificando o modelo
espec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0,0)),
                   distribution.model = "sstd",
                   fixed.pars = list(mu = 0))
# passo 2: estimando os parametros
m_garch = ugarchfit(data = resid, spec = espec)

# resumo do modelo
print(m_garch)
coef(m_garch)

# residuos padronizados do modelo
res_garch = residuals(m_garch, standardize = TRUE)
acf(res_garch)
acf(res_garch^2)

# alguns graficos do ajuste
plot(m_garch, which = 9)
plot(m_garch, which = 3)
plot(m_garch, which = 12)

# previsao da volatilidade em t + h
prev = ugarchforecast(m_garch, n.ahead = 20)
plot(prev, which = 3)

# plot da previsao da serie em t + h
plot(prev, which = 1)

# limpando
rm(m_garch, res_garch, espec, prev)
#-----------------------------
#-----------------------------
# UMA FORMA ALTERNATIVA (AJUSTAR CONJUNTAMENTE)

# especificando o modelo
espec = ugarchspec(mean.model = list(armaOrder = c(7,0)),
                  variance.model = list(model = "sGARCH",
                  garchOrder = c(2, 1)),
                  distribution.model = "norm",
                  fixed.pars = list(ar3 = 0, ar4 = 0, ar5 = 0, ar6 = 0, mu = 0))
# estimando os parametros
m = ugarchfit(data = ret, spec = espec, solver = "hybrid",
              solver.control = list(tol=1e-20,delta=1e-20) )

# resumo do modelo
print(m)

# residuos padronizados do modelo
res_garch = residuals(m, standardize = TRUE)
acf(res_garch)
acf(res_garch^2)

# alguns graficos do ajuste
plot(m, which = 9)
plot(m, which = 3)
plot(m, which = 12)

# previsao da volatilidade em t + h
prev = ugarchforecast(m, n.ahead = 20)
plot(prev, which = 3)

# plot da previsao do retorno em t + h
plot(prev, which = 1)


# limpando
rm(m, res_garch, espec, prev)
#-----------------------------
# APARCH
#-----------------------------
# especificando o modelo
espec = ugarchspec(mean.model = list(armaOrder = c(7,0)),
                   variance.model = list(model = "apARCH",
                                         garchOrder = c(1, 1)),
                   distribution.model = "norm",
                   fixed.pars = list(ar3 = 0, ar4 = 0, ar5 = 0, ar6 = 0, mu = 0))
# estimando os parametros
m = ugarchfit(data = ret, spec = espec, solver = "hybrid",
              solver.control = list(tol=1e-20,delta=1e-20) )

# resumo do modelo
print(m)

# residuos padronizados do modelo
res_garch = residuals(m, standardize = TRUE)
acf(res_garch)
acf(res_garch^2)

# alguns graficos do ajuste
plot(m, which = 9)
plot(m, which = 3)
plot(m, which = 12)

# previsao da volatilidade em t + h
prev = ugarchforecast(m, n.ahead = 20)
plot(prev, which = 3)

# plot da previsao do retorno em t + h
plot(prev, which = 1)
#-----------------------------
```

