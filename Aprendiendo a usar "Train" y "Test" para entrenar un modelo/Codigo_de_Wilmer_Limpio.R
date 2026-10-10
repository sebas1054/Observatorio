library(readxl)
library(dplyr)
library(forecast)
library(ggplot2)
library(ggfortify)
library(lmtest)
library(stats)
library(urca)
library(uroot)
library(httpgd)

getwd()
setwd("..")
list.files()
setwd("Data")


# 1. IDENTIFICACIÓN DE LOS MODELOS ============
df <- read_excel("ipc 2003 2026 (Wilmer).xlsx")
df$Fecha <- as.Date(df$Fecha)
ipc <- ts(df$IPC, start = c(2003, 01), end = c(2025, 12), frequency = 12)
lipc <- log(ipc)
autoplot(ipc) 
autoplot(lipc) 
lipc_diff1 <- diff(lipc)

stl_ipc <- stl(lipc_diff1, s.window = "periodic")
autoplot(stl_inf) 

componentes <- stl_ipc$time.series
tendencia <- componentes[, "trend"]
estacionalidad <- componentes[ , "seasonal"]
residual <- componentes[ , "remainder"]

autoplot(residual)
checkresiduals(residual) # El test de Ljung-Box muestra autocorrelación 

lipc_desestacinalizado <- c(lipc_diff1 - estacionalidad)
lipc_desestacinalizado <- ts(lipc_desestacinalizado, start = c(2003, 2), frequency = 12)
autoplot(cbind(lipc_desestacinalizado, lipc_diff1))+
    ggtitle("Inflación original y desestacionalizada STL")+
    xlab("Año") + ylab("Valor")


# 2. ESTIMACIÓN DE PARÁMETROS ==============
# Para diff1
par(mfrow = c(2,1))
Acf(lipc_diff1, lag.max = 48, main = "ACF de inflación")    # 3 MA
Pacf(lipc_diff1, lag.max = 48, main = "PACF de inflación")  # 1 AR
par(mfrow= c(1,1))
summary(auto.arima(lipc_diff1))   #ARIMA(2,0,2)(2,1,0)[12]
summary(auto.arima(lipc_diff1, seasonal = F)) #ARIMA(2,0,0)
summary(hegy.test(lipc_diff1), deterministic = c(1,0,1), lag.method = "AIC")
summary(hegy.test(lipc_diff1), deterministic = c(1,0,0), lag.method = "AIC")
print(ch.test(lipc_diff1, type = "dummy", lag1 = T))    #H0 = patrón estacional estable
print(ch.test(lipc_diff1, type = "dummy", lag1 = F))
modelo_nivel <- Arima(lipc_diff1,
                    order = c(2, 0, 1),
                    seasonal = c(1, 0, 1),
                    xreg = seasonaldummy(lipc_diff1),
                    include.constant = T)
                    
# Ahora se hacen forecast usando train
train <- window(lipc_diff1, end = c(2018, 12))
test <- window(lipc_diff1, start = c(2019, 1))
xreg_train <- seasonaldummy(train)
xreg_test <- seasonaldummy(test)
auto.arima(train)   # ARIMA(1,0,0)(1,1,0)[12]
modelo0_dif_train <- Arima(train, order = c(1,1,1), seasonal = c(1,0,1), xreg = xreg_train, include.constant = F)
modelo0_nivel_train <- Arima(train, order = c(2,0,1), seasonal = c(1,0,1), xreg = xreg_train, include.constant = T)

# Ahora se hacen diferentes modelos para ver si se logra ajustar los indicadores (MASE, ACF, etc...)
# Modelo 1





# 3. DIAGNÓSTICO Y VERIFICACIÓN DEL MODELO =================
summary(modelo_nivel)
checkresiduals(modelo_nivel)

summary(modelo_dif_train0)
checkresiduals(modelo_dif_train0)
summary(modelo_nivel_train0)
checkresiduals(modelo_nivel_train0)






# 4. PRONÓSTICOS =====================
forecast0 <- forecast(modelo_nivel, h=24, xreg= seasonaldummy(lipc, 24), level = c(95))
summary(forecast0)
autoplot(forecast0)

# Pronósticos sobre el conjunto de prueba 0 (el que el profe Wilmer nos dió)
fcast0_dif <- forecast(modelo0_dif_train, xreg = xreg_test, h = length(test))
fcast0_nivel <- forecast(modelo0_nivel_train, xreg = xreg_test, h = length(test))
accuracy(fcast0_dif, test)
accuracy(fcast0_nivel, test)
