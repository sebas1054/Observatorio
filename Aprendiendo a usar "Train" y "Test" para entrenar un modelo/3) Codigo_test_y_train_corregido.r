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
autoplot(stl_ipc) 

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

## INTENTAMOS AHORA USAR DUMMIES PARA LA PARTE DE LOS CONFINAMIENTOS, LO CUAL PUEDE SER UNA SOLUCIÓN MÁS REALISTA QUE CAMBIAR EL ORDEN DE LA PARTE ARMA. PARA ELLO, SE INTERPOLAN LOS MESES EN LOS QUE HUBO CONFINAMIENTOS (MARZO A AGOSTO)
# 1. Se recogen fechas para los índices lógicos 
fechas_diff <- df$Fecha[-1]
# 2. Se necesitan índices lógicos para las fechas de cuarentena
idx_cuarentena <- which(fechas_diff >= as.Date("2020-03-01") & fechas_diff <= as.Date("2020-08-31"))
idx_normales <- which(!(fechas_diff >= as.Date("2020-03-01") & fechas_diff <= as.Date("2020-08-31")))
# 3. Extraemos los valores como vector numérico para poder interpolar
valores_limpios <- as.numeric(lipc_diff1)
# 4. Interpolamos linealmente los meses de cuarentena (xout) usando los meses normales
valores_limpios[idx_cuarentena] <- approx(x = idx_normales, y = valores_limpios[idx_normales], xout = idx_cuarentena)$y
# 5. Reconstruimos la serie de tiempo (ts) ya sin el choque
lipc_diff1_limpio <- ts(valores_limpios, start = c(2003,2), frequency = 12)
# Graficamos para verificar visualmente que el pico del COVID desapareció
autoplot(cbind(Original = lipc_diff1, Interpolada = lipc_diff1_limpio))+
                    ggtitle("Original vs Interpolada")+
                    ylab("lipc_diff1")



# 2. ESTIMACIÓN DE PARÁMETROS ==============
# Para diff1
par(mfrow = c(2,1))
Acf(lipc_diff1, lag.max = 48, main = "ACF de inflación")    # 3 MA
Pacf(lipc_diff1, lag.max = 48, main = "PACF de inflación")  # 1 AR
par(mfrow= c(1,1))
#summary(auto.arima(lipc_diff1))   #ARIMA(2,0,2)(2,1,0)[12]
#summary(auto.arima(lipc_diff1, seasonal = F)) #ARIMA(2,0,0)
summary(hegy.test(lipc_diff1, deterministic = c(1,0,1), lag.method = "AIC"))
summary(hegy.test(lipc_diff1, deterministic = c(1,0,0), lag.method = "AIC"))
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
#auto.arima(train)   # ARIMA(1,0,0)(1,1,0)[12]
modelo0_dif_train <- Arima(train, order = c(1,1,1), seasonal = c(1,0,1), xreg = xreg_train, include.constant = F)
modelo0_nivel_train <- Arima(train, order = c(2,0,1), seasonal = c(1,0,1), xreg = xreg_train, include.constant = T)

# Desde acá se empiaza a cambiar el orden de los rezagos arma (arma(1,1) -> arma(2,2)...)

# Modelo 1 (el que sugiere el auto.arima)
modelo1_dif_train <- Arima(train, order = c(1, 1, 0), seasonal = c(1, 1, 0), xreg = xreg_train, include.constant = FALSE)
modelo1_nivel_train <- Arima(train, order = c(1, 0, 0), seasonal = c(1, 0, 0), xreg = xreg_train, include.constant = TRUE)

# Modelo 2
modelo2_dif_train <- Arima(train, order = c(2, 1, 2), seasonal = c(1, 0, 1), xreg = xreg_train, include.constant = FALSE)
modelo2_nivel_train <- Arima(train, order = c(2, 0, 2), seasonal = c(1, 0, 1), xreg = xreg_train, include.constant = TRUE)

# Modelo 3
modelo3_dif_train <- Arima(train, order = c(3, 1, 3), seasonal = c(1, 0, 1), xreg = xreg_train, include.constant = FALSE)
modelo3_nivel_train <- Arima(train, order = c(3, 0, 2), seasonal = c(1, 0, 1), xreg = xreg_train, include.constant = TRUE)

# Modelo 4
modelo4_dif_train <- Arima(train, order = c(4, 1, 4), seasonal = c(1, 0, 1), xreg = xreg_train, include.constant = FALSE)
modelo4_nivel_train <- Arima(train, order = c(4, 0, 4), seasonal = c(1, 0, 1), xreg = xreg_train, include.constant = TRUE)

# DESDE ACÁ SE HACEN LOS MODELOS INTERPOLANDO LOS MESES DE CUARENTENA
test_interpolado <- window(lipc_diff1_limpio, start = c(2019, 1)) # El train es el mismo
# Se usan los mismos modelos, porque el train no cambió (los cambios son en el 2020, pero el train va hasta 2018), lo que cambió fue el test






# 3. DIAGNÓSTICO Y VERIFICACIÓN DEL MODELO =====================
summary(modelo_nivel)
checkresiduals(modelo_nivel)

# Diágnóstico del primer modelo de entremaniento (el que nos dío el profe)
summary(modelo0_dif_train)
checkresiduals(modelo0_dif_train)
summary(modelo0_nivel_train)
checkresiduals(modelo0_nivel_train)

# Desde acá se empiaza a cambiar el orden de los rezagos arma (arma(1,1) -> arma(2,2)...)
# Modelo 1
checkresiduals(modelo1_dif_train)   # No sirve: presenta autocorrelación
checkresiduals(modelo1_nivel_train)

# Modelo 2
checkresiduals(modelo2_dif_train)
checkresiduals(modelo2_nivel_train)

# Modelo 3
checkresiduals(modelo3_dif_train)
checkresiduals(modelo3_nivel_train)

# Modelo 4
checkresiduals(modelo4_dif_train)
checkresiduals(modelo4_nivel_train)






# 4. PRONÓSTICOS ==========================
forecast0 <- forecast(modelo_nivel, h=24, xreg= seasonaldummy(lipc, 24), level = c(95))
summary(forecast0)
autoplot(forecast0)

# Pronósticos sobre el conjunto de prueba 0 (el que el profe Wilmer nos dió)
fcast0_dif <- forecast(modelo0_dif_train, xreg = xreg_test, h = length(test))
fcast0_nivel <- forecast(modelo0_nivel_train, xreg = xreg_test, h = length(test))
accuracy(fcast0_dif, test)  # 1.1846852 / 0.73496455
accuracy(fcast0_nivel, test)    # 1.1437882 / 0.7349868857

# Desde acá se empiaza a cambiar el orden de los rezagos arma (arma(1,1) -> arma(2,2)...)
# Los únicos parámetros que se van a tener en cuenta son MASE / ACF1 
# Modelo 1
fcast1_dif <- forecast(modelo1_dif_train, xreg= xreg_test, h= length(test))
fcast1_nivel <- forecast(modelo1_nivel_train, xreg = xreg_test, h = length(test))
accuracy(fcast1_dif, test)      # 2.1821222 / 0.73950243
accuracy(fcast1_nivel, test)    # 1.1429612 / 0.7337423 

# Modelo 2
fcast2_dif <- forecast(modelo2_dif_train, xreg= xreg_test, h= length(test))
fcast2_nivel <- forecast(modelo2_nivel_train, xreg = xreg_test, h = length(test))
accuracy(fcast2_dif, test)      # 1.2244868 / 0.735205135
accuracy(fcast2_nivel, test)    # 1.1438763 / 0.73455664 

# Modelo 3
fcast3_dif <- forecast(modelo3_dif_train, xreg= xreg_test, h= length(test))
fcast3_nivel <- forecast(modelo3_nivel_train, xreg = xreg_test, h = length(test))
accuracy(fcast3_dif, test)      # 1.1914320 / 0.72910157
accuracy(fcast3_nivel, test)    # 1.1390424 / 0.73500994 

# Modelo 4
fcast4_dif <- forecast(modelo4_dif_train, xreg= xreg_test, h= length(test))
fcast4_nivel <- forecast(modelo4_nivel_train, xreg = xreg_test, h = length(test))
accuracy(fcast4_dif, test)      # 1.1325950 / 0.72348265
accuracy(fcast4_nivel, test)    # 1.1325950 / 0.72348265 ***

# DESDE ACÁ SE HACEN LOS MODELOS INTERPOLANDO LOS MESES DE CUARENTENA
# Modelo 2.0
accuracy(fcast0_dif, test_interpolado)      # 1.1632729 / 0.70787186
accuracy(fcast0_nivel, test_interpolado)    # 1.1042602 / 0.7075322206 

# Modelo 2.1
accuracy(fcast1_dif, test_interpolado)      # 2.2307343 / 0.71493622
accuracy(fcast1_nivel, test_interpolado)    # 1.1041297 / 0.7065590 

# Modelo 2.2
accuracy(fcast2_dif, test_interpolado)      # 1.1056223 / 0.70729910
accuracy(fcast2_nivel, test_interpolado)    # 1.1056223 / 0.70729910 

# Modelo 2.3
accuracy(fcast3_dif, test_interpolado)      # 1.1716909 / 0.70168834
accuracy(fcast3_nivel, test_interpolado)    # 1.0986222 / 0.70724293  ***

# Modelo 2.4
accuracy(fcast4_dif, test_interpolado)      # 1.1003588 / 0.69569145 
accuracy(fcast4_nivel, test_interpolado)    # 1.1011038 / 0.69581353 


