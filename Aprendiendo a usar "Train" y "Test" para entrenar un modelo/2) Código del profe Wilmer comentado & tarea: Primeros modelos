# El profesor no  solo busca hacer un ARIMA, sino que también busca descubrir las PROPIEDADES ESTADÍSTICAS que tiene la serie
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

df <- read_excel("C:/Users/johan/OneDrive/Documents/Analisis macroeconomico/ipc 2003 2026.xlsx")
 # SE ACOMODA LA BASE DE DATOS Y SE GRAFICA ----------
getwd()
setwd("..")
list.files()
setwd("Data")

df <- read_excel("ipc 2003 2026 (Wilmer).xlsx")


df$Fecha <- as.Date(df$Fecha)
# 2. Filtar hasta diciembre de 2025 <- elimina 2026 porque después va a probar los pronósticos con este año
df <- df[df$Fecha <= as.Date("2025-12-31"),]

# 3. Declarar la serie de tiempo IPC sobre el df filtrado
df$IPC <- ts(df$IPC, 
             start = c(2003, 1),
            frequency = 12)

# 4. Crear logaritmo natural del IPC
df$lipc <- log(df$IPC)

#5. Serie de tiempo del los IPC (también sobre el df filtrado)
df$lipc <- ts(df$lipc,
            start = c(2003, 1),
            frequency = 12)

# 6. Graficar 
autoplot(df$IPC) + theme_minimal()
autoplot(df$lipc) + theme_minimal()

# DESCOMPOSICIÓN DE LA SERIE ----

# 1. Descomposición STL
stl_inf <- stl(diff(df$lipc), s.window = "periodic")
#stl_inf <- stl(df$lipc, s.window = "periodic")
# 2. Graficar la descomsición
autoplot(stl_inf)+theme_minimal()
# STL = Seasonal-Trend Decomposition using Loess
# Acá lo que se busca es descubrir si la inflación tiene una parte estacioanal
# s.window lo que hace es decirle al filtro STL que la estacionalidad es fija (periódica), o sea es determinística. La otra opción sería colocar un número impar, esto le dice que la estacionalidad cambia cada ciertos periódos 
# Para probar si la estacionalidad es fija ("periodic") o cambia con el tiempo se debe aplicar la prueba Canova-Hansen (CH) (esto se hace más adelante)
# Lo que hace el filtro STL es que separa los componentes, es una aproximación a lo que debemos hacer. Entonces vemos que la inflación tiene un componente estacional y que DEBEMOS DETERMINAR SI ESE COMPONENTE ES ESTACIONARIO Y SI ES ALEATORIO O DETERMINÍSTICO. A su vez, se ve que si eliminamos los componentes tendenciales y estacionales de los precios, nos queda una serie que se ve cíclica, es decir, ¿estacionaria?

# 3. Extraer componentes
# Extraer compoonentes de la descomposición STL 
componentes <- stl_inf$time.series

# Ajustar longitudes: Agregar NA al inicio
df$tendencia    <- c(NA, componentes[, "trend"])
df$estacionalidad <- c(NA, componentes[, "seasonal"])
df$residuos_stl <- c(NA, componentes[, "remainder"])

# 4. Test de Ljung Box sobre los residuos
Box.test(df$residuos_stl, lag=24, type = "Ljung") #Correlación del componente residual
### Si los resultados después de quitarles al estacionalidad están correlacionados, puedo aplicarles el ARIMA, si no no

# p-value = 4.659e-09, se rechaza H0, hay correlación de los residuos
# Que haya correlación es bueno, significa que podemos plantear los modelos ARIMA, ya que, la razón de ser de los modelos ARIMA es atrapar y modelar matemáticamente esa "memoria" o autocorrelación que tienen los datos. Si al hacer la descomposición STL los residuos hubieran dado ruido blanco (sin correlación), significaría que la simple extracción de la tendencia y la estacionalidad ya explicó todo el comportamiento de la serie. En ese escenario, no habría ninguna dinámica compleja que modelar y no se necesitaría usar un ARIMA

# 5. Serie desestacionalizada 
df$inf_stl <- c(NA, diff(df$lipc) - df$estacionalidad[-1])


# Graficar original vs desestacionalizada
autoplot(cbind(diff(df$lipc), df$inf_stl[-1]))+
    ggtitle("Inflación original y desestacionalizada STL")+
    xlab("Año") + ylab("Valor")+
    theme_minimal()

acf(diff(df$lipc), lag.max = 48, main = "ACF de inflación")
pacf(diff(df$lipc), lag.max= 48, main = "PACF de inflación")

#endregion
# HEGY test para raíces unitarias estacionales  <- nos permite decidir si necesitamos una diferenciación estacional
# HEGY, también nos dice en qué frecuencias estacionales podría existir una raíz unitaria (mensual, anual, trimestral... )
summary(hegy.test(diff(df$lipc), deterministic = c(1, 0, 1), lag.method = "AIC"))
summary(hegy.test(diff(df$lipc), deterministic = c(1, 0, 0), lag.method = "AIC"))
# Componentes de la sintaxis de hegy.test:
# deterministic: Le indica a HEGY qué componentes determinísticos debe incluir en la regresión auxiliar 
# c(constante, tendencia, dummies estacionales)
# lag.method="AIC": Determina cuántos rezagos debo incluir utilizando el criterio de información Akaike 


# Canova-Hansen (CH) test para estabilidad estacional 
print(ch.test(diff(df$lipc), type= "dummy", lag1= T))  # Estamos queriendo ver si la estacionalidad de la inflación mensual es estable a través del tiempo 
# lag1 = T: lo que está haciendo es que está incluyendo un rezago de la variable dependiente en la regresión auxiliar, esto es para controlar por autocorrelación. Si no se controla por autocorrelación, el test puede dar resultados falsos positivos.
print(ch.test(diff(df$lipc), type= "dummy", lag1 = F))


# Ajustar el modelo ARIMA con SAR(1) y AR(1), sin diferenciación estacional
modelo_nivel <- Arima(diff(df$lipc),
                    order = c(2, 0, 1),
                    seasonal = c(1, 0, 1),
                    xreg = seasonaldummy(diff(df$lipc)),
                    include.constant = T)

summary(modelo_nivel)   
checkresiduals(modelo_nivel) # Esto es para ver si los residuos del modelo ARIMA son ruido blanco, si no lo son, significa que el modelo no está capturando toda la dinámica de la serie y hay que buscar otro modelo ARIMA
plot(modelo_nivel)
coeftest(modelo_nivel)

autoplot(forecast(modelo_nivel, h=24, xreg = seasonaldummy(df$lipc, 24), level =c(95)))


# Ahora se hace el HEGY test sobre la serie diferenciada dos veces

summary(hegy.test(diff(df$lipc, 2), deterministic = c(1, 0, 1), lag.method = "AIC"))
summary(hegy.test(diff(df$lipc, 2), deterministic = c(0, 0, 1), lag.method = "AIC"))

# Canova-Hansen (CH) test para estabilidad estacional
print(ch.test(diff(df$lipc, 2), type= "dummy", lag1= T))  # Estamos queriendo ver si la estacionalidad de la inflación mensual es estable a través del tiempo

autoplot(diff(df$lipc, 2))

acf(diff(df$lipc, 2), lag.max = 48, main="ACF de Inflación")
pacf(diff(df$lipc, 2), lag.max = 48, main="PACF de Inflación")

# Ajustar el modelo ARIMA con SAR(1) y AR(1), sin diferenciación estacional
modelo_dif <- Arima(diff(df$lipc),
                    order = c(1, 1, 1),
                    seasonal = c(1, 0, 1),
                    xreg = seasonaldummy(diff(df$lipc)),
                    include.constant = F)
summary(modelo_dif)
checkresiduals(modelo_dif)
plot(modelo_dif)
coeftest(modelo_dif)

autoplot(forecast(modelo_dif, h=24, xreg = seasonaldummy(df$lipc, 24), level =c(95)))






# =================== ENTRENAMIENTO DEL FORECAST =========================
library(forecast)

# supongamos que tu serie lipc va desde 2003-01 a 2025-12
# Definimos subconjunto de entrenamiento (ejemplo: hasta 2020-12)

train <- window(diff(df$lipc), end = c(2018, 12))
test <- window(diff(df$lipc), start = c(2019, 1))

# Dummies estacionales recortadas al mismo tiempo
xreg_train <- seasonaldummy(train)
xreg_test <- seasonaldummy(test)

# Re-estimar modelo_dif sobre el subconjunto de entrenamiento 
modelo_dif_train <- Arima(train,
                          order = c(1, 1, 1),
                          seasonal = c(1, 0, 1),
                          xreg = xreg_train,
                          include.constant = F)

# Re-estimar modelo_nivel sobre el subconjunto de entrenamiento
modelo_nivel_train <- Arima(train,
                          order = c(2, 0, 1),
                          seasonal = c(1, 0, 1),
                          xreg = xreg_train,
                          include.constant = T)

# Pronósticos sobre el conjunto de prueba
fcast_dif <- forecast(modelo_dif_train, xreg = xreg_test, h = length(test))
fcast_nivel <- forecast(modelo_nivel_train, xreg = xreg_test, h = length(test))




# PRIMERA SOLUCIÓN PARA MEJORAR EL AJUSTE DEL MODELO (MÁS REZAGOS ARMA) -----
# 1. Poner más rezagos en la parte ARMA

modelo2_dif_train <- Arima(train,
                            order = c(2, 1, 2),
                            seasonal = c(1, 0, 1),
                            xreg = xreg_train,
                            include.constant = FALSE
)
modelo2_nivel_train <- Arima(train,
                            order = c(2, 0, 2),
                            seasonal = c(1, 0, 1),
                            xreg = xreg_train,
                            include.constant = TRUE
)
fcast2_dif <- forecast(modelo2_dif_train, xreg= xreg_test, h= length(test))
fcast2_nivel <- forecast(modelo2_nivel_train, xreg = xreg_test, h = length(test))

modelo3_dif_train <- Arima(train,
                            order = c(3, 1, 3),
                            seasonal = c(1, 0, 1),
                            xreg = xreg_train,
                            include.constant = FALSE
)
modelo3_nivel_train <- Arima(train,
                            order = c(3, 0, 2),
                            seasonal = c(1, 0, 1),
                            xreg = xreg_train,
                            include.constant = TRUE
)
fcast3_dif <- forecast(modelo3_dif_train, xreg= xreg_test, h= length(test))
fcast3_nivel <- forecast(modelo3_nivel_train, xreg = xreg_test, h = length(test))

modelo4_dif_train <- Arima(train,
                            order = c(4, 1, 4),
                            seasonal = c(1, 0, 1),
                            xreg = xreg_train,
                            include.constant = FALSE
)
modelo4_nivel_train <- Arima(train,
                            order = c(4, 0, 4),
                            seasonal = c(1, 0, 1),
                            xreg = xreg_train,
                            include.constant = TRUE
)
fcast4_dif <- forecast(modelo4_dif_train, xreg= xreg_test, h= length(test))
fcast4_nivel <- forecast(modelo4_nivel_train, xreg = xreg_test, h = length(test))

### SE EVALUA EL CRITERIO MASE Y ACF1 (compara con un ramdom walk y mira la autocorrelación de los residuos)
accuracy(fcast_dif, test)       # 1.1846852 / 0.73496455
accuracy(fcast_nivel, test)     # 1.1437882 / 0.7349868857 *

accuracy(fcast2_dif, test)      # 1.2244868 / 0.735205135
accuracy(fcast2_nivel, test)    # 1.1438763 / 0.73455664 *

accuracy(fcast3_dif, test)      # 1.1914320 / 0.72910157
accuracy(fcast3_nivel, test)    # 1.1390424 / 0.73500994 **

accuracy(fcast4_dif, test)      # 1.1325950 / 0.72348265
accuracy(fcast4_nivel, test)    # 1.1325950 / 0.72348265 **


# SEGUNDA SOLUCIÓN PARA MEJORAR EL AJUSTE DEL MODELO (DUMMIES ESTACIONALES)
#   La cuarentena en Colombia empezó el 25 de marzo de 2020 y se extendió hasta el 31 de agosto de 2020 


# Para hacer el proceso de agregarle las dummies estacionales, se aplica una interpolación para tener una idea de cuánto fue el efecto de la cuarente (es efecto se le descuenta para hallar la serie limpia): 
inflacion <- df$inf_stl  # <- Esta es la aproximación de la inflación desestacionalizada
mes <- format(df$Fecha[-1], "%Y-%m")
df$dummy_cuarentena <- as.integer(
    mes >= "2020-03" & mes <="2020-08"
)
# Comprobamos que las dummies que creamos cubran los meses esperados 
data.frame(Fecha = df$Fecha, dummy = df$dummy_cuarentena) |> subset(dummy == 1)

# Índice de los meses afectados
idx_cuarentena <- which(df$dummy_cuarentena == 1)
idx_normales <- which(df$dummy_cuarentena == 0)

inflacion[idx_cuarentena] <- approx(
    x = idx_normales,
    y = inflacion[idx_normales],
    xout = idx_cuarentena
)$y


# Devolviendo la estructural temporal
inflacion <- ts(inflacion, start = start(df$inf_stl), frequency = frequency(inflacion))

plot(inflacion)

df$d.cuarentena <- as.integer(df$Fecha >="2020-03-01" & df$Fecha <= "2020-08-01")
idx_cuarentena <- which(df$d.cuarentena == 1)
idx_normales <- which(df$d.cuarentena == 0)

df$infla <- df$inf_stl
df$infla[idx_cuarentena] <- approx(x=idx_normales, y=df$infla[idx_normales], xout = idx_cuarentena)$y


y <- diff(df$lipc)

infla_ts <- ts(
  as.numeric(df$infla[-1]),
  start = start(y),
  frequency = frequency(y)
)

train <- window(infla_ts, end = c(2018, 12))
test  <- window(infla_ts, start = c(2019, 1))

xreg_train <- seasonaldummy(train)
xreg_test  <- seasonaldummy(test)


# Re-estimar modelo_dif sobre el subconjunto de entrenamiento 
modelo_dif_train <- Arima(train,
                          order = c(1, 1, 1),
                          seasonal = c(1, 0, 1),
                          xreg = xreg_train,
                          include.constant = F)

# Re-estimar modelo_nivel sobre el subconjunto de entrenamiento
modelo_nivel_train <- Arima(train,
                          order = c(4, 0, 3),
                          seasonal = c(1, 0, 1),
                          xreg = xreg_train,
                          include.constant = T)

# Pronósticos sobre el conjunto de prueba
fcast_dif <- forecast(modelo_dif_train, xreg = xreg_test, h = length(test))
fcast_nivel <- forecast(modelo_nivel_train, xreg = xreg_test, h = length(test))

accuracy(fcast_dif, test)
accuracy(fcast_nivel, test)

