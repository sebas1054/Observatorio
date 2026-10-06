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

# 6. Graficar original vs desestacionalizada 
autoplot(cbind(diff(df$lipc), df$inf_stl[-1]))+
ggtitle("Inflación original y desestacionalizada STL")
