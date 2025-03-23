# Cargar paquetes necesarios
library(tseries) # Para la prueba de Dickey-Fuller
library(ggplot2) # Para visualización
library(forecast) # Para análisis de series temporales

# 1. Cargar el dataset y explorar su estructura
data("AirPassengers")
print(class(AirPassengers))   # Verificar tipo de datos
print(summary(AirPassengers)) # Resumen estadístico
print(start(AirPassengers))   # Inicio de la serie
print(end(AirPassengers))     # Fin de la serie
print(frequency(AirPassengers)) # Frecuencia: 12 (mensual)

# 2. Exploración inicial
plot(AirPassengers, main="Número de Pasajeros de Aerolíneas (1949-1960)",
     ylab="Pasajeros (en miles)", xlab="Año", col="blue")

# Estadísticas descriptivas
mean_value <- mean(AirPassengers)
sd_value <- sd(AirPassengers)
cat("Media:", mean_value, "\n")
cat("Desviación estándar:", sd_value, "\n")

# 3. Descomposición de la serie temporal
decomposed <- decompose(AirPassengers, type="multiplicative")
plot(decomposed)

# 4. Análisis de estacionariedad
acf(AirPassengers, main="Autocorrelación de AirPassengers")
pacf(AirPassengers, main="Autocorrelación Parcial de AirPassengers")

# Prueba de Dickey-Fuller para verificar estacionariedad
adf_test <- adf.test(AirPassengers)
print(adf_test)

# Si la serie no es estacionaria, aplicamos diferenciación
diff_series <- diff(AirPassengers, differences=1)
plot(diff_series, main="Serie Diferenciada", col="red")

# Volver a probar la estacionariedad tras diferenciación
adf_test_diff <- adf.test(diff_series, na.action=na.omit)
print(adf_test_diff)

# 5. Detección de valores atípicos
boxplot(AirPassengers, main="Boxplot de AirPassengers",
        ylab="Pasajeros (en miles)", col="orange")

# 6. Interpretación de resultados (Se incluirá en el documento final .Rmd o comentarios en el código)