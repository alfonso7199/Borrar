# Cargar paquetes necesarios
library(tseries)   # Para la prueba de Dickey-Fuller
library(ggplot2)   # Para visualización
library(forecast)  # Para análisis de series temporales

# 1. Cargar el dataset y explorar su estructura
data("nottem")
print(class(nottem))   # Verificar tipo de datos (ts)
print(summary(nottem)) # Resumen estadístico

# Graficar la serie temporal
plot(nottem, main = "Temperaturas Mensuales en Nottingham (1920-1939)",
     xlab = "Año", ylab = "Temperatura (°F)", col = "blue")

# 2. Descomposición de la serie temporal
decomposed <- decompose(nottem, type="additive")
plot(decomposed)

# 3. Análisis de estacionariedad
acf(nottem, main="Autocorrelación de nottem")
pacf(nottem, main="Autocorrelación Parcial de nottem")

# Prueba de Dickey-Fuller para verificar estacionariedad
adf_test <- adf.test(nottem)
print(adf_test)

# 4. Transformación de la serie si no es estacionaria
diff_series <- diff(nottem, differences=1)
plot(diff_series, main="Serie Diferenciada", col="red")

# Volver a probar la estacionariedad tras la diferenciación
adf_test_diff <- adf.test(diff_series, na.action=na.omit)
print(adf_test_diff)

# 5. Detección de valores atípicos
boxplot(nottem, main="Boxplot de Temperaturas en Nottingham",
        ylab="Temperatura (°F)", col="orange")

# Resumen e interpretación de resultados (incluir en .Rmd si se usa formato Markdown)
