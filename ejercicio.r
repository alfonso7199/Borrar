# Paso 1: Configuración inicial

# Definir los vectores
energia <- c(rep("Renovable", 10), rep("No Renovable", 10))
consumo <- c(15, 20, 18, 22, NA, 25, 19, 21, NA, 30,
             50, 45, 60, 55, 50, 53, 47, 48, 52, 49, 51)
costo_kwh <- c(rep(0.10, 10), rep(0.15, 10))  # Costo por kWh (Renovable: 0.10, No renovable: 0.15)

# Paso 2: Limpieza de datos
# Reemplazar los valores NA por la mediana del consumo de cada tipo de energía

# Función para reemplazar NA por la mediana dentro de cada tipo de energía
replace_na_with_median <- function(consumo, energia) {
  for (tipo in unique(energia)) {
    median_val <- median(consumo[energia == tipo], na.rm = TRUE)
    consumo[energia == tipo & is.na(consumo)] <- median_val
  }
  return(consumo)
}

# Aplicar la limpieza
consumo <- replace_na_with_median(consumo, energia)

# Paso 3: Crear el dataframe
df_consumo <- data.frame(
  energia = energia,
  consumo_diario = consumo,
  costo_kwh = costo_kwh
)

# Paso 4: Cálculos

# Agregar una columna costo_total (consumo * costo por kWh)
df_consumo$costo_total <- df_consumo$consumo_diario * df_consumo$costo_kwh

# Calcular el total de consumo y costo total por tipo de energía
total_consumo <- tapply(df_consumo$consumo_diario, df_consumo$energia, sum)
total_costo <- tapply(df_consumo$costo_total, df_consumo$energia, sum)

# Calcular la media del consumo diario por tipo de energía
media_consumo <- tapply(df_consumo$consumo_diario, df_consumo$energia, mean)

# Agregar una columna ganancia (costo_total * 1.1 para simular un aumento del 10%)
df_consumo$ganancia <- df_consumo$costo_total * 1.1

# Paso 5: Resumen

# Crear una lista llamada resumen_energia
resumen_energia <- list(
  # Ordenar el dataframe por costo_total en orden descendente
  dataframe_ordenado = df_consumo[order(-df_consumo$costo_total), ],
  
  # Total de consumo energético por tipo de energía
  total_consumo = total_consumo,
  
  # Total de costo por tipo de energía
  total_costo = total_costo,
  
  # Tres filas con el mayor costo_total
  top_3_costos = head(df_consumo[order(-df_consumo$costo_total), ], 3)
)

# Mostrar la lista resumen_energia
print(resumen_energia)
