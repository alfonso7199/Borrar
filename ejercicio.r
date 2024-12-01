# Función para leer el archivo y devolver un vector de números
leer_numeros <- function(nombre_archivo) {
  if (!file.exists(nombre_archivo)) {
    stop("El archivo no existe. Por favor, verifica el nombre y la ubicación.")
  }
  numeros <- as.integer(readLines(nombre_archivo))
  return(numeros)
}

# Función para calcular los estadísticos y manejar alta variabilidad
calcular_estadisticos <- function(numeros) {
  media <- mean(numeros)
  mediana <- median(numeros)
  desviacion <- sd(numeros)
  
  # Verificar si hay alta variabilidad
  if (desviacion > 10) {
    cat("Alta variabilidad detectada: la desviación estándar es mayor a 10.\n")
  }
  
  return(list(media = media, mediana = mediana, desviacion = desviacion))
}

# Función para calcular el cuadrado de cada número usando sapply
calcular_cuadrados <- function(numeros) {
  cuadrados <- sapply(numeros, function(x) x^2)
  return(cuadrados)
}

# Función para guardar los resultados en un archivo
guardar_resultados <- function(nombre_archivo, estadisticos, cuadrados) {
  salida <- file(nombre_archivo, "w")
  
  # Escribir los estadísticos
  cat("Resultados estadísticos:\n", file = salida)
  cat("Media: ", estadisticos$media, "\n", file = salida)
  cat("Mediana: ", estadisticos$mediana, "\n", file = salida)
  cat("Desviación estándar: ", estadisticos$desviacion, "\n\n", file = salida)
  
  # Escribir los cuadrados de los números
  cat("Cuadrados de los números:\n", file = salida)
  cat(paste(cuadrados, collapse = ", "), "\n", file = salida)
  
  close(salida)
}

# Función principal
procesar_numeros <- function(nombre_entrada, nombre_salida) {
  # Leer los números desde el archivo
  numeros <- leer_numeros(nombre_entrada)
  
  # Calcular los estadísticos
  estadisticos <- calcular_estadisticos(numeros)
  
  # Calcular los cuadrados de los números
  cuadrados <- calcular_cuadrados(numeros)
  
  # Guardar los resultados en el archivo de salida
  guardar_resultados(nombre_salida, estadisticos, cuadrados)
  
  cat("Procesamiento completado. Resultados guardados en", nombre_salida, "\n")
}

# Ejecutar el script con los nombres de archivo adecuados
procesar_numeros("numeros.txt", "resultados.txt")
