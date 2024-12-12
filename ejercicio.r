# 1. Cargar las librerías y los datos
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")

library(dplyr)
library(tidyr)

# Cargar el dataset mtcars y convertirlo en un dataframe
data(mtcars)
df <- as.data.frame(mtcars)

# Verificar el dataframe inicial
print("Dataframe inicial:")
print(df)

# 2. Selección de columnas y filtrado de filas
df_filtered <- df %>%
  select(mpg, cyl, hp, gear) %>%
  filter(cyl > 4)

print("Dataframe después de selección y filtrado:")
print(df_filtered)

# 3. Ordenación y renombrado de columnas
df_sorted <- df_filtered %>%
  arrange(desc(hp)) %>%
  rename(consumo = mpg, potencia = hp)

print("Dataframe después de ordenación y renombrado:")
print(df_sorted)

# 4. Creación de nuevas columnas y agregación de datos
df_with_eficiencia <- df_sorted %>%
  mutate(eficiencia = consumo / potencia)

df_aggregated <- df_with_eficiencia %>%
  group_by(cyl) %>%
  summarise(consumo_medio = mean(consumo, na.rm = TRUE),
            potencia_maxima = max(potencia, na.rm = TRUE))

print("Dataframe después de agregar la columna eficiencia:")
print(df_with_eficiencia)

print("Dataframe agrupado por cilindros:")
print(df_aggregated)

# 5. Creación del segundo dataframe y unión de dataframes
df_gear <- data.frame(
  gear = c(3, 4, 5),
  tipo_transmision = c("Manual", "Automática", "Semiautomática")
)

df_joined <- df_with_eficiencia %>%
  left_join(df_gear, by = "gear")

print("Dataframe después del left_join:")
print(df_joined)

# 6. Transformación de formatos
# Transformar a formato largo
df_long <- df_joined %>%
  pivot_longer(cols = c(consumo, potencia, eficiencia),
               names_to = "medida",
               values_to = "valor")

print("Dataframe en formato largo:")
print(df_long)

# Identificar combinaciones duplicadas
df_long_grouped <- df_long %>%
  group_by(cyl, gear, tipo_transmision, medida) %>%
  summarise(valor_medio = mean(valor, na.rm = TRUE), .groups = "drop")

print("Dataframe con valores agrupados para eliminar duplicados:")
print(df_long_grouped)

# Transformar de nuevo a formato ancho
df_wide <- df_long_grouped %>%
  pivot_wider(names_from = medida, values_from = valor_medio)

print("Dataframe en formato ancho final:")
print(df_wide)

# 7. Verificación
print("Verificación final de todos los pasos completados:")
print(list(
  Selección_y_filtrado = df_filtered,
  Ordenación_y_renombrado = df_sorted,
  Con_eficiencia = df_with_eficiencia,
  Agrupado = df_aggregated,
  Unión = df_joined,
  Largo = df_long,
  Agrupado_para_eliminar_duplicados = df_long_grouped,
  Ancho = df_wide
))
