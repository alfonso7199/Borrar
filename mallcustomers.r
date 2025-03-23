# Cargar librerías necesarias
library(tidyverse)
library(cluster)
library(factoextra)
library(ggplot2)

# 1. Carga del dataset
data <- read.csv("Mall_Customers.csv")
head(data)

# 2. Exploración y limpieza de datos
str(data)
dim(data)
summary(data)

# Renombrar columnas para facilidad
data <- data %>% rename(ID = CustomerID, Genre = Gender, Income = Annual.Income..k.., Score = Spending.Score..1.100.)

# Codificar 'Genre' como variable numérica
data$Genre <- ifelse(data$Genre == "Male", 1, 0)

# Normalizar variables numéricas clave
data_scaled <- data %>% mutate(across(c(Income, Score), scale))

# 3. Exploración de variables
hist(data$Income, main="Distribución del Ingreso Anual", col="skyblue")
hist(data$Score, main="Distribución del Spending Score", col="orange")

# 4. Entrenamiento de modelos de clustering
## K-Means: Determinar el número óptimo de clusters
set.seed(42)
wss <- sapply(2:6, function(k) {kmeans(data_scaled[, c("Income", "Score")], centers=k, nstart=25)$tot.withinss})
plot(2:6, wss, type="b", pch=19, col="blue", main="Método del Codo", xlab="Número de Clusters", ylab="Suma de Cuadrados")

# Entrenar modelo K-Means
kmeans_model <- kmeans(data_scaled[, c("Income", "Score")], centers=5, nstart=25)
data$Cluster_KMeans <- as.factor(kmeans_model$cluster)

## Clustering Jerárquico
# Matriz de distancias
dist_matrix <- dist(data_scaled[, c("Income", "Score")], method="euclidean")
# Generar dendrograma
hc <- hclust(dist_matrix, method="ward.D")
plot(hc, main="Dendrograma", xlab="Clientes", sub="")
rect.hclust(hc, k=5, border="red")
data$Cluster_HC <- as.factor(cutree(hc, k=5))

# 5. Evaluación de modelos
sil_kmeans <- silhouette(kmeans_model$cluster, dist_matrix)
sil_hc <- silhouette(cutree(hc, k=5), dist_matrix)
mean(sil_kmeans[, 3]) # Promedio de silueta K-Means
mean(sil_hc[, 3]) # Promedio de silueta Clustering Jerárquico

# 6. Análisis descriptivo de segmentos
aggregate(data[, c("Income", "Score")], by=list(Cluster=data$Cluster_KMeans), mean)
aggregate(data[, c("Income", "Score")], by=list(Cluster=data$Cluster_HC), mean)

# 7. Visualización
p1 <- ggplot(data, aes(x=Income, y=Score, color=Cluster_KMeans)) +
  geom_point() + theme_minimal() + ggtitle("Clusters K-Means")
p2 <- ggplot(data, aes(x=Income, y=Score, color=Cluster_HC)) +
  geom_point() + theme_minimal() + ggtitle("Clusters Jerárquicos")
print(p1)
print(p2)

# 8. Guardar resultados para GitHub
write.csv(data, "Mall_Customers_Clusters.csv", row.names=FALSE)
