# -------------------------------
# 3. Data Preparation
# -------------------------------

# Exploración inicial
str(Datos)
summary(Datos)

# Conversión de "NULL" a NA
Datos <- Datos %>%
  mutate(
    tam_m2   = na_if(tam_m2, "NULL"),
    oos_days = na_if(oos_days, "NULL")
  )

# Conversión a numérico
Datos <- Datos %>%
  mutate(
    tam_m2   = as.numeric(tam_m2),
    oos_days = as.numeric(oos_days)
  )

# Eliminación de registros sin tamaño
afiliados_clean <- Datos %>%
  filter(!is.na(tam_m2))

# Reemplazo de NA en métricas
afiliados_clean <- afiliados_clean %>%
  mutate(
    order_frequency = ifelse(is.na(order_frequency), 0, order_frequency),
    oos_days        = ifelse(is.na(oos_days), 0, oos_days)
  )

# Eliminación de valores inconsistentes
afiliados_clean <- afiliados_clean %>%
  filter(
    order_frequency >= 0,
    oos_days >= 0
  )

# -------------------------------
# 4. Clustering Model
# -------------------------------

# Selección de variables
datos_clustering <- afiliados_clean[, 
  c("tam_m2", "engage", "order_frequency", "oos_days")
]

# Normalización
datos_scaled <- scale(datos_clustering)

# Método del codo
wcss <- numeric()

for (k in 1:10) {
  set.seed(123)
  modelo_k <- kmeans(datos_scaled, centers = k, nstart = 25)
  wcss[k] <- modelo_k$tot.withinss
}

plot(1:10, wcss,
     type = "b",
     pch = 19,
     xlab = "Número de clústeres",
     ylab = "WCSS",
     main = "Método del codo")

# Modelo final
set.seed(123)
modelo_kmeans <- kmeans(datos_scaled, centers = 4, nstart = 25)

# Silhouette
sil <- silhouette(modelo_kmeans$cluster, dist(datos_scaled))
mean(sil[, 3])

plot(sil, main = "Coeficiente de silueta")

# Asignación de clusters
afiliados_clean$cluster <- modelo_kmeans$cluster

# Distribución
table(afiliados_clean$cluster)

# -------------------------------
# 5. Cluster Profiling
# -------------------------------

perfil_clusters <- aggregate(
  afiliados_clean[, c(
    "tam_m2",
    "engage",
    "order_frequency",
    "oos_days"
  )],
  by = list(Cluster = afiliados_clean$cluster),
  FUN = mean,
  na.rm = TRUE
)

perfil_clusters

