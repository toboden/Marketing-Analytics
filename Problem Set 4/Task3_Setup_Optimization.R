library(dplyr)      # Für Datenmanipulation (ähnlich pandas)
library(class)      # Für KNN (Basis-Implementierung) oder 'FNN' für schnellere Algorithmen
library(cluster)    # Für Clustering (kmeans, hierarchical)

# 1. Daten laden
# check.names = FALSE ist wichtig, damit "Front Wing" nicht zu "Front.Wing" wird!
data <- read.csv('data/simulator_data.csv', check.names = FALSE)

## Belgium Circuit
track_params <- list(
  'Cornering' = 95,
  'Inclines' = 98,
  'Camber' = 38,
  'Grip' = 9,
  'Altitude' = 20,
  'Roughness' = 78,
  'Width' = 15,
  'Lap Distance' = 4.9  # in km
)

# No weather data for Belgium Circuit available
# (Definieren wir trotzdem, falls wir es später brauchen, wie im Python Skript)
weather_params <- list(
  'Temperature' = 31,
  'Humidity' = 96,
  'Wind (Avg. Speed)' = 10,
  'Wind (Gusts)' = 20,
  'Air Density' = 97,
  'Air Pressure' = 45
)

# 3. Parameter kombinieren
# Da wir keine Wetterdaten für Belgien haben, nehmen wir nur die Track Params
fixed_params <- track_params

# Falls du später beide kombinieren willst (wie {**x, **y} in Python):
# fixed_params <- c(track_params, weather_params)

# 4. Features festlegen (Python List -> R Vector)
decision_features <- c(
  'Front Wing',
  'Rear Wing',
  'Brake Balance',
  'Suspension',
  'Engine',
  'Differential'
)


# 1. Lineares Modell fitten
# Formel: Zielvariable ~ Feature
linear_model <- lm(`Lap Time` ~ `Lap Distance`, data = data)

# 2. Vorhersage auf den gleichen Daten
preds <- predict(linear_model, newdata = data)

data$lap_time_adjusted <- data$`Lap Time` - preds

data$`Lap Time` <- NULL



##################################################
# Fit KNN
#################################################


library(FNN) 

k_max <- 1000

# 1. Features vorbereiten & Skalieren
feat_cols <- names(fixed_params)

# Trainingsdaten skalieren
X <- data[, feat_cols]
X_scaled <- scale(X)

# Skalierungsparameter speichern (um den Query-Punkt gleich zu behandeln)
X_center <- attr(X_scaled, "scaled:center")
X_scale <- attr(X_scaled, "scaled:scale")

# 2. Query-Punkt vorbereiten (Belgien Parameter)
query_df <- as.data.frame(fixed_params, check.names = FALSE)

# Query-Punkt mit den Parametern der Trainingsdaten skalieren
query_df <- query_df[, feat_cols, drop=FALSE]
query_scaled <- scale(query_df, center = X_center, scale = X_scale)

# 3. Nächste Nachbarn finden
knn_res <- get.knnx(data = X_scaled, query = query_scaled, k = k_max)

# Ergebnisse extrahieren (Indizes und Distanzen)
indices <- knn_res$nn.index[1, ]
distances <- knn_res$nn.dist[1, ]

# 4. Nachbarn aus den Originaldaten holen
cols_to_select <- c(decision_features, "lap_time_adjusted")
neighbors <- data[indices, cols_to_select]

# 5. Plotten (Elbow Kurve)
plot(1:k_max, distances, type = "l", col = "blue", lwd = 2,
     main = "KNN Distances to Neighbors",
     xlab = "Neighbor Index", ylab = "Distance to Query Point")
grid()



k <- 200
knn_res_final <- get.knnx(data = X_scaled, query = query_scaled, k = k)
indices_final <- knn_res_final$nn.index[1, ]

# Nachbarn extrahieren
neighbors <- data[indices_final, c(decision_features, "lap_time_adjusted")]


neighbors_scaled <- scale(neighbors[, decision_features])

##################################################
# Determine Kmeans Clusters
#################################################



max_clusters <- 10
inertia <- numeric(max_clusters)
silhouette_scores <- numeric(max_clusters) # Wir füllen k=1 später mit NA oder 0

# 2. Loop über Cluster-Anzahlen
for (k in 1:max_clusters) {
  set.seed(0) # Reproduzierbarkeit (wie random_state=0)
  
  # nstart=25 entspricht n_init in sklearn (verhindert lokale Minima)
  km <- kmeans(neighbors_scaled, centers = k, nstart = 25)
  
  # Inertia speichern (Total within-cluster sum of squares)
  inertia[k] <- km$tot.withinss
  
  # Silhouette Score berechnen (nur für k > 1 möglich)
  if (k > 1) {
    # dist() berechnet die euklidische Distanzmatrix
    ss <- silhouette(km$cluster, dist(neighbors_scaled))
    silhouette_scores[k] <- mean(ss[, 3])
  }
}

# 3. Plot: Elbow Method (Inertia)
plot(1:max_clusters, inertia, type = "b", pch = 19, col = "black",
     main = "Elbow Method for Optimal k",
     xlab = "Number of Clusters", ylab = "Inertia (Tot.WithinSS)")
grid()

# 4. Plot: Silhouette Scores
k_range <- 2:max_clusters
plot(k_range, silhouette_scores[k_range], type = "b", pch = 19, col = "orange",
     main = "Silhouette Scores for Different k",
     xlab = "Number of Clusters", ylab = "Silhouette Score")
grid()


##################################################
# Perform Kmeans
#################################################


# 1. K-Means mit k=5 fitten
n_clusters <- 5
set.seed(0)
kmeans_final <- kmeans(neighbors_scaled, centers = n_clusters, nstart = 25)

# 2. Centroids zurücktransformieren (Inverse Scaling)
scale_vec <- attr(neighbors_scaled, "scaled:scale")
center_vec <- attr(neighbors_scaled, "scaled:center")
centers_scaled <- kmeans_final$centers

centroids_original <- t(t(centers_scaled) * scale_vec + center_vec)

# 3. Summary DataFrame erstellen
cluster_summary <- as.data.frame(centroids_original)
cluster_summary$Cluster_Size <- kmeans_final$size

mean_lap_times <- tapply(neighbors$lap_time_adjusted, kmeans_final$cluster, mean)
cluster_summary$Mean_Adjusted_Lap_Time <- as.numeric(mean_lap_times)

print("K-Means Cluster Summary:")
print(cluster_summary)

#boxplot of results
neighbors$KMeans_Cluster <- as.factor(kmeans_final$cluster)
boxplot(lap_time_adjusted ~ KMeans_Cluster, data = neighbors,
        col = RColorBrewer::brewer.pal(n_clusters, "Set2"), # oder einfach col=1:n_clusters
        main = "K-Means: Which Strategy is Faster?",
        xlab = "K-Means Cluster", ylab = "Lap Time (Adjusted)")
grid()

# --- 4. EXTRACT RESULTS: Bounds for Best Cluster ---
best_km_cluster <- as.numeric(names(which.min(mean_lap_times)))

cat(sprintf("\nBest K-Means Cluster is #%d. Use this for further optimization.\n", best_km_cluster))

best_km_data <- neighbors[neighbors$KMeans_Cluster == best_km_cluster, ]

cat("\nBounds for optimization (based on best K-Means cluster):\n")
for (col in decision_features) {
  q5 <- quantile(best_km_data[[col]], 0.05)
  q95 <- quantile(best_km_data[[col]], 0.95)
  cat(sprintf("%s: %.2f - %.2f\n", col, q5, q95))
}

##################################################
# Fit Hirachical Clustering
#################################################


# --- 1. Datenvorbereitung ---
X <- neighbors[, decision_features]
X_scaled <- scale(X)

# --- 2. Hierarchical Clustering ---
d <- dist(X_scaled, method = "euclidean")
hc <- hclust(d, method = "ward.D2")


plot(hc, labels = FALSE, hang = -1, 
     main = "Hierarchical Dendrogram (Strategy Separation)",
     xlab = "Samples", ylab = "Distance (Ward)", sub = "")
rect.hclust(hc, k = 5, border = "red")
abline(h = 10, col = "red", lty = 2)

# --- DECISION ---
k_selected <- 5
neighbors$Strategy_Cluster <- cutree(hc, k = k_selected)

cluster_means_df <- aggregate(. ~ Strategy_Cluster, 
                              data = neighbors[, c(decision_features, "Strategy_Cluster")], 
                              FUN = mean)

# Cluster-Spalte als Zeilennamen setzen für die Matrix
rownames(cluster_means_df) <- cluster_means_df$Strategy_Cluster
cluster_means_mat <- as.matrix(cluster_means_df[, -1]) # Erste Spalte (ID) entfernen


# Boxplot erstellen
boxplot(lap_time_adjusted ~ Strategy_Cluster, data = neighbors,
        col = RColorBrewer::brewer.pal(k_selected, "Set2"), # Falls RColorBrewer installiert
        main = "Which Strategy is Faster?",
        xlab = "Strategy Cluster", ylab = "Lap Time (Adjusted)")
grid()


# 1. Mittelwerte der Features berechnen
hc_summary <- aggregate(neighbors[, decision_features], 
                        by = list(Cluster = neighbors$Strategy_Cluster), 
                        FUN = mean)

# 2. Cluster-Größen hinzufügen
hc_summary$Cluster_Size <- as.vector(table(neighbors$Strategy_Cluster))

# 3. Performance (Rundenzeit) hinzufügen
hc_summary$Mean_Adjusted_Lap_Time <- as.vector(tapply(neighbors$lap_time_adjusted, 
                                                      neighbors$Strategy_Cluster, 
                                                      mean))

# 4. Output anzeigen
print("Hierarchical Cluster Summary:")
print(hc_summary)


mean_times <- tapply(neighbors$lap_time_adjusted, neighbors$Strategy_Cluster, mean)
best_cluster <- as.numeric(names(which.min(mean_times)))
best_data <- neighbors[neighbors$Strategy_Cluster == best_cluster, ]


cat(sprintf("Best cluster is #%d. Use this for further optimization.\n", best_cluster))

cat("\nBounds for optimization (based on best cluster):\n")
for (col in decision_features) {
  q5 <- quantile(best_data[[col]], 0.05)
  q95 <- quantile(best_data[[col]], 0.95)
  cat(sprintf("%s: %.2f - %.2f\n", col, q5, q95))
}






































