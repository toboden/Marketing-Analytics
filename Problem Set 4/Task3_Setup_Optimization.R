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

# weather data for Belgium Circuit 
weather_params <- list(
  'Temperature' = 7,
  'Humidity' = 68,
  'Wind (Avg. Speed)' = 54,
  'Wind (Gusts)' = 6,
  'Air Density' = 37,
  'Air Pressure' = 64
)

# 3. Parameter kombinieren
fixed_params <- append(track_params, weather_params)

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


# 4. Plot: Silhouette Scores (bug here, fix. Scores must be similar to sklearn.metrics)
k_range <- 2:max_clusters
plot(k_range, silhouette_scores[k_range], type = "b", pch = 19, col = "orange",
     main = "Silhouette Scores for Different k",
     xlab = "Number of Clusters", ylab = "Silhouette Score")



##################################################
# Perform Kmeans
#################################################


# 1. K-Means mit k=5 fitten
n_clusters <- 4
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
k_selected <- 4
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



#######################################
# Sample results to try out
#######################################

n=90
K=12

## Define function for Successive Rejects algorithm
get_draws_per_phase <- function(K, n) {
  stopifnot(K >= 2, n >= K)
  
  # \bar{log}(K) = 1/2 + sum_{i=2}^K 1/i
  logK_bar <- 0.5 + sum(1 / (2:K))
  
  k <- seq_len(K - 1)  # 1,2,...,K-1
  
  # n_k (cumulative pulls per active arm up to phase k)
  n_k <- ceiling((1 / logK_bar) * ((n - K) / (K + 1 - k)))
  n_k <- c(0, n_k)
  
  # phase increments: n_k - n_{k-1}, with n_0 = 0
  inc <- diff(n_k)
  
  total_pulls <- sum((K:2) * inc)
  
  list(n_k = n_k, inc = inc, total_pulls = total_pulls)
}

print(get_draws_per_phase(K,n))


################################
# Get final Samples
##############################


library(lhs)
#install.packages("writexl")
library(writexl) # Falls nicht installiert: install.packages("writexl")

set.seed(0)

# Deine Bounds (aus der Cluster-Analyse)
mins = c(170, 360, 40,  1,   1,   1)
maxs = c(500, 500, 480, 80, 110, 300)

decision_features <- c(
  'Front Wing',
  'Rear Wing',
  'Brake Balance',
  'Suspension',
  'Engine',
  'Differential'
)

# 1. Erzeuge LHS (Werte 0-1)
n_samples <- 30
raw_data <- randomLHS(n = n_samples, k = length(mins))

# 2. Skaliere auf Bereiche (a-b)
final_data <- raw_data # Kopie erstellen
for(i in 1:ncol(final_data)) {
  final_data[,i] <- qunif(raw_data[,i], min = mins[i], max = maxs[i])
}

# 3. Dataframe erstellen & Spaltennamen zuweisen
setup_df <- as.data.frame(final_data)
colnames(setup_df) <- decision_features

# 4. Runden auf ganze Zahlen (Integers)
setup_df <- round(setup_df, 0)

# 5. Setup-ID hinzufügen (damit man den Überblick behält)
setup_df$Setup_ID <- 1:n_samples

# ID-Spalte nach vorne schieben
setup_df <- setup_df[, c("Setup_ID", decision_features)]

# 6. Kontrolle
print(head(setup_df))

# 7. Als Excel exportieren
write_xlsx(setup_df, "Problem Set 4/Belgien_Setup_candidates.xlsx")

############################################
# after first 60
###########################################

first_60_df = read.csv('data/practice_data_belgium_first_60.csv', check.names = FALSE)
head(first_60_df)
first_60_df = first_60_df %>% filter(Track == "Belgium") %>% select(c(`Rear Wing`, `Front Wing`, 
                                                        Engine ,Brake , Differential, Suspension, `Lap Time`))
avg_times = first_60_df %>% group_by(`Rear Wing`, `Front Wing`, 
                                       Engine ,Brake , Differential, Suspension) %>% 
                        summarise(Mean_Lap_Time = mean(`Lap Time`, na.rm = TRUE)) %>%
                        rename(`Brake Balance` = Brake) %>% arrange(Mean_Lap_Time)
results = avg_times %>% left_join(setup_df, by = decision_features)
head(results)
top_15 = head(results, 15)
print("--- TOP 15 SCHNELLSTE SETUPS ---")
print(top_15 %>% select(Setup_ID, Mean_Lap_Time))

############################################
# after first 75
###########################################

first_75_df = read.csv('data/practice_data_belgium_first_75.csv', check.names = FALSE)
first_75_df = first_75_df %>% filter(Track == "Belgium") %>% select(c(`Rear Wing`, `Front Wing`, 
                                                                      Engine ,Brake , Differential, Suspension, `Lap Time`))
avg_times = first_75_df %>% group_by(`Rear Wing`, `Front Wing`, 
                                     Engine ,Brake , Differential, Suspension) %>% 
  summarise(Mean_Lap_Time = mean(`Lap Time`, na.rm = TRUE)) %>%
  rename(`Brake Balance` = Brake) %>% arrange(Mean_Lap_Time)
results = avg_times %>% left_join(setup_df, by = decision_features)
head(results)
top_15 = head(results, 5)
print("--- TOP 5 SCHNELLSTE SETUPS ---")
print(top_15 %>% select(Setup_ID, Mean_Lap_Time))


############################################
# after first 80
###########################################

first_80_df = read.csv('data/practice_data_belgium_first_80.csv', check.names = FALSE)
first_80_df = first_80_df %>% filter(Track == "Belgium") %>% select(c(`Rear Wing`, `Front Wing`, 
                                                                      Engine ,Brake , Differential, Suspension, `Lap Time`))
avg_times = first_80_df %>% group_by(`Rear Wing`, `Front Wing`, 
                                     Engine ,Brake , Differential, Suspension) %>% 
  summarise(Mean_Lap_Time = mean(`Lap Time`, na.rm = TRUE)) %>%
  rename(`Brake Balance` = Brake) %>% arrange(Mean_Lap_Time)
results = avg_times %>% left_join(setup_df, by = decision_features)
head(results)
top_15 = head(results, 5)
print("--- TOP 5 SCHNELLSTE SETUPS ---")
print(as.data.frame(top_15 %>% select(Setup_ID, Mean_Lap_Time)))

print("--- FINAL CAR SETUP ---")
print(head(as.data.frame(top_15 %>% select(Setup_ID, Mean_Lap_Time)),1))

decision_features
































