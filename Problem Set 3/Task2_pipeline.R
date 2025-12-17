library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readr)
library(Matrix)
library(gamlr)
source("Problem Set 3/pipeline_functions.R")


# 1. Load Data
simulator_data <- read_csv("data/simulator_data.csv")

#Feature engieneering
simulator_data = simulator_data %>% mutate(
  Engine_is_min = as.numeric(Engine == min(Engine, na.rm = TRUE)),
  Engine_log = log(Engine),
  Differential_is_min = as.numeric(Differential == min(Differential)),
  Rear_Wing_is_max = as.numeric(`Rear Wing` == min(`Rear Wing`)),
  Brake_Balance_is_min = as.numeric(`Brake Balance` == min(`Brake Balance`)),
  Suspension_is_min = as.numeric(Suspension == min(Suspension))
  
)


# Define Feature Sets
car_features <- c("Rear Wing", "Engine", "Front Wing", "Brake Balance", "Differential", "Suspension")
flag_features = c("Engine_is_min", "Differential_is_min", "Rear_Wing_is_max", "Brake_Balance_is_min",
                  "Suspension_is_min")
condition_features <- c( "Lap Distance","Cornering", "Inclines", "Camber", "Grip", 
                        "Wind (Avg. Speed)", "Temperature", "Humidity", "Air Density", 
                        "Air Pressure", "Wind (Gusts)", "Altitude", "Roughness", "Width")

all_features <- c(car_features, condition_features)

#clear Lap Distcance Influence
model_distance <- lm(`Lap Time` ~ `Lap Distance`, data = simulator_data)
simulator_data$lap_time_adjusted <- residuals(model_distance)
summary(model_distance)

#prepate training data
y <- simulator_data$lap_time_adjusted
X <- explode_matrix(simulator_data, all_features)

# --- Setup ---
run_loop = FALSE
num_runs <- 50
n_rows <- nrow(X) # X is the output from explode_matrix
y <- simulator_data$lap_time_adjusted # Target is residualized lap time
set.seed(123)

# Pre-calculate random split indices for reproducibility
train_idx_list <- replicate(num_runs, sample(n_rows, floor(0.8 * n_rows)), simplify = FALSE)
stability_log <- list()

cat("Starting Stability Selection Loop...\n")



if (run_loop){
  # --- Stability Loop ---
  for(i in 1:num_runs){
    
    # Get current split data
    idx <- train_idx_list[[i]]
    X_train <- X[idx, ]; y_train <- y[idx]
    
    # Train Lasso (AICc optimized)
    cat("Training Model", i)
    model_gamlr <- gamlr(X_train, y_train, nlambda = 1000, lmr = 1e-4, verb = FALSE)
    
    # Select best model using BIC (stricter than AICc)
    bic_values <- model_gamlr$deviance + log(model_gamlr$nobs) * model_gamlr$df
    best_idx <- which.min(bic_values)
    
    # Extract non-zero coefficients (excluding intercept)
    coefs <- coef(model_gamlr, select = best_idx)
    active_feats <- rownames(coefs)[as.vector(coefs) != 0]
    stability_log[[i]] <- setdiff(active_feats, "intercept")
    
    cat(".")
  }
  cat("\nDone!\n")
  
  stability_results <- unlist(stability_log) %>%
    table() %>%
    as.data.frame() %>%
    rename(Feature = ".", Count = Freq) %>%
    mutate(Probability = Count / num_runs) %>%
    arrange(desc(Probability))
  
  
  write.csv(stability_results, 'Problem Set 3/stability_results.csv')
} else {
  cat("Load stability_resulty.csc")
  stability_results = read.csv('Problem Set 3/stability_results.csv')
  
}
# --- Analysis ---

# Filter for robust drivers (>70% selection rate)
stable_drivers <- stability_results %>% filter(Probability >= 0.7)

print(head(stable_drivers, 20))

# --- Final OLS Model ---

# Prepare final dataset using only stable features (full data)
final_features <- as.character(stable_drivers$Feature)
X_final_stable <- X[, final_features, drop = FALSE]

final_df <- data.frame(
  lap_time_adjusted = y, 
  as.matrix(X_final_stable)
)

# Fit OLS and refine with backward selection
final_ols <- lm(lap_time_adjusted ~ ., data = final_df)
final_clean <- backward_selection_p(final_ols, sig_level = 0.05)

summary(final_clean)


#plot the drivers
ggplot(stable_drivers, aes(x = reorder(Feature, Probability), y = Probability)) +
  
  # Balken
  geom_col(fill = "#2E86C1", width = 0.7) +
  
  # Text-Labels am Ende der Balken (Prozentzahl)
  geom_text(aes(label = scales::percent(Probability, accuracy = 1)), 
            hjust = -0.2, size = 3.5, color = "#444444") +
  
  # Schwellenwert-Linie (0.9 als 'Very Stable')
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "red") +
  annotate("text", x = 5, y = 0.92, label = "High Stability (>90%)", color = "red", hjust=0) +
  
  # Koordinaten drehen (für lesbare Namen)
  coord_flip() +
  
  # Achsen sauber skalieren (0 bis 1.1 für Platz für Text)
  scale_y_continuous(limits = c(0, 1.15), labels = scales::percent) +
  
  # Styling
  labs(title = "Stability Selection: Top 20 Robust Features",
       subtitle = "Frequency of selection over 50 Lasso runs (Subsampling)",
       x = NULL, # Kein Label nötig, Namen sind klar
       y = "Selection Probability") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank() # Sauberer Look
  )



#plot last training run
summary_frame = summary(model_gamlr)
summary_frame$bic = bic_values

#plot the training
plot_data <- summary_frame %>%
  select(df, aicc, bic, r2) %>%
  pivot_longer(cols = c("aicc", "bic", "r2"), 
               names_to = "Metric", 
               values_to = "Value") %>%
  mutate(Metric = factor(Metric, levels = c("aicc", "bic", "r2")))

best_df_aic <- summary_frame$df[which.min(summary_frame$aicc)]
best_df_bic <- summary_frame$df[which.min(summary_frame$bic)]

plot_lasso(plot_data, 0, best_df_bic)

write.csv(summary_frame, 'Problem Set 3/summary_frame.csv')



#optimization
france_conditions <- data.frame(
  "Lap Distance"      = 3.3, 
  "Cornering"         = 98,
  "Inclines"          = 73,
  "Camber"            = 13,
  "Grip"              = 18,   
  "Wind (Avg. Speed)" = 48,
  "Temperature"       = 39,   
  "Humidity"          = 74,
  "Air Density"       = 14,   
  "Air Pressure"      = 41,
  "Wind (Gusts)"      = 77,
  "Altitude"          = 15,
  "Roughness"         = 74,   
  "Width"             = 8,    
  check.names = FALSE
)



predict_lap_time_wrapper <- function(x) {
  
  current_car <- data.frame(
    "Rear Wing"     = x[1],
    "Engine"        = x[2],
    "Front Wing"    = x[3],
    "Brake Balance" = x[4],
    "Differential"  = x[5],
    "Suspension"    = x[6],
    check.names = FALSE
  )
  
  full_data <- bind_cols(current_car, france_conditions)
  print(head(full_data))
  
  X_matrix <- explode_matrix(full_data, all_features, verbose=FALSE)
  
  X_df <- as.data.frame(as.matrix(X_matrix))
  colnames(X_df) <- make.names(colnames(X_df))
  
  prediction <- predict(model_clean, newdata = X_df)
  
  return(as.numeric(prediction))
}


start_params <- c(250, 250, 250, 250, 250, 250)

cat("Starting L-BFGS-B Optimization...\n")

opt_result <- optim(
  par = start_params, 
  fn = predict_lap_time_wrapper, 
  method = "L-BFGS-B",       # Handles box constraints (1-500) natively
  lower = rep(1, 6),         # Minimum allowed value
  upper = rep(500, 6),       # Maximum allowed value
  control = list(
    fnscale = 1, 
    maxit = 500, 
    trace = 6,        
    REPORT = 1        
  ) 
)

# 3. Present Results
best_setup <- round(opt_result$par) # Round ONLY at the very end for the report
names(best_setup) <- c("Rear Wing", "Engine", "Front Wing", "Brake Balance", "Differential", "Suspension")

cat("\n=== OPTIMAL SETUP (France) ===\n")
print(best_setup)
cat("\nPredicted Adjusted Time:", round(opt_result$value, 4), "\n")




#Nearest neighbours of france to valudate results
df = simulator_data

target_cond <- france_conditions
track_cols <- colnames(target_cond)

# 3. Standardize data (Z-score normalization) to ensure equal weight
df_scaled <- scale(df[, track_cols])
# Scale target vector using mean/sd of the training set
target_scaled <- (unlist(target_cond) - colMeans(df[, track_cols])) / apply(df[, track_cols], 2, sd)

# 4. Compute Euclidean distance (Nearest Neighbor)
# Subtract target from all rows, square differences, sum, take sqrt
dists <- sqrt(rowSums(sweep(df_scaled, 2, target_scaled, "-")^2))

# 5. Show Top 5 most similar historical track conditions
cat("--- Nearest Neighbors (Top 5) ---\n")
print(df[order(dists)[1:5], c(car_features, "lap_time_adjusted")])
cat("\nClosest Distance:", min(dists), "\n")

closest_indices <- order(dists)[1:100]

# 2. Daten extrahieren und nach Performance sortieren
# Wir nehmen die Zeilen der Neighbors, wählen die Features + Zeit und sortieren.
best_historical_setups <- df[closest_indices, ] %>%
  select(all_of(car_features), lap_time_adjusted) %>%
  arrange(lap_time_adjusted) # Schnellste Zeit zuerst (aufsteigend)

cat("--- Best Setups in Similar Conditions (Sorted by Speed) ---\n")
print(best_historical_setups)









#Analyze feature importance
train_df_scaled <- as.data.frame(scale(train_df))

relevant_vars <- names(coef(model_clean))
relevant_vars <- relevant_vars[relevant_vars != "(Intercept)"]

cols_to_use <- c("lap_time_adjusted", relevant_vars)
model_scaled <- lm(lap_time_adjusted ~ ., data = train_df_scaled[, cols_to_use])

importance <- data.frame(
  Feature = names(coef(model_scaled)),
  Importance = abs(coef(model_scaled))
) %>%
  filter(Feature != "(Intercept)") %>% 
  arrange(desc(Importance)) %>%
  mutate(
    Share = Importance / sum(Importance),
    Cumulative = cumsum(Importance) / sum(Importance)
  )

cat("---", nrow(importance),"Overall Drivers (Standardized Impact) ---\n")
print(head(importance, 15))

#reducing to actionable features (car features)
car_regex <- paste(gsub(" ", "\\.", car_features), collapse = "|")

actionable_importance <- importance %>%
  filter(str_detect(Feature, car_regex)) %>%
  mutate(
    Actionable_Share = Importance / sum(Importance), # Anteil innerhalb der Actionables
    Actionable_Cumulative = cumsum(Importance) / sum(Importance)
  )

cat("\n--- Top 15 Actionable Setup Parameters ---\n")
cat("Anzahl gefundener Setup-Hebel:", nrow(actionable_importance), "\n")
print(head(actionable_importance, 15))

summary(model_clean)

















