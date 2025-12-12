library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(Matrix)
library(gamlr)


# 2. Function: Generate Polynomials & Interactions (Degree 3)
explode_matrix <- function(data, features, verbose = TRUE) { # NEUES ARGUMENT
  
  if(verbose) cat("--- START: Matrix Construction ---\n")
  
  # 1. Base Matrix
  X_base <- data %>%
    select(all_of(features)) %>%
    mutate(across(where(is.character), as.factor)) %>%
    as.matrix() %>% 
    Matrix(sparse = TRUE)
  
  # 2. Polynomials
  if(verbose) cat("[1/3] Polynomials...\n")
  # ... (Dein Polynom Code wie vorher) ...
  # Hier verkürzt dargestellt, nutze deinen vollen Code!
  cont_feats <- features # Vereinfacht
  X_cont <- X_base[, cont_feats, drop = FALSE]
  X_p2 <- X_cont^2; colnames(X_p2) <- paste0(colnames(X_cont), "_p2")
  X_p3 <- X_cont^3; colnames(X_p3) <- paste0(colnames(X_cont), "_p3")
  X_pool <- cbind(X_base, X_p2, X_p3)
  rm(X_base, X_cont, X_p2, X_p3); gc()
  
  # 3. Interactions
  if(verbose) cat("[2/3] Interactions...\n")
  
  interaction_parts <- list()
  n_cols <- ncol(X_pool)
  col_names <- colnames(X_pool)
  
  # Grade bestimmen (Hilfslogik)
  is_p2 <- grepl("_p2$", col_names); is_p3 <- grepl("_p3$", col_names)
  degrees <- ifelse(is_p3, 3, ifelse(is_p2, 2, 1))
  
  for(i in 1:(n_cols-1)) {
    vec_i <- X_pool[, i]
    target_idx <- (i + 1):n_cols
    
    # Filter Maske (Degree <= 3)
    mask_degree <- (degrees[i] + degrees[target_idx]) <= 3
    valid_targets <- target_idx[mask_degree]
    
    if(length(valid_targets) == 0) next
    
    X_target <- X_pool[, valid_targets, drop=FALSE]
    X_inter  <- X_target * vec_i
    colnames(X_inter) <- paste0(col_names[i], ":", colnames(X_target))
    interaction_parts[[i]] <- X_inter
  }
  
  # 4. Assembly
  if(verbose) cat("[3/3] Assembly...\n")
  
  if(length(interaction_parts) > 0) {
    X_final <- cbind(X_pool, do.call(cbind, interaction_parts))
  } else {
    X_final <- X_pool
  }
  
  if(verbose) cat("--- DONE ---\n")
  return(X_final)
}



plot_lasso <- function(plot_data, best_df_aic, best_df_bic) {
  
  # Abhängigkeit sicherstellen
  if (!require("ggplot2")) install.packages("ggplot2")
  library(ggplot2)
  
  # Plot erstellen
  p <- ggplot(plot_data, aes(x = df, y = Value)) +
    # Hauptlinien
    geom_line(size = 0.8, color = "grey30") +
    geom_point(aes(color = Metric), size = 1.2) +
    
    # Vertikale Linien für die Gewinner (AICc rot, BIC blau)
    geom_vline(xintercept = best_df_aic, color = "red", linetype = "dashed") +
    geom_vline(xintercept = best_df_bic, color = "blue", linetype = "dashed") +
    
    # Text-Label für die Minima
    annotate("text", x = best_df_bic, y = Inf, label = "BIC Best", 
             vjust = 2, hjust = 1.1, color = "blue", size = 3) +
    
    # Faceting
    facet_wrap(~Metric, scales = "free_y", ncol = 1, 
               labeller = as_labeller(c(aicc = "AICc (Information)", 
                                        bic = "BIC (Stricter)", 
                                        r2 = "R-Squared"))) +
    
    # Styling
    scale_color_manual(values = c("red", "blue", "green4")) +
    labs(title = "Model Complexity vs. Performance",
         subtitle = paste0("Optimal Variables: AICc = ", best_df_aic, " | BIC = ", best_df_bic),
         x = "Degrees of Freedom (Anzahl Variablen)",
         y = "Wert") +
    theme_minimal() +
    theme(legend.position = "none", 
          strip.text = element_text(face = "bold", size = 10))
  
  return(p)
}


backward_selection_p <- function(model, sig_level = 0.1) {
  
  current_model <- model
  
  while(TRUE) {
    # 1. Tabelle der p-Werte holen
    coef_table <- summary(current_model)$coefficients
    p_values <- coef_table[, 4]
    
    # Intercept ignorieren (nie löschen!)
    if("(Intercept)" %in% names(p_values)) {
      p_values <- p_values[names(p_values) != "(Intercept)"]
    }
    
    # 2. Das schlechteste Feature finden
    max_p <- max(p_values)
    worst_feature <- names(p_values)[which.max(p_values)]
    
    # 3. Abbruchbedingung: Wenn alle signifikant sind -> Fertig
    if(max_p < sig_level) {
      break
    }
    
    # 4. Rauswurf & Update
    # Wir nutzen update(), um das Modell ohne dieses Feature neu zu rechnen
    # " . ~ . - Feature" heißt: Gleiche Formel, aber minus das Feature
    cat("Entferne:", worst_feature, "(p =", round(max_p, 4), ")\n")
    current_model <- update(current_model, as.formula(paste(". ~ . -", "`", worst_feature, "`", sep="")))
  }
  
  return(current_model)
}

get_metrics <- function(actual, predicted, set_name = "Set") {
  residuals <- actual - predicted
  
  mse <- mean(residuals^2)
  mae <- mean(abs(residuals))
  
  # R2 manuell berechnen (für Testdaten ist das sicherer als summary())
  tss <- sum((actual - mean(actual))^2) # Total Sum of Squares
  rss <- sum(residuals^2)               # Residual Sum of Squares
  r2  <- 1 - (rss / tss)
  
  return(data.frame(
    Set = set_name,
    R2  = round(r2, 5),
    MSE = round(mse, 5),
    MAE = round(mae, 5),
    RMSE = round(sqrt(mse), 5)
  ))
}





