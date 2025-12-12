# load data
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
#install.packages("plotly")
library(plotly)

#load the data
simulator_data <- read_csv("data/simulator_data.csv")

#check for NA
simulator_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Spalte", values_to = "Anzahl_NAs") %>%
  filter(Anzahl_NAs > 0)

#View summary statistics
summary(simulator_data)

#check for hidden categorical variables
possible_categoricals <- simulator_data %>%
  summarise(across(everything(), n_distinct)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Unique_Values") %>%
  filter(Unique_Values < 100) %>% # Zeige nur Spalten mit wenigen Ausprägungen
  arrange(Unique_Values)

print(possible_categoricals)

# theme for plots
theme_fontsize <- theme(
  plot.title = element_text(size = 14),
  plot.subtitle = element_text(size = 10),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 11),
  legend.text = element_text(size = 11),
)

#categorize features
target = c("Lap Time")
car_features <- c(
  "Rear Wing", 
  "Engine", 
  "Front Wing", 
  "Brake Balance", 
  "Differential", 
  "Suspension"
)
condition_features <- c(
  "Lap Distance", 
  "Cornering", 
  "Inclines", 
  "Camber",       # Neigung der Kurven
  "Grip",         # Grip-Niveau der Strecke
  "Wind (Avg. Speed)", 
  "Temperature", 
  "Humidity", 
  "Air Density", 
  "Air Pressure", 
  "Wind (Gusts)", 
  "Altitude", 
  "Roughness",    # Bodenwellen der Strecke
  "Width"
)


create_histogram <- function(data, column) {
  
  column_mean <- mean(data[[column]], na.rm = TRUE)
  
  ggplot(data, aes(x = .data[[column]])) + 
    
    geom_histogram(aes(y = after_stat(density)), 
                   bins = 40, 
                   fill = "#69b3a2",   
                   color = "white",    
                   alpha = 0.7) +      
    
    geom_density(color = "#404080", linewidth = 1, fill="#404080", alpha=0.1) +
    
    geom_vline(xintercept = column_mean, 
               color = "darkred", linetype = "dashed", linewidth = 1) +
    
    annotate("text", x = column_mean, y = 0.01, 
             label = paste("Mean:", round(column_mean, 2)), 
             color = "darkred", angle = 90, vjust = -1, hjust = 0) +
    
    labs(title = paste("Distribution of", column),
         subtitle = "Histogram and Kernel Density Estimate",
         x = column,
         y = "Density") +
    
    theme_minimal() + 
    theme_fontsize
}


plot_2d_scatter <- function(data, x_var, y_var, color_var = NULL, trendline = FALSE, sample_rate = 1.0) {
  
  # 1. Downsampling
  if (sample_rate < 1.0 && sample_rate > 0) {
    n_keep <- floor(nrow(data) * sample_rate)
    data <- data[sample(nrow(data), n_keep), ]
    sample_note <- paste0(" (Sampled: ", sample_rate * 100, "%)")
  } else {
    sample_note <- ""
  }
  
  # 2. Basis-Plot
  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
  
  # 3. Punkte
  if (!is.null(color_var)) {
    p <- p + geom_point(aes(color = .data[[color_var]]), alpha = 0.8, size = 2.5) +
      labs(color = color_var)
    
    if (is.numeric(data[[color_var]])) {
      p <- p + scale_color_viridis_c(option = "magma", end = 0.9)
    } else {
      p <- p + scale_color_brewer(palette = "Dark2")
    }
  } else {
    p <- p + geom_point(color = "#2E86C1", alpha = 0.7, size = 2.5)
  }
  
  # 4. Trendlinien & R2 (HIER WAR DER FEHLER)
  r2_info <- ""
  
  if (trendline) {
    # WICHTIG: Backticks (`) einfügen, damit Leerzeichen kein Problem sind
    # paste0 statt paste, um Lücken zu vermeiden
    f_lin  <- as.formula(paste0("`", y_var, "` ~ `", x_var, "`"))
    f_quad <- as.formula(paste0("`", y_var, "` ~ poly(`", x_var, "`, 2)"))
    
    # Fits berechnen
    m_lin  <- lm(f_lin, data = data)
    m_quad <- lm(f_quad, data = data)
    
    # R2 extrahieren
    r2_lin  <- round(summary(m_lin)$r.squared, 3)
    r2_quad <- round(summary(m_quad)$r.squared, 3)
    
    r2_info <- paste0("\nLinear R² (Red): ", r2_lin, " | Quadratic R² (Blue): ", r2_quad)
    
    # Linien zum Plot hinzufügen
    p <- p + 
      geom_smooth(method = "lm", formula = y ~ x, 
                  color = "#D95F02", size = 1, se = FALSE) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
                  color = "#1B9E77", size = 1, se = FALSE)
  }
  
  # 5. Styling
  if (!is.null(color_var)) {
    sub_text <- paste0("Colored by: ", color_var, sample_note, r2_info)
  } else if (sample_note != "" || r2_info != "") {
    sub_text <- paste0("Data randomly downsampled", sample_note, r2_info)
  } else {
    sub_text <- NULL
  }
  
  p <- p + 
    labs(
      title = paste(x_var, "vs.", y_var),
      subtitle = sub_text,
      x = x_var,
      y = y_var
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, color = "#333333"),
      plot.subtitle = element_text(size = 11, color = "#444444"),
      axis.title = element_text(face = "bold"),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

plot_interaction <- function(data, x_var, interact_var, target = "lap_time_adjusted") {
  
  # 1. Daten vorbereiten
  # Wir schneiden die Interaktions-Variable in 3 Teile (Quantile)
  plot_data <- data %>%
    select(.data[[x_var]], .data[[interact_var]], .data[[target]]) %>%
    mutate(
      Group = cut_number(.data[[interact_var]], n = 3, 
                         labels = c("Low", "Medium", "High"))
    )
  
  # 2. Plotten
  ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[target]], color = Group)) +
    
    # Punkte (leicht transparent, damit man die Masse sieht)
    geom_point(alpha = 0.3, size = 1) +
    
    # Trendlinien (Linear) - Das ist der Beweis!
    # se = FALSE macht den Plot sauberer
    geom_smooth(method = "lm", se = FALSE, size = 1.5) +
    
    # Styling
    scale_color_manual(values = c("Low" = "#E74C3C",    # Rot
                                  "Medium" = "#F1C40F", # Gelb
                                  "High" = "#2E86C1")) + # Blau
    
    labs(title = paste("Interaction:", x_var, "x", interact_var),
         subtitle = paste("Effect of", x_var, "on Time, split by", interact_var),
         x = x_var,
         y = paste(target, "(Residuals)"),
         color = paste(interact_var, "Level")) +
    
    theme_minimal() +
    theme(plot.title = element_text(face="bold"),
          legend.position = "bottom")
}



plot_2d_scatter(simulator_data, "Lap Distance","Lap Time", sample_rate = 0.1, trendline = TRUE)

#plot_2d_scatter(simulator_data, "Cornering","Lap Time", sample_rate = 0.1, trendline = TRUE)

#plot_2d_scatter(simulator_data, "Grip", "Lap Time", sample_rate = 0.1, trendline = TRUE)

#checking pearson correlations with target and distance
cor(simulator_data[condition_features], simulator_data[target])
cor(simulator_data[car_features], simulator_data[target])

cor(simulator_data[condition_features], simulator_data['Lap Distance'])
cor(simulator_data[car_features], simulator_data['Lap Distance'])

#checking spearman correlation to detect general monototonous relationships
cor(simulator_data[condition_features], simulator_data[target], method = "spearman")
cor(simulator_data[car_features], simulator_data[target], method = "spearman")


cor(simulator_data[condition_features], simulator_data['Lap Distance'], method = "spearman")
cor(simulator_data[car_features], simulator_data['Lap Distance'], method = "spearman")

#distance adjust lap time
model_distance <- lm(`Lap Time` ~ `Lap Distance`, data = simulator_data)
summary(model_distance)
simulator_data$lap_time_adjusted <- residuals(model_distance)

#correlations with adjusted lap time
cor(simulator_data[setdiff(condition_features, "Lap Distance")], simulator_data['lap_time_adjusted'])
cor(simulator_data[car_features], simulator_data['lap_time_adjusted'])

#spearmen correlations with adjusted lap time
cor(simulator_data[setdiff(condition_features, "Lap Distance")], simulator_data['lap_time_adjusted'], method='spearman')
cor(simulator_data[car_features], simulator_data['lap_time_adjusted'], method='spearman')

#check for correlations between features
cor_matrix <- cor(select_if(simulator_data, is.numeric))

cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA

sorted_correlations <- as.data.frame(as.table(cor_matrix)) %>%
  na.omit() %>% 
  rename(Var1 = Var1, Var2 = Var2, Correlation = Freq) %>%
  arrange(desc(abs(Correlation))) 

print(head(sorted_correlations, 20))




#Analyze Engine as special case
plot_2d_scatter(simulator_data, "Engine", "lap_time_adjusted", sample_rate = 0.3, trendline=TRUE)
create_histogram(simulator_data, "Engine")
logged_Engine = simulator_data %>% mutate(
  Engine =log(Engine)
)
plot_2d_scatter(logged_Engine, "Engine", "lap_time_adjusted", sample_rate = 0.3, trendline=TRUE)
min_Engine = simulator_data %>%
  filter(Engine == 1)
nmin_Engine = simulator_data %>% filter(Engine >1)
create_histogram(min_Engine, "lap_time_adjusted")
create_histogram(nmin_Engine, "Engine")

############################
### ANALYZE CAR FEATURES ###
############################
plot_2d_scatter(simulator_data, "Differential", "Lap Time", sample_rate = 0.3, trendline = TRUE)
plot_2d_scatter(simulator_data, "Differential", "lap_time_adjusted", sample_rate = 0.3, trendline=TRUE)
create_histogram(simulator_data, "Differential")

create_histogram(simulator_data, "Rear Wing")
plot_2d_scatter(simulator_data, "Rear Wing", "lap_time_adjusted", sample_rate = 0.3, trendline = TRUE)

create_histogram(simulator_data, "Brake Balance")
plot_2d_scatter(simulator_data, "Brake Balance", "lap_time_adjusted", sample_rate = 0.3, trendline = TRUE)

create_histogram(simulator_data, "Front Wing")
plot_2d_scatter(simulator_data, "Front Wing", "lap_time_adjusted", sample_rate = 0.3, trendline = TRUE)

create_histogram(simulator_data, "Suspension")
plot_2d_scatter(simulator_data, "Suspension", "lap_time_adjusted", sample_rate = 0.3, trendline = TRUE)

##############################
### ANALYZE TRACK FEATURES ###
##############################
create_histogram(simulator_data, "Lap Distance")
create_histogram(simulator_data, "Cornering")
create_histogram(simulator_data, "Inclines")
create_histogram(simulator_data, "Camber")
create_histogram(simulator_data, "Grip")
create_histogram(simulator_data, "Wind (Avg. Speed)")
create_histogram(simulator_data, "Temperature")
create_histogram(simulator_data, "Humidity")
create_histogram(simulator_data, "Air Pressure")


#Check Extrem Vale Share of Car Features
simulator_data %>%
  select(all_of(car_features)) %>%
  pivot_longer(everything(), names_to = "Feature", values_to = "Value") %>%
  group_by(Feature) %>%
  summarise(
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    Min_Share = mean(Value == min(Value, na.rm = TRUE)),
    Max_Share = mean(Value == max(Value, na.rm = TRUE))
  ) %>%
  arrange(desc(Min_Share + Max_Share)) %>%
  mutate(
    Min_Pct = paste0(round(Min_Share * 100, 1), "%"),
    Max_Pct = paste0(round(Max_Share * 100, 1), "%")
  ) %>%
  select(Feature, Min, Max, Min_Pct, Max_Pct)


#test track features for unifomity
check_uniform <- function(x) {
  c(
    KS_p = ks.test(x, "punif", min(x), max(x))$p.value,
    X2_p = chisq.test(table(cut(x, breaks = 20)))$p.value
  )
}

sapply(simulator_data[condition_features], check_uniform)

#test car features for normality and uniformity
sapply(simulator_data[car_features], function(x) {
  x_trimmed <- x[x > min(x, na.rm=TRUE) & x < max(x, na.rm=TRUE)]
  jarque.bera.test(x_trimmed)$p.value
})

sapply(simulator_data[car_features], function(x) {
  x_trimmed <- x[x > min(x, na.rm=TRUE) & x < max(x, na.rm=TRUE)]
  check_uniform(x_trimmed)
})

#check range of Distance
simulator_data %>% group_by(`Lap Distance`) %>% count()




#check for interactions
plot_interaction(simulator_data, "Front Wing", "Rear Wing")
plot_interaction(simulator_data, "Brake Balance", "Cornering")
plot_interaction(simulator_data, "Front Wing", "Air Pressure")
plot_interaction(simulator_data, "Engine", "Inclines")






