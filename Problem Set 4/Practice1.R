library(readr)
library(ggplot2)
practice1_data <- read.csv('data/practice_data-6.csv')

# tyre degradation soft
soft_deg <- practice1_data[111, "Tyre.Remaining"] - practice1_data[112, "Tyre.Remaining"]
# tyre degradation medium
medium_deg <- practice1_data[108, "Tyre.Remaining"] - practice1_data[109, "Tyre.Remaining"]
# tyre degradation hard
hard_deg <- practice1_data[71, "Tyre.Remaining"] - practice1_data[72, "Tyre.Remaining"]
# fuel consumption
fuel_consumption <- practice1_data[71, "Fuel"] - practice1_data[72, "Fuel"]

# Influence of Fuel on LapTime:
# Hard tyre data:
fuel_exp_data_hard <- practice1_data[c(71, 103:107), ]
# Soft tyre data:
fuel_exp_data_soft <- practice1_data[c(96, 111, 98:102), ]

combined_data <- rbind(fuel_exp_data_hard, fuel_exp_data_soft)

interaction_model_fuel <- lm(Lap.Time ~ Fuel * Tyre.Choice, data = combined_data)
summary(interaction_model_fuel) 
beta_fuel <- as.numeric(coef(interaction_model_fuel)["Fuel"])
# Unterschied zwischen soft und hard nicht signifikant
# 1 Einheit weniger Fuel macht das Auto also 0,0652 Sekunden schneller


#--------------------------------------------------------------
# Influence of Tyre Choice on LapTime:
soft_tyre_data <- practice1_data[practice1_data$Tyre.Choice == "Soft", ]
medium_tyre_data <- practice1_data[practice1_data$Tyre.Choice == "Medium" & practice1_data$Fuel >= 114, ]
hard_tyre_data <- practice1_data[practice1_data$Tyre.Choice >= "Hard" & practice1_data$Stint == 71, ]

# Add influence of fuel to laptime:
target_fuel <- 120
soft_tyre_data$LapTime_Adjusted <- soft_tyre_data$Lap.Time + 
  beta_fuel * (target_fuel - soft_tyre_data$Fuel)
medium_tyre_data$LapTime_Adjusted <- medium_tyre_data$Lap.Time + 
  beta_fuel * (target_fuel - medium_tyre_data$Fuel)
hard_tyre_data$LapTime_Adjusted <- hard_tyre_data$Lap.Time + 
  beta_fuel * (target_fuel - hard_tyre_data$Fuel)

# Modelle für jede Mischung berechnen
model_soft   <- lm(LapTime_Adjusted ~ Tyre.Remaining, data = soft_tyre_data)
model_medium <- lm(LapTime_Adjusted ~ Tyre.Remaining, data = medium_tyre_data)
model_hard   <- lm(LapTime_Adjusted ~ Tyre.Remaining, data = hard_tyre_data)

# Koeffizienten extrahieren (Zeitverlust pro 1% Verschleiß)
beta_soft   <- coef(model_soft)["Tyre.Remaining"]
beta_medium <- coef(model_medium)["Tyre.Remaining"]
beta_hard   <- coef(model_hard)["Tyre.Remaining"]

# Ergebnisse anzeigen
cat("Zeitverlust pro 1% Verschleiß:\n",
    "Soft:  ", beta_soft, "\n",
    "Medium:", beta_medium, "\n",
    "Hard:  ", beta_hard, "\n")

# 1. Daten für das Plotting vorbereiten
# Wir fügen eine Spalte "Type" hinzu, um die Gruppen im Plot zu unterscheiden
soft_plot_data   <- soft_tyre_data[, c("Tyre.Remaining", "LapTime_Adjusted")]
soft_plot_data$Tyre   <- "Soft"

medium_plot_data <- medium_tyre_data[, c("Tyre.Remaining", "LapTime_Adjusted")]
medium_plot_data$Tyre <- "Medium"

hard_plot_data   <- hard_tyre_data[, c("Tyre.Remaining", "LapTime_Adjusted")]
hard_plot_data$Tyre   <- "Hard"

# Alles in einem Dataframe kombinieren
plot_df <- rbind(soft_plot_data, medium_plot_data, hard_plot_data)

# 2. Den Plot erstellen
ggplot(plot_df, aes(x = Tyre.Remaining, y = LapTime_Adjusted, color = Tyre)) +
  geom_point(alpha = 0.5) +  # Die tatsächlichen (angepassten) Datenpunkte
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) + # Die Regressionslinien
  scale_color_manual(values = c("Soft" = "red", "Medium" = "yellow", "Hard" = "white")) +
  theme_dark() + # Dunkles Theme passt gut zu Motorsport-Daten
  labs(
    title = "Einfluss des Reifenverschleißes auf die Rundenzeit",
    subtitle = "Bereinigt um Kraftstoffeinfluss (Target Fuel = 120)",
    x = "Reifenzustand in % (Tyre Remaining)",
    y = "Adjusted Lap Time (s)",
    color = "Mischung"
  ) +
  scale_x_reverse() # Da der Verschleiß abnimmt (100 -> 0), ist die Umkehrung oft intuitiver


# Welcher Reifen ist der schnellste?
summary(model_soft)$coefficients[1]   # Intercept Soft
summary(model_medium)$coefficients[1] # Intercept Medium
summary(model_hard)$coefficients[1]   # Intercept Hard
