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
model_fuel <- lm(Lap.Time ~ Fuel, data = combined_data)
beta_fuel <- as.numeric(coef(model_fuel)["Fuel"])
# Unterschied zwischen soft und hard nicht signifikant
# 1 Einheit weniger Fuel macht das Auto also 0,0652 Sekunden schneller


#--------------------------------------------------------------
# Influence of Tyre Choice on LapTime:
soft_tyre_data <- practice1_data[practice1_data$Tyre.Choice == "Soft" & practice1_data$Lap < 10, ]
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

summary(model_soft) 
summary(model_medium)
summary(model_hard)

# Welcher Reifen ist der schnellste? --> Soft
summary(model_soft)$coefficients[1]   # Intercept Soft
summary(model_medium)$coefficients[1] # Intercept Medium
summary(model_hard)$coefficients[1]   # Intercept Hard

# Optimiere Strategie:
# Parameter
fuel_per_lap <- fuel_consumption
fuel_saving_per_liter <- beta_fuel
tank_capacity <- 120
pit_stop_time <- 30
total_laps <- 93 # Beispielwert für ein Rennen

# Daten aus deinen Modellen (Intercept und Steigung für Tyre.Remaining)
# Hinweis: Wir berechnen den Reifenstatus pro Runde (100 - Degradation * Runde)
tyre_configs <- list(
  soft = list(intercept = summary(model_soft)$coefficients[1], coef = 0.0114, deg = 5.9),
  medium = list(intercept = summary(model_medium)$coefficients[1], coef = -0.0008, deg = 4.24),
  hard = list(intercept = summary(model_hard)$coefficients[1], coef = -0.0083, deg = 3.12)
)

# Funktion zur Berechnung der Zeit eines Stints
calc_stint_time <- function(type, laps) {
  conf <- tyre_configs[[type]]
  times <- numeric(laps)
  
  for (n in 1:laps) {
    # 1. Reifenstatus am Anfang der Runde
    tyre_status <- max(0, 100 - (n-1) * conf$deg)
    
    # 2. Zeitvorteil durch weniger Sprit (Relativ zu vollem Tank)
    # Wir nehmen an, der Tank ist für den Stint so voll wie nötig
    fuel_in_tank <- laps * fuel_per_lap - (n-1) * fuel_per_lap
    fuel_advantage <- (tank_capacity - fuel_in_tank) * fuel_saving_per_liter
    
    # 3. Rundenzeit berechnen
    times[n] <- conf$intercept + (conf$coef * tyre_status) - fuel_advantage
  }
  return(sum(times))
}

# Strategie-Check: Alle Stintlängen für jeden Reifentyp
results <- data.frame()

for (type in names(tyre_configs)) {
  # Ein Reifen hält max bis er 0% erreicht
  max_laps <- floor(100 / tyre_configs[[type]]$deg)
  
  for (laps in 5:max_laps) {
    total_time <- calc_stint_time(type, laps)
    avg_time_with_pit <- (total_time + pit_stop_time) / laps
    avt_time_excl_pit <- total_time / laps
    
    results <- rbind(results, data.frame(
      Tyre = type,
      StintLaps = laps,
      AvgTimeIncPit = avg_time_with_pit,
      AvgTimeExcPit = avt_time_excl_pit,
      TotalFuelNeeded = laps * fuel_per_lap
    ))
  }
}

# Den effizientesten Stint finden
best_stint <- results[which.min(results$AvgTimeIncPit), ]
print(best_stint)
# Nur soft --> 5×66,29+11×64,415=1040,015 s
# 4 * soft + 1 * hard (27 Runden): 4*66,29+11*65,7485=988,394 s
ceiling(15*fuel_consumption) #fuel per stint soft
ceiling(28 * fuel_consumption) # fuel per stint hard
ceiling(13 * 3.23)


# Beste Strategie 4* 16 Runden auf Soft, mit je 34l Fuel + 29 Runden auf Hart mit 64l, um Pitstop zu vermeiden