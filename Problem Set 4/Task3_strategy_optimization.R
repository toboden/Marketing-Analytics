############################
# EOQ Strategy optimization
#############################

get_strategy_no_deg <- function(
    total_laps = 63,
    pit_loss = 30,
    max_fuel = 120,
    fuel_burn = 3.37,
    fuel_effect_per_lap = 0.2646, # A
    
    base_pace,              # Zeit Runde 0
    wear_percent_per_lap    # Wie viel % Leben verliert der Reifen?
) {
  
  # --- A. Limits berechnen ---
  # 1. Tank Limit (Runden)
  L_fuel <- max_fuel / fuel_burn
  
  # 2. Reifen Limit (Runden bis 100% Wear)
  # Wenn wear=0, hält er ewig
  L_tire <- if(wear_percent_per_lap == 0) 999 else (100 / wear_percent_per_lap)
  
  # --- B. Entscheidung ---
  # Wir stoppen NUR, wenn wir müssen (Tank oder Reifen am Ende)
  L_limit <- min(L_fuel, L_tire)
  
  # Stintlänge (abgerundet)
  L_final <- max(1, floor(L_limit))
  
  # Strategie Struktur
  # Wenn L_final > total_laps, fahren wir durch (1 Stint)
  L_final_capped <- min(L_final, total_laps)
  
  n_stints <- ceiling(total_laps / L_final_capped)
  n_stops <- n_stints - 1
  L_avg <- total_laps / n_stints 
  
  # --- C. Zeit-Vorhersage ---
  time_base <- total_laps * base_pace
  time_pit  <- n_stops * pit_loss
  
  # Gewinn durch Sprit (Auto wird leichter)
  # Formel: Summe aller Runden * Durchschnittlicher Fuel-Gain
  # Da D=0, werden wir jede Runde um 'fuel_effect' schneller.
  # Zeitgewinn = 0.5 * TotalLaps * (StintLänge - 1) * A
  time_gain_fuel <- total_laps * 0.5 * (L_avg - 1) * fuel_effect_per_lap
  
  # Gesamtzeit = Basis + Box - SpritGewinn
  total_race_time <- time_base + time_pit - time_gain_fuel
  
  return(list(
    Strategy = paste0(n_stints, " Stint(s)"),
    Laps_Avg = round(L_avg, 1),
    Stops = n_stops,
    Predicted_Time_Min = round(total_race_time / 60, 2),
    Limit_Reason = case_when(
      L_limit == L_fuel ~ "Fuel Limited",
      L_limit == L_tire ~ "Tire Life (100% Wear)",
      TRUE ~ "No Limit (Race Distance)"
    )
  ))
}

# ---------------------------------------------------------
# 2. DEINE DATEN
# ---------------------------------------------------------

# Hier musst du jetzt nur noch Base Pace und Haltbarkeit (%) tunen
tire_specs <- data.frame(
  Compound = c("Extra Soft", "Soft", "Medium", "Hard"),
  
  # Wie schnell ist er in Runde 0?
  Base_Pace = c(92.59, 93.41, 93.43, 93.64), 
  
  # D ist jetzt irrelevant (0), aber wir brauchen den %-Verschleiß!
  # Beispiel: Extra Soft verliert 6% pro Runde -> hält ~16 Runden
  Wear_Percent = c(5.98, 3.92, 3.42, 3.19) 
)

results <- data.frame()

for(i in 1:nrow(tire_specs)) {
  t <- tire_specs[i,]
  
  res <- get_strategy_no_deg(
    total_laps = 63,
    pit_loss = 30,
    max_fuel = 120,
    fuel_burn = 3.37,
    fuel_effect_per_lap = 0.2646,
    
    base_pace = t$Base_Pace,
    wear_percent_per_lap = t$Wear_Percent
  )
  
  results <- rbind(results, data.frame(
    Compound = t$Compound,
    Strategy = res$Strategy,
    Stops = res$Stops,
    Laps_Stint = res$Laps_Avg,
    Total_Time = res$Predicted_Time_Min,
    Reason = res$Limit_Reason
  ))
}

# Sortieren (Schnellste Zeit oben)
results <- results[order(results$Total_Time),]

# Gap berechnen
best_time <- results$Total_Time[1]
results$Gap_Min <- round(results$Total_Time - best_time, 2)

print(results)
