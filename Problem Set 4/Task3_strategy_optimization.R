############################
# EOQ Strategy optimization
#############################

get_strategy <- function(total_laps, base_pace, pit_loss, tire_deg, fuel_sens, fuel_burn, max_fuel, max_tire) {
  
  # A: Fuel Weight Penalty (sec/lap), D: Tire Deg (sec/lap)
  A <- fuel_burn * fuel_sens
  D <- tire_deg
  
  # 1. Unconstrained Optimum (EOQ / Andler Formula)
  # L_opt = sqrt(2 * P / (A + D))
  L_math <- if ((A + D) == 0) total_laps else sqrt((2 * pit_loss) / (A + D))
  
  # 2. Apply Hard Constraints
  # The stint cannot be longer than the tank or tire life allows
  L_fuel <- max_fuel / fuel_burn
  L_limit <- min(L_math, L_fuel, max_tire)
  
  # 3. Discretize
  # Floor ensures we don't accidentally exceed fuel/tire limits by rounding up
  L_final <- max(1, floor(L_limit)) 
  
  # Calculate resulting strategy
  n_stints <- ceiling(total_laps / L_final)
  n_stops = n_stints - 1

  #calculate resulting time  
  L_avg <- total_laps / n_stints
  time_pit <- n_stops * pit_loss
  time_penalty <- total_laps * 0.5 * L_avg * (A + D)
  time_base <- total_laps * base_pace
  
  total_time <- time_base + time_pit + time_penalty
  
  return(list(
    Opt_Laps = L_final,
    Stints = n_stints,
    Stops = n_stints - 1,
    Limiter = dplyr::case_when(
      L_limit == L_math ~ "Speed (Math Optimum)",
      L_limit == L_fuel ~ "Fuel Capacity",
      TRUE ~ "Tire Life"
    ),
    Total_time = total_time
  ))
}

# --- Usage Example ---
get_strategy(total_laps=53, base_pace = 90.0,pit_loss=20, tire_deg=0.08, fuel_sens=0.035, fuel_burn=1.8, max_fuel=110, max_tire=25)

