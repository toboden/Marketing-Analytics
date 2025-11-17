# Read data
library(readr)
library(dplyr)
daily_data <- read_tsv("data/daily_fish_market_data.txt")
daily_data <- daily_data %>%
  rename(price_log=price, qty_log=qty)
# Plot data to get understanding of it
plot(daily_data$price, daily_data$qty, xlab="log(Price)", ylab="log(Quantity)")
# price and qty are the log of the original price/ qty
daily_data$price_original <- exp(daily_data$price)
daily_data$qty_original <- exp(daily_data$qty)

plot(daily_data$price_original, daily_data$qty_original, xlab="Price", ylab="Quantity")
linear_reg = lm(daily_data$qty_original~ daily_data$price_original)
abline(linear_reg, col = "red", lwd = 2)
summary(linear_reg)


# 1. Identify outliers:
# 1.1 Examine potential outliers in dependent variable (quantity)
# Leverage: Idea -> extreme observations on x are weighted more strongly
# Step 1: Compute weights w_i

# Step 2: Compute leverage h_i=1/n+w_i
# Leverage-Werte berechnen
h = hatvalues(linear_reg)
# Step 3: Get highest h values and compare them to 2/n
top10 <- order(h, decreasing = TRUE)[1:10]
n = nobs(linear_reg)
equal_influence = 2 / n
equal_influence
text(daily_data$price_original[top10],
     daily_data$qty_original[top10],
     labels = top10,
     pos = 3)   # Label über dem Punkt
h[top10]
daily_data$price_original[top10]
# --> Interpretation:
# Werte mit einem Preis von über 1,5 beeinflussen das Ergebnis sehr stark

# 1.2 Examine potential outliers in independent variable (price)
# Studentized residuals: Idea -> Use residuals to find outliers
# Step 1: 

# studentized (deleted) residuals
r_student <- rstudent(linear_reg)
top5_idx = order(r_student, decreasing = TRUE)[1:5]
top5_values = r_student[top5_idx]
# No value that is larger than 1.96
low5_idx = order(r_student, decreasing = FALSE)[1:5]
low5_values = r_student[low5_idx]
# 4 values that are lower than -1.96
plot(daily_data$price_original, daily_data$qty_original, xlab="Price", ylab="Quantity")
abline(linear_reg, col = "red", lwd = 2)
text(daily_data$price_original[top5_idx],
     daily_data$qty_original[top5_idx],
     labels = top5_idx,
     pos = 3)

# Labels für low 5
text(daily_data$price_original[low5_idx],
     daily_data$qty_original[low5_idx],
     labels = low5_idx,
     pos = 3)
r_student[top5_idx]
r_student[low5_idx]
#--> Interpretation:
# Die höchsten 5 residuals sind deutlich über 1,96. Die niedrigsten sind nicht kleiner als -1,96.
# Insbesondere Wert 68 sticht heraus.

# 1.3 Examine potential outliers considering dependent and independent variable
# Cooks Distance: Looks for observation

# 1) Cook's distance mit eingebauter Funktion
sd_res = sd(resid(linear_reg)) # standard deviation of residuals

D <- cooks.distance(linear_reg, res = r_student, sd= sd_res, hat=h)
# Ausgabe: erste Werte
head(D)

# 2) Indizes der 10 größten Cook's distances
top10_D <- order(D, decreasing = TRUE)[1:10]
top10_D
D[top10_D]

# 4) Plot: Punkte proportional zu Cook's D und Top-10 labeln
plot(daily_data$price_original, daily_data$qty_original,
     xlab = "Price", ylab = "Quantity",
     main = "Price vs Quantity (Cook's distance hervorgehoben)")

abline(linear_reg, col = "red", lwd = 2)


# Top-10 labeln
text(daily_data$price_original[top10_D],
     daily_data$qty_original[top10_D],
     labels = top10_D,
     pos = 3, cex = 0.8, col = "blue")

# 5) (Optional) Vergleich mit der Folienformel:
# Folie: D_i = r_i*^2 * h_i / (2 s^2 (1 - h_i))
r_student <- rstudent(linear_reg)   # extern studentized residuals (r_i^*)
h <- hatvalues(linear_reg)
s <- summary(linear_reg)$sigma      # Residual standard error
sd_res = sd(resid(linear_reg)) # standard deviation of residuals
D_slide <- (r_student^2 * h) / (2*sd_res^2 * (1 - h))  # nur, wenn die Folie p=2 impliziert
top_10_D_slide = order(D_slide, decreasing = TRUE)[1:10]
D_slide[top_10_D_slide]
# Vergleiche Maximum / Differenzen
head(D_slide)

# --> Interpretation: should we delete outliers?


# 2. Compare linear, polynomial and logarithmic functional forms
# 2.1 Define hypothesized functional form (linear, polynomial and logarithmic)
# Hypothesized functional form of the logarithmic transformation:
# Quantity = alpha*Price^beta, beta < 0 --> ln(Quantity) = ln(alpha) + beta*ln(price)

# Hypothesized functional form of the polynomial regression:
# 2.2 Look at scatterplot of data
plot(daily_data$price_original, daily_data$qty_original, xlab="Price", ylab="Quantity")
# 2.3 Linear regression
# 2.4 Polynomial regression
# 2.4.1 Mean centering
daily_data$price_original_MC = daily_data$price_original-mean(daily_data$price_original, na.rm=TRUE)
summary(daily_data$price_original_MC)
# 2.4.2 Compute polynomials
daily_data$price_original_MC_squared = daily_data$price_original_MC * daily_data$price_original_MC
daily_data$price_original_MC_cubic = daily_data$price_original_MC_squared * daily_data$price_original_MC
# running the linear model
summary(linear_reg)
# 2.4.3 Analyze and test hypothesized model
polynomial_regression_squared = lm(daily_data$qty_original~ daily_data$price_original_MC+daily_data$price_original_MC_squared)
summary(polynomial_regression_squared)
c(AIC(polynomial_regression_squared), BIC(polynomial_regression_squared))
plot(daily_data$price_original, daily_data$qty_original, xlab="Price", ylab="Quantity", main="polynomial model on untransformed data") 
b0 <- coef(polynomial_regression_squared)[1]   # intercept
b1 <- coef(polynomial_regression_squared)[2]
b2 <- coef(polynomial_regression_squared)[3]
curve(
  b0[1] + x*b1[1] + b2[1]*(x^2),
  from = min(daily_data$price_original, na.rm = TRUE),
  to   = max(daily_data$price_original, na.rm = TRUE),
  add  = TRUE,
  col  = "red",
  lwd  = 2
)
# 2.4.4 if tests are successful: Analyze extended model
polynomial_regression_cubic = lm(daily_data$qty_original~ daily_data$price_original_MC+daily_data$price_original_MC_squared+daily_data$price_original_MC_cubic)
summary(polynomial_regression_cubic)
c(AIC(polynomial_regression_cubic), BIC(polynomial_regression_cubic))


# 2.5 logarithmic transformation (log-log model)
plot(daily_data$price, daily_data$qty, xlab="log(Price)", ylab="log(Quantity)")
# the column price and qty or the daily dataset are already log values
min(daily_data$qty_original) # min > 0
min(daily_data$price_original) # min > 0
log_log_model = lm(cleaned_daily_data$qty~ cleaned_daily_data$price)
summary(log_log_model)
c(AIC(log_log_model), BIC(log_log_model))
# visualize
plot(daily_data$price, daily_data$qty, xlab="log(Price)", ylab="log(Quantity)")
abline(log_log_model, col = "red", lwd = 2)
# Koeffizienten aus dem lm holen
b0 <- coef(log_log_model)[1]   # intercept
b1 <- coef(log_log_model)[2]   # first_coeff
plot(daily_data$price_original, daily_data$qty_original, xlab="Price", ylab="Quantity", main="log-log model on untransformed data") 

curve(
  exp(b0[1]) * x^b1[1],
  from = min(daily_data$price_original, na.rm = TRUE),
  to   = max(daily_data$price_original, na.rm = TRUE),
  add  = TRUE,
  col  = "red",
  lwd  = 2
)


# 2.5 logarithmic transformation (linear-log model)
plot(daily_data$price, daily_data$qty_original, xlab="log(Price)", ylab="Quantity")
# the column price and qty or the daily dataset are already log values
lin_log_model = lm(cleaned_daily_data$qty_original~ cleaned_daily_data$price)
summary(lin_log_model)
c(AIC(lin_log_model), BIC(lin_log_model))
# visualize
plot(daily_data$price, daily_data$qty_original, xlab="log(Price)", ylab="Quantity", main="lin-log model")
abline(lin_log_model, col = "red", lwd = 2)
# Koeffizienten aus dem lm holen
b0_lin_log <- coef(lin_log_model)[1]   # intercept
b1_lin_log <- coef(lin_log_model)[2]   # first_coeff
plot(daily_data$price_original, daily_data$qty_original, xlab="Price", ylab="Quantity", main="lin-log model on untransformed data") 

curve(
  b0_lin_log[1] + b1_lin_log[1] * log(x),
  from = min(daily_data$price_original, na.rm = TRUE),
  to   = max(daily_data$price_original, na.rm = TRUE),
  add  = TRUE,
  col  = "red",
  lwd  = 2
)
