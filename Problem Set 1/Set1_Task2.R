# Read data
library(readr)
daily_data <- read_tsv("data/daily_fish_market_data.txt")
# Plot data to get understanding of it
plot(daily_data$price, daily_data$qty, xlab="Price", ylab="Quantity")
linear_reg = lm(daily_data$qty~ daily_data$price)
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
text(daily_data$price[top10],
     daily_data$qty[top10],
     labels = top10,
     pos = 3)   # Label über dem Punkt

# --> Interpretation:


# 1.2 Examine potential outliers in independent variable (price)
# Studentized residuals: Idea -> Use residuals to find outliers
# Step 1: 

# studentized (deleted) residuals
r_student <- rstudent(linear_reg)
top5 = order(r_student, decreasing = TRUE)[1:5]
low5 = order(r_student, decreasing = FALSE)[1:5]
plot(daily_data$price, daily_data$qty, xlab="Price", ylab="Quantity")
abline(linear_reg, col = "red", lwd = 2)
text(daily_data$price[top5],
     daily_data$qty[top5],
     labels = top5,
     pos = 3)

# Labels für low 5
text(daily_data$price[low5],
     daily_data$qty[low5],
     labels = low5,
     pos = 3)

#--> Interpretation

# 1.3 Examine potential outliers considering dependent and independent variable
# Cooks Distance: Looks for observaction

# Annahme: linear_reg existiert (linear_reg <- lm(qty ~ price, data = daily_data))

# 1) Cook's distance mit eingebauter Funktion
D <- cooks.distance(linear_reg)

# Ausgabe: erste Werte
head(D)

# 2) Indizes der 10 größten Cook's distances
top10_D <- order(D, decreasing = TRUE)[1:10]
top10_D
D[top10_D]

# 4) Plot: Punkte proportional zu Cook's D und Top-10 labeln
plot(daily_data$price, daily_data$qty,
     xlab = "Price", ylab = "Quantity",
     main = "Price vs Quantity (Cook's distance hervorgehoben)")

abline(linear_reg, col = "red", lwd = 2)


# Top-10 labeln
text(daily_data$price[top10_D],
     daily_data$qty[top10_D],
     labels = top10_D,
     pos = 3, cex = 0.8, col = "blue")

# 5) (Optional) Vergleich mit der Folienformel:
# Folie: D_i = r_i*^2 * h_i / (2 s^2 (1 - h_i))
r_student <- rstudent(linear_reg)   # extern studentized residuals (r_i^*)
h <- hatvalues(linear_reg)
s <- summary(linear_reg)$sigma      # Residual standard error
D_slide <- (r_student^2 * h) / (2 * s^2 * (1 - h))  # nur, wenn die Folie p=2 impliziert
# Vergleiche Maximum / Differenzen
head(D_slide)

max_diff <- max(abs(D - D_slide))
cat("Max Differenz zwischen cooks.distance() und Folienformel:", max_diff, "\n")
# Werte sind komplett anders. Also stimmt hier irgendwas nicht.
# --> Interpretation: should we delete outliers?


# 2. Compare linear, polynomial and logarithmic functional forms
# 2.1 Define hypothesized functional form (linear, polynomial and logarithmic)
# 2.2 Look at scatterplot of data
# 2.3 Linear regression
# 2.4 Polynomial regression
# 2.4.1 Mean centering
# 2.4.2 Compute polynomials
# 2.4.3 Analyze and test hypothesized model
# 2.4.4 if tests are successful: Analyze extended model
# 2.5 logarithmic transformation
# be careful of 0s 