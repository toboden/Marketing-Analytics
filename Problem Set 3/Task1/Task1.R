
library(reticulate)

# before using Python, create a virtual enviornment. Type the following in your terminal
# create venv
# Windows: python -m venv venv
# macOS & Linux: python3 -m venv venv
# activate venv
# Windows: venv\Scripts\activate
# macOS & Linux: source venv/bin/activate
# find a list of required packages in the requirements.txt

# song data: to save storage in GitHub we did not upload the songdata. Therefore,
# please download the data and use your own filepath to the data. 
# Add the filepath to audio_dir in line 31.

# integrate Python to R
#path_to_venv <- '/venv'
#use_virtualenv(path_to_venv, required = TRUE)
#py_config()


#py_run_string("
#print('Hello World')
#")
# Benötigte Pakete
library(readxl)
library(gamlr)
library(dplyr)

# 1. Excel-Datei einlesen
data <- read_excel("data/audio_features.xlsx")

# 2. Punkte extrahieren: wenn 'x', dann NA
data <- data %>%
  mutate(points = ifelse(grepl("_x\\.wav$", song),
                         NA,
                         as.numeric(sub(".*_(\\d+(\\.\\d+)?)\\.wav$", "\\1", song))))

# Tempo-Spalte bereinigen: eckige Klammern entfernen und numerisch machen
data <- data %>%
  mutate(tempo = as.numeric(gsub("\\[|\\]", "", tempo)))
# 3. Trainings- und Testdaten trennen
train_data <- data %>% filter(!is.na(points))
test_data <- data %>% filter(is.na(points))

# 4. Feature-Matrix und Zielvariable vorbereiten
x_train <- as.matrix(train_data %>% select(-song, -points))
y_train <- train_data$points

x_test <- as.matrix(test_data %>% select(-song, -points))

# 5. Lasso-Regression
set.seed(123) # für Reproduzierbarkeit
fit <- gamlr(x_train, y_train, standardize = TRUE, lambda.min.ratio = 1e-4)  

# K-Fold Cross-Validation (default: 10-fold)
cv_fit <- cv.gamlr(x_train, y_train, verb = TRUE, standardize = TRUE)

# Bestes Lambda basierend auf minimalem CV-Fehler
best_lambda <- cv_fit$lambda.min 

cat("Bestes Lambda aus CV:", best_lambda, "\n")

# 6. Vorhersage für den 101. Song
prediction <- predict(cv_fit$gamlr, newdata = x_test, lambda = best_lambda)
value <- as.numeric(prediction[1])
cat("Vorhergesagte Punkte für den Song:", value, "\n")
