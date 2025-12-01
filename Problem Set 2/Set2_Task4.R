# erste note: der Datensatz wurde so mit statistischem Rauschen und 
# scaling facotrs verändert, dass die absolute höhe nicht den 
# wahren effekten entsprechen. Allerdings bleiben alle statistischen Beziehungen erhalten.
# das ist wichtig im Hinterkopf zu behalten bei der statistischen Analyse. Die Signifikanz, Richung und ökonomische Bedeutung
# der Regressionkoeffizienten bleibt erhalten, die absolute Höhe ist aber nicht verlässlich interpretierbar.

# Zu meinem Vorgehen in der Aufgabe. Da in der Aufgabenbeschreibung explizit gefordert ist, basierend auf der eigenen ökonomischen 
# Einschätzung drei Variablen auszuwählen, und das Logistische Regressionsmodell basierend auf diesen Variablen zu interpretieren
# wird dies auch so durchgeführt. Wäre die Aufgabenstellung offener gestaltet, hätte ich ein Modell mit allen Regressoren gefitted
# und mit einem Lasso-Modell verglichen. Eine weitere Idee wäre es, mittel Train und Test split die Feature-Selection durchzuführen,
# was bei der relativ geringen Datenmenge aber mit Vorsicht zu genießen ist. 









# choose predictors

# Die Idee ist es die drei Predictors so auszuwäheln, dass sie möglichst keine inhaltlichen Überschneidungen haben, was 
# gegebenenfalls zu redundanten Variablen im Modell führen könnte.

# - change sales_value_total: guter Indikator über verschiedene Produktkategorien hinweg? da value unabhängig von 
# unabhängig von wert des einzelnen verkauften produkts ist
#
# nieddrige Umsätze klassisch indikator für schließungen => ist zu erwarten, dass in den counties, wo umsatz 
# über längeren Zeitraum niedrig ist zuerst geschlossen wird

# damit niedrige Umsätze erkannt werden (kann sein, dass store generell e.g. kleiner ist und geringeres sales level hat), 
# wird hier change in sales verwendet. Allerdings ist es robuster den change in kontext eines 3 monats trends zu sehen. es kann durchaus sein, 
# dass auf monatlicher ebene zufällige schwankungen herrschen oder aus völlig unbedenklichen gründen die sales kurzfristig einbrechen.
# e.g. viele Feiertage oder produktknappheit.

# - pct_online_sales: zu erwarten, dass stores mit hohem anteil an online sales unprofitabel werden und
# potenziell geschlossen werden


# - pct_discounts: falls kunden vermehrt mit rabatten in stores gelockt werden müssen (schwache lokale nachfrage), 
# kann das hinweis darauf sein, dass store schwaches preissetzungsvermögen hat und ggfs. bald schließen muss

# changes wären generell besser, da discounts auch preisstrategie sein können. allerdings wird diese variable nicht auf dem
# monatlichen level reported und kann daher nicht zu relative change umgewandelt werden.


# weiterer möglicher predictor wäre
# return rate Qualitäts oder Sortimentsprobleme? Aber nicht genommen, da potenziell Produkte eines Herstellers über verschiedene Counties
# in den Stores identisch sind.






#################################################################################

# daten filtern! nur<= 6, da sonst 
# inforation leakage: die independent variablen werden durch die store closure potenziell beeinflusst und zum zeitpunkt 
# month > 6 ist bereits bekannt ob treat = 1 oder 0. 

set.seed(1)

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(broom)

store_df <- read.csv("data/StoreData.csv")

store_df_pre_treat <- store_df %>%
  filter(month <= 6)


# create relative three month change predictor for sales_value_total 
store_df_pre_treat <- store_df_pre_treat %>%
  group_by(county_id) %>%
  mutate(
    sales_early = mean(sales_value_total[month %in% 1:3], na.rm = TRUE),
    sales_late  = mean(sales_value_total[month %in% 4:6], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    sales_trend_3m = (sales_late - sales_early) / sales_early
  ) %>%
  select(-sales_late, -sales_early)

store_df_pre_treat_3selected <- store_df_pre_treat %>%
  select(county_id, month, treat, sales_trend_3m, pct_online_sales, pct_discounts)

store_df_pre_treat_3selected <- store_df_pre_treat_3selected %>%
  group_by(county_id, treat) %>%
  distinct(pct_online_sales, pct_discounts, sales_trend_3m)



# logit modell, multikollinearität nicht wichtig zu prüfen für prediction 
# "Multicollinearity inflates variances of individual coefficients,
# but has little effect on prediction accuracy" Hastie, Tibshirani & Friedman – The Elements of Statistical Learning Kap 3,4
vars <- c("pct_online_sales", "pct_discounts", "sales_trend_3m")

logit_models_list <- unlist(
  lapply(0:length(vars), function(k) {
    if (k == 0) {
      "1"                               # Intercept only
    } else {
      combn(vars, k, FUN = function(x) paste(x, collapse = " + "))
    }
  }),
  use.names = FALSE
)

# Daraus Formeln machen: treat ~ ...
logit_models_formulas <- lapply(logit_models_list, function(rhs) {
  as.formula(paste("treat ~", rhs))
})

logit_model_results <- map_dfr(logit_models_formulas, function(f) {
  
  m <- glm(f,
           data = store_df_pre_treat_3selected,
           family = binomial(link = "logit"))
  
  prediction <- predict(m, type = "response")
  prediction <- if_else(prediction > 0.5, 1L, 0L)
  
  class_accuracy <- mean(prediction == store_df_pre_treat_3selected$treat)
  
  
  tibble(
    formula = deparse(f),
    aic     = AIC(m),
    accuracy = class_accuracy,
    model = list(m)
  )
})


logit_model_results <- logit_model_results %>%
  arrange(aic)

# perform likelihood ratio test  for all fitted models
logL_null_model <- as.numeric(logLik(logit_model_results %>% 
                                       filter(formula == "treat ~ 1") %>% 
                                       pull(model) %>% 
                                       .[[1]]))

LR_test_tibble <- tibble(
  formula = character(),
  LR_stc = numeric(),
  p_value = numeric()
)

for (i in 1:length(logit_model_results$formula)) {
  
  current_model <- logit_model_results$model[[i]]
  
  test_statistic <- -2*(logL_null_model-as.numeric(logLik(current_model)))
  df_model <- attr(logLik(current_model), "df")
  p_v <- 1-pchisq(test_statistic,df_model - 1)
  
  LR_test_tibble <- LR_test_tibble %>%
    add_row(formula = logit_model_results$formula[[i]],
            LR_stc = test_statistic,
            p_value = p_v)
    
}

logit_model_results <- left_join(logit_model_results, LR_test_tibble, by = "formula")




## Classification accuracy -----
# Classification accuracy was computed in-sample, as prediction on new, 
# unseen data was not required by the assignment and the dataset is small. 
# Therefore, the accuracy should not be interpreted as an out-of-sample predictive 
# performance measure but rather as a descriptive indicator of model fit.


