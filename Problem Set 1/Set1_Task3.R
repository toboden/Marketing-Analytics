library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

#data loading
detailed_data = read_tsv("data/detailed_fish_market_data.txt")

#1 data preperation
detailed_data_prep <- detailed_data %>%
  filter(!is.na(pric),
         !is.na(quan),
         type == "w") %>%
  arrange(date) %>%
  
  # 1. 'cash' Dummy erstellen
  mutate(cash_dummy = if_else(cash == 'c', 1, 0)) %>%
  
  # 1.1 standardization
  group_by(cusn) %>%
  mutate(Qty_Dev = tots - mean(tots, na.rm = TRUE)) %>%
  ungroup() %>%
  
  # 1.2 mean centering
  mutate(
    price_c = scale(pric, center = TRUE, scale = FALSE),
    quality_c = scale(qual, center = TRUE, scale = FALSE), # Moderator 1
    #cash_c = scale(cash_dummy, center = TRUE, scale = FALSE) 
    dayw = as.factor(dayw)
  )


#2 Baseline Price-Demand function
baseline_model = lm(Qty_Dev ~ price_c, data = detailed_data_prep)
summary(baseline_model)

#3 Moderated Models
#3.1 Moderated Model 1
quality_model = lm(Qty_Dev ~ price_c*quality_c, data=detailed_data_prep)
summary(quality_model)

#3.2 Moderated Model 2
cash_model = lm(Qty_Dev ~ price_c*cash_dummy, data=detailed_data_prep)
summary(cash_model)

#3.3 Moderated Model 3
dayw_model = lm(Qty_Dev ~ price_c*dayw, data = detailed_data_prep)
summary(dayw_model)
#4 Comparison (F-Test and ANOVA)
