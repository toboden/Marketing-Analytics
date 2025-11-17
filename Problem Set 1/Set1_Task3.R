library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lmtest)

#data loading
detailed_data = read_tsv("data/detailed_fish_market_data.txt")


#1 data preperation
detailed_data_prep <- detailed_data %>%
  filter(!is.na(pric),
         !is.na(quan),
         type == "w") %>%
  arrange(date) %>%
  
  # 1. Dummies erstellen
  mutate(cash_dummy = if_else(cash == 'c', 1, 0),
         phon = as.numeric(phon)) %>%
  
  # 1.1 standardization
  group_by(cusn) %>%
  mutate(Qty_Dev = quan - mean(quan, na.rm = TRUE)) %>%
  ungroup() %>%
  
  # 1.2 mean centering
  mutate(
    price_c = as.numeric(scale(pric, center = TRUE, scale = FALSE)),
    quality_c = as.numeric(scale(qual, center = TRUE, scale = FALSE)), # Moderator 1
    #cash_c = scale(cash_dummy, center = TRUE, scale = FALSE) 
    dayw = as.factor(dayw),
    estb = as.factor(estb),
    ethn = as.factor(ethn)
  ) 
# additional steps for establishment
detailed_data_prep %>% group_by(estb) %>% count()

detailed_data_perep_estb = detailed_data_prep %>%
  filter(estb %in% c("s", "f", "sf"))


#2 Baseline Price-Demand function
baseline_model = lm(Qty_Dev ~ price_c, data = detailed_data_prep)
summary(baseline_model)

#3 Moderated Models
#3.1 Moderated Model 1
estb_model = lm(Qty_Dev ~ price_c + estb, data = detailed_data_perep_estb)
estb_model_moderation = lm(Qty_Dev ~ price_c * estb, data = detailed_data_perep_estb)
summary(estb_model)
summary(estb_model_moderation)

anova(estb_model, estb_model_moderation)
lrtest(estb_model, estb_model_moderation)

#3.2 Moderated Model 2
cash_model = lm(Qty_Dev ~ price_c+cash_dummy, data=detailed_data_prep)
cash_model_moderation = lm(Qty_Dev ~ price_c*cash_dummy, data=detailed_data_prep)
summary(cash_model)
summary(cash_model_moderation)

anova(cash_model, cash_model_moderation)
lrtest(cash_model, cash_model_moderation)

#3.3 Moderated Model 3
phon_model = lm(Qty_Dev ~ price_c + phon, data = detailed_data_prep)
phon_model_moderation = lm(Qty_Dev ~ price_c*phon, data = detailed_data_prep)
summary(phon_model)
summary(phon_model_moderation)

anova(phon_model, phon_model_moderation)
lrtest(phon_model, phon_model_moderation)

