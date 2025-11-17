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
  
  # 1.1 standardization
  group_by(cusn) %>%
  mutate(Qty_Dev = quan - mean(quan, na.rm = TRUE)) %>% #target variable
  ungroup() %>%
  
  # 1.2 mean centering and dummy creation
  mutate(
    price_c = as.numeric(scale(pric, center = TRUE, scale = FALSE)), #main regressor
    quality_c = as.numeric(scale(qual, center = TRUE, scale = FALSE)), # Moderator 1
    cash_dummy = if_else(cash == 'c', 1, 0), # Moderator 2
    estb = as.factor(estb), # Moderator 3
  ) 
# additional steps for establishment
detailed_data_prep %>% group_by(estb) %>% count()

detailed_data_perep_estb = detailed_data_prep %>%
  filter(estb %in% c("s", "f", "sf"))


#2 Baseline Price-Demand function
baseline_model = lm(Qty_Dev ~ price_c, data = detailed_data_prep)
summary(baseline_model)

#3 Moderated Models
#2.1 Moderated Model 1
quality_model = lm(Qty_Dev ~ price_c + quality_c, data = detailed_data_prep)
quality_model_moderation = lm(Qty_Dev ~ price_c*quality_c, data = detailed_data_prep)
summary(quality_model)
summary(quality_model_moderation)

anova(quality_model, quality_model_moderation)
lrtest(quality_model, quality_model_moderation)

#2.2 Moderated Model 2
cash_model = lm(Qty_Dev ~ price_c+cash_dummy, data=detailed_data_prep)
cash_model_moderation = lm(Qty_Dev ~ price_c*cash_dummy, data=detailed_data_prep)
summary(cash_model)
summary(cash_model_moderation)

anova(cash_model, cash_model_moderation)
lrtest(cash_model, cash_model_moderation)

#2.3 Moderated Model 3
#2.3 Moderated Model 3
estb_model = lm(Qty_Dev ~ price_c + estb, data = detailed_data_perep_estb)

