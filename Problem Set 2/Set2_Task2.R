# It is important to install Matrix first, then lme4
#install.packages("Matrix", repos = "https://cloud.r-project.org", type = "binary")
#install.packages("lme4", repos = "https://cloud.r-project.org", type = "binary")
#install.packages("lmerTest", repos = "https://cloud.r-project.org", type = "binary")

library(lme4)
library(lmerTest)
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
  mutate(
    price_c = as.numeric(scale(pric, center = TRUE, scale = FALSE)), #main regressor
    quality_c = as.numeric(scale(qual, center = TRUE, scale = FALSE)), # Moderator 1
    cash_dummy = if_else(cash == 'c', 1, 0), # Moderator 2
    estb = as.factor(estb), # Moderator 3
  ) 

# additional steps for establishment
detailed_data_perep_estb = detailed_data_prep %>%
  filter(estb %in% c("s", "f", "sf"))


baseline_model = lmer(quan ~ price_c + (1|cusn), data = detailed_data_prep)
summary(baseline_model)

#2.1 Moderated Model 1
quality_model = lmer(quan ~ price_c + quality_c+ + (1|cusn), data = detailed_data_prep)
quality_model_moderation = lmer(quan ~ price_c*quality_c + (1|cusn), data = detailed_data_prep)
summary(quality_model)
summary(quality_model_moderation)

anova(quality_model, quality_model_moderation)
lrtest(quality_model, quality_model_moderation)

#2.2 Moderated Model 2
cash_model = lmer(quan ~ price_c+cash_dummy + (1|cusn), data=detailed_data_prep)
cash_model_moderation = lmer(quan ~ price_c*cash_dummy + (1|cusn), data=detailed_data_prep)
summary(cash_model)
summary(cash_model_moderation)

anova(cash_model, cash_model_moderation)
lrtest(cash_model, cash_model_moderation)

#2.3 Moderated Model 3
estb_model = lmer(quan ~ price_c + estb + (1|cusn), data = detailed_data_perep_estb)
estb_model_moderation = lmer(quan ~ price_c * estb + (1|cusn), data = detailed_data_perep_estb)
summary(estb_model)
summary(estb_model_moderation)

anova(estb_model, estb_model_moderation)
lrtest(estb_model, estb_model_moderation)



