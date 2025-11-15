library(readr)
library(dplyr)
library(ggplot2)

detailed <- read_tsv("data/detailed_fish_market_data.txt")


# delete those rows that have NA for 
# "price","quan", "totr", "tots" and 
# filter for whiting (no king)
detailed_whiting <- detailed %>%
  filter(!is.na(pric),
         !is.na(quan),
         !is.na(totr),
         !is.na(quan),
         type == "w") %>%
  arrange(date)


# There seem to be two entries in the dataset, where there are two dealer per day.
# Since this is the case only for two out of all days in April and May: drop those two observations 
detailed_whiting <- detailed_whiting %>%
  # frequency of the same tots value for different days
  group_by(date, dayw, tots) %>%
  mutate(n_same_tots = n()) %>%
  
  # number of distinct tots days
  group_by(date, dayw) %>%
  mutate(n_tots_values = n_distinct(tots)) %>%
  ungroup() %>%
  
  # delete rows for which (there are multiple different tots values 
  #                                   AND 
  #                       for which the tot value only appears once)
  filter(!(n_tots_values > 1 & n_same_tots == 1)) %>%
  
  # delete rows that are not longer needed
  select(-n_same_tots, -n_tots_values)


## two cases, where > 1 dealer is present
# tots_inconsistent <- detailed_whiting |>
#   group_by(date, dayw) |>
#   mutate(
#     n_tots = n_distinct(tots)
#   ) |>
#   filter(n_tots > 1) |>
#   arrange(date, dayw, tots, totr)





##----- 
# Descriptive analysis
##-----


# theme for plots
theme_fontsize <- theme(
  plot.title = element_text(size = 14),
  plot.subtitle = element_text(size = 10),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 11),
  legend.text = element_text(size = 11),
)

## Analysis on daily basis -----
# dataset for the daily-level
detailed_whiting_daily <- detailed_whiting %>%
  group_by(date) %>%
  summarise(
    avg_pric = mean(pric),
    totr = first(totr),
    tots = first(tots),
    dayw = first(dayw),
    n_trsact = n(),
    strate = first(tots)/first(totr)
  )




# boxplot tots(total sales) by dayw (Day of the Week)
ggplot(detailed_whiting_daily, 
       aes(x=factor(dayw), y=tots)) +
  geom_boxplot(fill = "lightblue",         # dezente Farbe
               colour = "grey20",
               width = 0.6,
               outlier.colour = "firebrick",
               outlier.alpha = 0.7,
               outlier.size = 2) + 
  labs(
    x = NULL,
    y = "Total sales",
    title = "Distribution of Total Sales per Weekday",
    subtitle = "Whiting sales, April-May 1992"
  ) +
  scale_x_discrete(breaks = seq(1,5,by=1),
                   labels = c("Mon", "Tue", "Wed", "Thu", "Fri")) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme_fontsize
  



#### price

# boxplot tots(total sales) by dayw (Day of the Week)
ggplot(detailed_whiting, 
       aes(x=factor(dayw), y=pric)) +
  geom_boxplot(fill = "lightblue",         # dezente Farbe
               colour = "grey20",
               width = 0.6,
               outlier.colour = "firebrick",
               outlier.alpha = 0.7,
               outlier.size = 2) + 
  labs(
    x = NULL,
    y = "Price per pound",
    title = "Distribution of Price per Weekday",
    subtitle = "Whiting sales, April-May 1992"
  ) +
  scale_x_discrete(breaks = seq(1,5,by=1),
                   labels = c("Mon", "Tue", "Wed", "Thu", "Fri")) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme_fontsize


# number of sales per hour distribution density


# sell_through = tots/totr vs. distinct day AND Tagesvolumen (relativ?)






# heatmap stunde x wochentag für mittleren preis?








# verteilung von qual über den Tag

# standard deviation von price für qualities 1-5?





