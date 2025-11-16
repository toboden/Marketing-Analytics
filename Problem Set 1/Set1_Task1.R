library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

detailed <- read_tsv("data/detailed_fish_market_data.txt")


# delete those rows that have NA for 
# "price","quan", "totr", "tots" and 
# filter for whiting (no king)
detailed_whiting <- detailed %>%
  filter(!is.na(pric),
         !is.na(quan),
         !is.na(totr),
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
tots_inconsistent <- detailed_whiting |>
  group_by(date, dayw) |>
  mutate(
    n_tots = n_distinct(tots)
  ) |>
  filter(n_tots > 1) |>
  arrange(date, dayw, tots, totr)





##------------------------------------------------------------------------------ 
# Descriptive analysis
##-----------------------------------------------------------------------------


# theme for plots
theme_fontsize <- theme(
  plot.title = element_text(size = 14),
  plot.subtitle = element_text(size = 10),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 11),
  legend.text = element_text(size = 11),
)



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

# labels for time series data plots
date_seq <- seq(
  from = as.Date("1992-04-06"),
  to   = as.Date("1992-05-15"),
  by   = "day"
)

# format as "MM-YYYY"
day_labels <- format(date_seq, "%d-%m")

# Named character vector: names are month_ids
day_lookup_vec <- setNames(day_labels, c(seq(406,430, by = 1),seq(501,515, by=1)))

break_vec_x_axis <- c(seq(406,430, by = 7),seq(504,515, by=7))
all_days_x_axis <- c(seq(406,430, by = 1),seq(501,515, by=1))



####
# summary of the daily datset
####
detailed_whiting_daily %>%
  select(totr, tots, n_trsact) %>%
  summary()

## sales analysis--------------------------------------------------------------

####
# barchart average sales by dayw (Day of the Week)
####
detailed_whiting_daily %>%
  group_by(dayw) %>%
  summarise(avg_tots = mean(tots, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(dayw), y = avg_tots)) +
  geom_col(fill = "lightblue", colour = "grey20", width = 0.4) + 
  labs(title = "Average Total Sales by Weekday",
       subtitle = "Whiting sales, April-May 1992",
       y = "Sales (lbs)",
       x = NULL)+
  geom_text(
    aes(label = scales::comma(round(avg_tots, 0))),
    vjust = -0.3,
    size = 3
  ) +
  scale_x_discrete(breaks = 1:5,
                   labels = c("Mon", "Tue", "Wed", "Thu", "Fri")) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme_fontsize


####
# time series of tots (total sales)
####
tots_plot_df <- detailed_whiting_daily %>%
  select(date, tots) %>%
  complete(date = 406:515,
           fill = list(tots = 0)) %>%
  arrange(date) %>%
  filter(!between(date, 431, 500)) %>%
  mutate(date_fac = factor(date, levels = date))


ggplot(tots_plot_df, aes(x=date_fac, y = tots, group = 1)) + 
  geom_col(width = 0.2,
           colour = "lightblue",
           fill="grey10") +
  labs(title = "Daily Total Sales over Time",
       subtitle = "Whiting sales, April-May 1992",
       y = "Sales (lbs)",
       x = NULL)+ 
  scale_x_discrete(breaks = as.character(all_days_x_axis),
                   labels = function(x) {
                     lab <- rep("", length(x))
                     sel <- x %in% as.character(break_vec_x_axis)
                     lab[sel] <- day_lookup_vec[x[sel]]
                     lab
                   }) +
  theme_bw() +
  theme_fontsize



####
# time series of str (sell trough rate)
####
str_plot_df <- detailed_whiting_daily %>%
  select(date, strate) %>%
  complete(date = 406:515,
           fill = list(strate = 0)) %>%
  arrange(date) %>%
  filter(!between(date, 431, 500)) %>%
  mutate(date_fac = factor(date, levels = date))


ggplot(str_plot_df, aes(x=date_fac, y = strate, group = 1)) + 
  geom_line(size = 0.7,
            colour = "grey20") +
  geom_point(
    data   = subset(str_plot_df, strate > 0),
    size   = 3,
    colour = "grey10"
  ) +
  geom_abline(intercept = 1, 
              slope = 0, 
              linetype = "dashed",
              size = 0.8,
              colour = "skyblue") + 
  labs(title = "Daily Sell-Through Rate over Time",
       subtitle = "Whiting sales, April-May 1992",
       y = "STR",
       x = NULL)+
  scale_y_sqrt(breaks = c(0, 0.5, 1, 2.5, 5, 10)) + 
  scale_x_discrete(breaks = as.character(all_days_x_axis),
                   labels = function(x) {
                     lab <- rep("", length(x))
                     sel <- x %in% as.character(break_vec_x_axis)
                     lab[sel] <- day_lookup_vec[x[sel]]
                     lab
                   }) +
  theme_bw() +
  theme_fontsize
  

## price analysis----------------------------------------------

####
# boxplot pric(price) by dayw (Day of the Week)
####
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
    y = "Price (per lbs)",
    title = "Price Distribution by Weekday",
    subtitle = "Whiting sales, April-May 1992"
  ) +
  scale_x_discrete(breaks = seq(1,5,by=1),
                   labels = c("Mon", "Tue", "Wed", "Thu", "Fri")) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme_fontsize


####
# boxplot pric(price) by qual (Qualitiy of the fish)
####
detailed_whiting %>%
  filter(!is.na(qual)) %>%
  ggplot(aes(x=factor(qual), y=pric)) +
  geom_boxplot(fill = "lightblue",
               colour = "grey20",
               width = 0.6,
               outlier.colour = "firebrick",
               outlier.alpha = 0.7,
               outlier.size = 2) + 
  labs(
    x = NULL,
    y = "Price (per lbs)",
    title = "Price Distribution by Whiting Quality",
    subtitle = "Whiting sales, April-May 1992"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme_fontsize


####
# correlation between tots (total dailiy sales) and avg_pric (average price)
####
cor(detailed_whiting_daily$avg_pric,detailed_whiting_daily$tots)


## Analysis based on hour of the day --------------------------------------
hourly_detailed_whiting <- detailed_whiting %>%
  mutate(time = round(time/100, digits = 0))

# number of transactions per hour 
hourly_detailed_whiting %>%
  group_by(time) %>%
  summarise(n_sales = n()) %>%
  filter(!is.na(time)) %>%
  ggplot(aes(x = factor(time), y = n_sales)) +
  geom_col(fill = "lightblue", colour = "grey20", width = 0.4) + 
  labs(title = "Number of Transactions per Hour",
       subtitle = "Whiting sales, April-May 1992",
       y = "Transactions",
       x = NULL)+
  geom_text(
    aes(label = scales::comma(round(n_sales, 0))),
    vjust = -0.3,
    size = 3
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme_fontsize
  

# average price across hours
hourly_detailed_whiting %>%
  group_by(time) %>%
  summarise(avg_pric = mean(pric, na.rm = TRUE)) %>%
  filter(!is.na(time)) %>%
  ggplot(aes(x = factor(time), y = avg_pric)) +
  geom_col(fill = "lightblue", colour = "grey20", width = 0.4) + 
  labs(title = "Average Price per Hour",
       subtitle = "Whiting sales, April-May 1992",
       y = "Price (per lbs)",
       x = NULL)+
  geom_text(
    aes(label = scales::comma(round(avg_pric, 2))),
    vjust = -0.3,
    size = 3
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme_fontsize







