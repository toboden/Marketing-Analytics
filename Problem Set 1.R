library(readr)
library(dplyr)
library(ggplot2)

daily <- read_tsv("data/daily_fish_market_data.txt")
detailed <- read_tsv("data/detailed_fish_market_data.txt")


ggplot(daily, aes(x = price)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Verteilung der Tagespreise")

ggplot(detailed, aes(x = pric)) +
  geom_histogram(bins = 30, fill = "darkgreen") +
  labs(title = "Verteilung der Einzelhandelspreise")
