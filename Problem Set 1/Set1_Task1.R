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
         type == "w")






# boxplot tots(total sales) nach dayw(Day of the Week)

# heatmap stunde x wochentag für mittleren preis?





# sell_through = tots/totr vs. dayw 



# verteilung von qual über den Tag

# standard deviation von price für qualities 1-5?

# analyse nach customer machen also für jeden cusn (z.b.1) alle einträge von quant addieren und zwischen allen customern vergleichen
customer_stats <- whiting |>
  filter(!is.na(cusn,ethn), !is.na(quan)) |>
  group_by(cusn) |>
  summarise(
    n_visits     = n(),                        # wie oft kam der Kunde
    total_quan   = sum(quan),                  # insgesamt gekaufte Menge
    avg_quan     = mean(quan),                 # Ø-Menge pro Einkauf
    avg_price    = mean(pric, na.rm = TRUE),   # einfacher Ø-Preis
    .groups      = "drop"
  ) |>
  arrange(desc(total_quan))


# 

