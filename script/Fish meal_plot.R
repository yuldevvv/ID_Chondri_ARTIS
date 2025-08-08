## Fish meal
#dari mana IDN mengimport fishmeal? --> list negara, grafik x = negara y= volume


library(tidytext)
library(here)
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(data.table)

options(scipen = 99999)  # Set a high value to disable scientific notation

#load data
trade.ID <- fread("clean/IDN_trade_chondri.csv")
glimpse (trade.ID)

## add new field to differentiate fishmeal product vs human consumption
trade.ID.product <- trade.ID %>% 
  mutate(product = ifelse(hs6 == 230120, "Fish Meal", "Human Consumption"))
glimpse (trade.ID.product)
fwrite(trade.ID.product, here("clean/IDN_trade_product.csv"))

## total volume Fish meal, source, domestic consumption, direct export, re-export, import
# 1. Direct export
FM.direct.export <- trade.ID.product %>% 
  filter (product == "Fish Meal", dom_source == "domestic", source_country_iso3c == "IDN")  
sum (FM.direct.export$live_weight_t) # 2214.126 tonnes

# 2. re-exporter
FM.re.exporter <- trade.ID.product %>% 
  filter (product == "Fish Meal", dom_source == "foreign", exporter_iso3c == "IDN")  
sum (FM.re.exporter$live_weight_t) # 436.08 tonnes

# 3. source
FM.source <- trade.ID.product%>% 
  filter (product == "Fish Meal", source_country_iso3c == "IDN" )
sum (FM.source $live_weight_t) #7185.099 tonnes

# 4. Import
FM.import <- trade.ID.product%>% 
  filter (product == "Fish Meal", importer_iso3c == "IDN" )
sum (FM.import $live_weight_t) #19901.29 tonnes


# Line graph consist of 3 lines: trade of IDN fishmeal
# 1. create dataframe --> combine

#1. Fish meal source dataframe + add field " type " to assign as source
FM.source <- FM.source %>% 
  group_by(year) %>% 
  summarise(Volume_t = sum(live_weight_t)) %>% 
  mutate(type = "source")
glimpse (FM.source)

#2. Fish meal export dataframe + add field " type " to assign as export
FM.export <- trade.ID.product %>% 
  filter(exporter_iso3c == "IDN" , product == "Fish Meal") %>% 
  group_by(year) %>% 
  summarise(Volume_t = sum(live_weight_t)) %>% 
  mutate(type = "export")
glimpse (FM.export)

#3. Fish meal import dataframe + add field " type " to assign as import
FM.import <- FM.import %>% 
  group_by(year) %>% 
  summarise(Volume_t = sum(live_weight_t)) %>% 
  mutate(type = "import")
glimpse (FM.import)

#4. combined 3 dataframe
FM.lines <-  bind_rows(FM.source, FM.export, FM.import)
glimpse (FM.lines)

## visualize into line graph, DF = FM.lines
P.FM.lines <- ggplot(FM.lines, aes(x = year, y = Volume_t, color = type)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Indonesia Shark and Ray Meat Trade for Fishmeal (1996 - 2020)",
    x = "Year",
    y = "Volume (tonnes)",
    color = "Category"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

ggsave("plot/P_FM_lines.jpg", plot = P.FM.lines, width = 8, height = 5, dpi = 300)


# IDN import fish meal looks significant, create the bar chart for imported fish meal vs human consumption
P.prod.import <- trade.ID.product %>% 
  filter (importer_iso3c == "IDN") %>%
  select (year, live_weight_t, product) %>% 
  group_by(year, product) %>% 
  summarise (volume_t = sum(live_weight_t, na.rm = TRUE), .groups = "drop")
  ggplot(aes(x = as.factor(year), y = volume_t, fill = product)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Indonesia's import: chondrichtyhyes products fish meal vs human consumption", x = "Year", y = "Total Live Weight (t)", fill = "product") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
# volume between 
ggsave("plot/p_prod_import.jpg", plot = P.prod.import, width = 8, height = 5, dpi = 300)

# dataframe for IDN imported product.
prod.import <- trade.ID.product %>% 
  filter (importer_iso3c == "IDN") %>%
  select (year, live_weight_t, product) %>% 
  group_by(year, product) %>% 
  summarise (volume_t = sum(live_weight_t, na.rm = TRUE), .groups = "drop")
sum(prod.import$volume_t[prod.import$product == "Fish Meal"], na.rm = TRUE) #19901.29
sum(prod.import$volume_t[prod.import$product == "Human Consumption"], na.rm = TRUE) #22787.84
# Human consumption > Fish Meal

fwrite(prod.import, here("clean/prod_import.csv"))
