##Data analysis: Extracting shark consumption record in Indonesia, from consumption ARTIS Database
## 4 April 2025
## Yuli, Modified from Rosa and Leslie


install.packages("readr")
install.packages("ggsankey")
install.packages("ggalluvial")



library(dplyr)
library(readr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(ggalluvial)
library(data.table)

cons.ID <- fread("clean/IDN_consumption_chondri.csv")


#visualize Indonesia trade volume and partner
#1 IDN as source + export
# - Create list of top 10 consumer countries 
top_10_consumer_list<- cons.ID %>% 
  filter(source_country_iso3c=="IDN") %>%
  group_by(consumer_iso3c) %>%
  summarise(cons_total_t = sum(consumption_live_t)) %>%
  arrange(desc(cons_total_t)) %>%
  slice(1:10) %>%
  pull(consumer_iso3c) #"IDN" "JPN" "USA" "SGP" "CHN" "KOR" "MYS" "THA" "TWN" "GBR"

# - calculate number of consumer country per year
country_count_cons <- cons.ID %>%
  filter(source_country_iso3c == "IDN") %>%
  select (consumer_iso3c, year, consumption_live_t) %>% 
  group_by(year) %>% 
  summarise (country_count = n_distinct(consumer_iso3c), tot_year = sum(consumption_live_t, na.rm = TRUE),
             .groups = "drop" )
glimpse(country_count)

# - visualize top 10 consumer countries label with country count on top of the bar
p.top.10.consumers <- cons.ID %>%
  mutate(consumer_grouped = ifelse(consumer_iso3c %in% top_10_consumer_list, 
                                   as.character(consumer_iso3c), "other")) %>%
  group_by(consumer_grouped, year) %>%
  summarise(tot_year = sum(consumption_live_t), .groups = "drop") %>%
  mutate(consumer_grouped = fct_relevel(consumer_grouped, top_10_consumer_list, after = 0)) %>% # Reorder based on top_10_consumer
  ggplot(aes(x = year, y = tot_year, fill = consumer_grouped)) +
  geom_bar(stat = "identity") +
  geom_text(data = country_count,
            aes(x = year, y = tot_year + 4000, label = country_count),
            inherit.aes = FALSE,
            size = 3.5)+
  scale_x_continuous(breaks = seq(1996, 2020, by = 4)) +
  labs(
    title = "Final Consumers of Shark and Ray Meat from Indonesia",
    x = "Year",
    y = "Volume (ton)",
    fill = "Final Consumer"
  ) +
  scale_fill_manual(values = my_colors) +
  theme_minimal()+
  theme(legend.position = "right")

ggsave("plot/p_top_10_consumers.jpg", plot = p.top.10.consumers, width = 8, height = 5, dpi = 300)


#2 Sankey plot: trade flow Indonesian shark meat source to final foreign consumer 2005 - 2020
# Source == IDN | exporter | consumer != IDN

consumer.flow <- cons.ID %>% 
  filter (source_country_iso3c == "IDN" , consumer_iso3c != "IDN", year <= 2020 & year >= 2005) %>% 
  select (source_country_iso3c, exporter_iso3c, consumer_iso3c, consumption_live_t) %>% 
  group_by(source_country_iso3c, exporter_iso3c, consumer_iso3c) %>% 
  summarise (consumption_t = sum(consumption_live_t)) %>% 
  ungroup() %>% 
  arrange (desc(consumption_t))
glimpse (consumer.flow)

# create list of top 10 exporter and consumer
# list of top 10 exporter
top10.exp.list <- consumer.flow %>% 
  group_by(exporter_iso3c) %>% 
  summarise (cons_tot = sum (consumption_t)) %>% 
  slice_max(order_by = cons_tot, n = 10) %>% 
  pull(exporter_iso3c) #"IDN" "CHN" "THA" "MYS" "TWN" "USA" "URY" "SGP" "VNM" "ESP"

# list of top 10 consumer
top10.cons.list <- consumer.flow %>% 
  group_by(consumer_iso3c) %>% 
  summarise (cons_tot = sum (consumption_t)) %>% 
  slice_max(order_by = cons_tot, n = 10) %>% 
  pull(consumer_iso3c) #"CHN" "JPN" "THA" "MYS" "SGP" "KOR" "USA" "TWN" "BRA" "VNM"

# ordering exporter country and consumer country based on largest weight
top10.exp.list.order <- c("IDN", "CHN", "THA", "MYS", "TWN", "USA", "URY", "SGP", "VNM", "ESP", "Others")
top10.cons.list.order <- c("CHN", "JPN", "THA", "MYS", "SGP", "KOR", "USA", "TWN", "BRA", "VNM", "Others")

#add others to the list of top 10, add into consumer.flow df
consumer.flow.df <- consumer.flow %>% 
  mutate(
    consumer_lump = if_else(consumer_iso3c %in% top10.cons.list, consumer_iso3c, "Others"),
    exporter_lump = if_else(exporter_iso3c %in% top10.exp.list, exporter_iso3c, "Others"),
    
    consumer_lump  = factor(consumer_lump,  levels = top10.cons.list.order),
    exporter_lump  = factor(exporter_lump,  levels = top10.exp.list.order),
    source_country_iso3c = factor(source_country_iso3c)
  ) %>% 
  group_by(source_country_iso3c, exporter_lump, consumer_lump) %>% 
  summarise(weight_t = sum(consumption_t), .groups = "drop") %>% 
  arrange(desc(weight_t))

summary(consumer.flow.df)
levels(consumer.flow.df$exporter_lump)

# visualize shark meat consumption flow source from IDN to final exporter to the final foreign consumer
p.consumer.flow <- ggplot (consumer.flow.df,
                   aes(axis1 = source_country_iso3c, axis2 = exporter_lump, axis3 = consumer_lump, y = weight_t)) +
  geom_alluvium(aes(fill = consumer_lump), width = 1/8, alpha = 0.8) +
  geom_stratum(width = 1/8, fill = "white", color = "grey40") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, color = "black") +
  scale_x_discrete(limits = c("Source", "exporter", "Consumer"), expand = c(.05, .05)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "exporter and consumer country of shark and ray meat source from ID, exclude domestic consumption",
       x = NULL,
       y = "Weight (tonnes)",
       fill = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 12)
  )

ggsave("plot/p_consumer_flow.jpg", plot = p.consumer.flow, width = 7, height = 8, dpi = 300)



#3 IDN as consumer 
# filter data frame (cons.ID) for consumer = IDN & Source!=IDN

# - Create list of top 10 source countries excluding IDN
top_10_source_list<- cons.ID %>% 
  filter(consumer_iso3c=="IDN" & source_country_iso3c !="IDN") %>%
  group_by(source_country_iso3c) %>%
  summarise(cons_total_t = sum(consumption_live_t)) %>%
  arrange(desc(cons_total_t)) %>% 
  slice(1:10) %>% 
  pull(source_country_iso3c) #<- "MYS" "JPN" "TWN" "ARG" "IND" "ESP" "THA" "AUS" "NZL" "PAK"

# - calculate number of consumer country per year
country_count_source <- cons.ID %>%
  filter(consumer_iso3c=="IDN" & source_country_iso3c !="IDN") %>%
  select (source_country_iso3c, year, consumption_live_t) %>% 
  group_by(year) %>% 
  summarise (country_count = n_distinct(source_country_iso3c), tot_year = sum(consumption_live_t, na.rm = TRUE),
             .groups = "drop" )
glimpse(country_count_source)

# - visualize top 10 source countries that Indonesia imported
p.top.10.source <- cons.ID %>%
  select (source_country_iso3c, consumer_iso3c, consumption_live_t, year) %>% 
  filter(consumer_iso3c=="IDN" & source_country_iso3c !="IDN") %>%
  mutate(source_grouped = ifelse(source_country_iso3c %in% top_10_source_list, 
                                   as.character(source_country_iso3c), "other")) %>% 
  group_by(source_grouped, year) %>%
  summarise(tot_year = sum(consumption_live_t), .groups = "drop") %>%
  mutate(source_grouped = fct_relevel(source_grouped, top_10_source_list, after = 0)) %>% # Reorder based on top_10_source_list
  ggplot(aes(x = year, y = tot_year, fill = source_grouped)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(1996, 2020, by = 4)) +
  labs(
    title = "Indonesia shark meat import",
    x = "Year",
    y = "Volume (ton)",
    fill = "Source country"
  ) +
  scale_fill_manual(values = my_colors) +
  theme_minimal()+
  theme(legend.position = "right")

ggsave("plot/p_top_10_source.jpg", plot = p.top.10.source, width = 8, height = 5, dpi = 300)

#4 Sankey plot: Trade flow of shark meat import to Indonesia 1996 - 2020
# Source != IDN | exporter | consumer == IDN

import.flow <- cons.ID %>% 
  filter (source_country_iso3c !="IDN" , consumer_iso3c == "IDN") %>% 
  select (source_country_iso3c, exporter_iso3c, consumer_iso3c, consumption_live_t) %>% 
  group_by(source_country_iso3c, exporter_iso3c, consumer_iso3c) %>% 
  summarise (consumption_t = sum(consumption_live_t)) %>% 
  ungroup() %>% 
  arrange (desc(consumption_t))
glimpse (import.flow)

# create list of top 10 source country and exporter
# list of top 10 source
top10.source.list.import <- import.flow %>%
  group_by(source_country_iso3c) %>% 
  summarise (total = sum (consumption_t)) %>% 
  slice_max(order_by = total, n = 10) %>% 
  pull(source_country_iso3c) #"MYS", "JPN", "TWN", "ARG", "IND", "ESP", "THA", "AUS", "NZL", "PAK" 

# list of top 10 exporter
top10.exp.list.import <- import.flow %>% 
  group_by(exporter_iso3c) %>% 
  summarise (total = sum (consumption_t)) %>% 
  slice_max(order_by = total, n = 10) %>% 
  pull(exporter_iso3c) # "MYS" "JPN" "SGP" "ARG" "TWN" "THA" "ESP" "AUS" "IND" "NZL"

# ordering exporter country and consumer country based on largest weight
top10.exp.list.import.order <- c("MYS", "JPN", "SGP", "ARG", "TWN", "THA", "ESP", "AUS", "IND", "NZL", "Others")
top10.source.list.import.order <- c("MYS", "JPN", "TWN", "ARG", "IND", "ESP", "THA", "AUS", "NZL", "PAK", "Others")

#add others to the list of the top10, add into import.flow df                                    
import.flow.df <- import.flow %>% 
  mutate (source_lump = if_else(source_country_iso3c %in% top10.source.list.import, source_country_iso3c, "Others"),
          exporter_lump = ifelse (exporter_iso3c %in% top10.exp.list.import, exporter_iso3c, "Others" ),
          source_lump = factor (source_lump, levels = top10.source.list.import.order),
          exporter_lump = factor (exporter_lump, levels = top10.exp.list.import.order),
          consumer_iso3c = as.factor (consumer_iso3c)) %>% 
  group_by(source_lump, exporter_lump, consumer_iso3c) %>% 
  summarise (weight_t = sum (consumption_t), .groups = "drop") %>% 
  arrange (desc(weight_t))

# visualize Indonesia shark meat import flow from source country and final exporter country

p.import.flow <- ggplot(import.flow.df,
              aes(axis1 = source_lump, axis2 = exporter_lump, axis3 = consumer_iso3c, y = weight_t)) +
  geom_alluvium(aes(fill = source_lump), width = 1/8, alpha = 0.8) +
  geom_stratum(width = 1/8, fill = "white", color = "grey40") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, color = "black") +
  scale_x_discrete(limits = c("Source", "exporter", "Consumer"), expand = c(.05, .05)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "foreign source and exporter country of shark consumed in Indonesia",
       x = NULL,
       y = "Weight (tonnes)",
       fill = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 12)
  )

ggsave("plot/p_import_flow.jpg", plot = p.import.flow, width = 7, height = 8, dpi = 300)

