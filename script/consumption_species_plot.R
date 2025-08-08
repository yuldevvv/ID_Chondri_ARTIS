List of species / species group that Indonesia consumed or caught
data: consumption ARTIS Database
8/04/2025
yuli



library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(data.table)
install.packages("patchwork")
library(patchwork)

cons.ID <- fread("clean/IDN_consumption_chondri.csv")
options(scipen = 99999)  # Set a high value to disable scientific notation


#create 3 plots for species composition and then combine
1. bar plot annual species composition over time, source
2. bar plot annual species composisiton over time, export
3. bar plot annual species composition over time, import
4. combined all

#1 create dataframe for species source per year. order the sciname based based on highest volume_t

sp.source <- cons.ID %>% 
  filter (source_country_iso3c == "IDN") %>% 
  select (sciname, sciname_hs_modified, consumption_live_t, year) %>%  
  group_by(sciname, year) %>% 
  summarise(volume_t = sum(consumption_live_t), .groups = "drop") %>% 
  mutate(sciname = fct_reorder(sciname, volume_t, .fun = sum, .desc = TRUE)) %>% 
  arrange(year)
glimpse(sp.source)

# - color pallete for 14 sciname
source.sp.colors <- c(
  "elasmobranchii"         = "#e41a1c", # red
  "rajiformes"             = "#377eb8", # blue
  "carcharhinidae"         = "#4daf4a", # green
  "squalidae"              = "#984ea3", # purple
  "sphyrnidae"             = "#ff7f00", # orange
  "lamnidae"               = "#dede00", # softer yellow
  "dasyatidae"             = "#ffb000", # gold-orange
  "myliobatidae"           = "#f781bf", # pink
  "mobulidae"              = "#1b7f6b", # teal
  "rhinobatidae"           = "#a6611a", # warm earthy orange-brown
  "pristidae"              = "#66c2a5", # medium aqua
  "alopias"                = "#cab2d6", # muted lavender
  "prionace glauca"        = "#b2df8a", # soft lime green
  "rhynchobatus australiae"= "#fb9a99"  # soft coral
)



# - create bar plot for species composition per year. legend order based on highest volume_t
p.sp.source <- ggplot(sp.source, aes(x = year, y = volume_t, fill = sciname)) +
  geom_bar (stat = "identity", position = "stack") +
  scale_fill_manual (values = source.sp.colors) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1/1000),
    name = "Live Weight (1000 tonnes)") +
  labs (title = "a. Source",
        x = "year",
        fill = "species group") +
  guides(fill = guide_legend(title.position = "top")) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.line = element_line (color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(size = 11, hjust = 0.5),
    plot.title = element_text(face = "bold", hjust = 0)
  )

ggsave("plot/p_sp_source.jpg", plot = p.sp.source, width = 7, height = 8, dpi = 300)


#2 create dataframe for species export per year. order the sciname based on highest volume_t
sp.export <- cons.ID %>% 
  filter (source_country_iso3c == "IDN" & consumer_iso3c != "IDN") %>% 
  select (sciname, sciname_hs_modified, consumption_live_t, year) %>% 
  group_by(sciname, year) %>% 
  summarise(volume_t = sum(consumption_live_t), .groups = "drop") %>% 
  arrange(year) %>% 
  mutate(sciname = fct_reorder(sciname, volume_t, .fun = sum, .desc = TRUE))
glimpse(sp.export)

# - create bar plot for species composition per year. legend order based on highest volume_t
p.sp.export <- ggplot(sp.export, aes(x = year, y = volume_t, fill = sciname)) +
  geom_bar (stat = "identity", position = "stack") +
  scale_fill_manual (values = source.sp.colors) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1/1000),
    name = "Live Weight (1000 tonnes)") +
  labs (title = "b. export",
        x = "year",
        fill = "species group") +
  guides(fill = guide_legend(title.position = "top")) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.line = element_line (color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(size = 11, hjust = 0.5),
    plot.title = element_text(face = "bold", hjust = 0)
  )

ggsave("plot/p_sp_export.jpg", plot = p.sp.export, width = 7, height = 8, dpi = 300)


#3 create dataframe for species import per year. order the sciname based on highest volume_t
# filter consumer = IDN & Source!=IDN

sp.import <- cons.ID %>% 
  filter (source_country_iso3c != "IDN" & consumer_iso3c == "IDN") %>% 
  select (sciname, sciname_hs_modified, consumption_live_t, year) %>% 
  group_by(sciname, year) %>% 
  summarise(volume_t = sum(consumption_live_t), .groups = "drop") %>% 
  arrange(year)
glimpse(sp.import)
unique (sp.import$sciname) #67 species groups --> extract top10

#create top 10 species 
sp.import.top10.list <- sp.import %>% 
  group_by(sciname) %>%
  summarise(total_volume = sum(volume_t, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_volume)) %>%
  slice_head(n = 10) %>% 
  pull (sciname) #  elasmobranchii, rajiformes, prionace glauca, mustelus schmitti, callorhinchus callorynchus, squalus acanthias, carcharhinidae, carcharhiniformes, rajidae, galeorhinus galeus  

#add column for top10 species + others, and reorder based on top10species + others
sp.import.top10 <- sp.import %>% 
  mutate(
    sciname_lump = ifelse(sciname %in% sp.import.top10.list, sciname, "Others"),
    sciname_lump = factor(sciname_lump, levels = c(sp.import.top10.list, "Others"))
  )

#create color pallete for species in import
import.sp.colors <- c(
  "elasmobranchii"        = "#e41a1c", # red (same as export)
  "rajiformes"            = "#377eb8", # blue
  "prionace glauca"       = "#b3de69", # lime green
  "mustelus schmitti"     = "#a6761d", # golden-brown
  "callorhinchus callorynchus" = "#80b1d3", # sky blue
  "squalus acanthias"     = "#bc80bd", # mauve
  "carcharhinidae"        = "#4daf4a", # green
  "carcharhiniformes"     = "#fdb462", # orange-yellow
  "rajidae"               = "#cab2d6", # pale violet
  "galeorhinus galeus"    = "#ffffb3", # pale yellow
  "Others"                = "#999999"  # grey (same as export)
)

# - create bar plot for species composition per year. legend order based on highest volume_t
p.sp.import.top10 <- ggplot(sp.import.top10, aes(x = year, y = volume_t, fill = sciname_lump)) +
  geom_bar (stat = "identity", position = "stack") +
  scale_fill_manual (values = import.sp.colors) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1/1000),
    name = "Live Weight (1000 tonnes)") +
  labs (title = "c. import",
        x = "year",
        fill = "species group") +
  guides(fill = guide_legend(title.position = "top")) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.line = element_line (color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(size = 11, hjust = 0.5),
    plot.title = element_text(face = "bold", hjust = 0)
  )

ggsave("plot/p_sp_import.jpg", plot = p.sp.import.top10, width = 7, height = 8, dpi = 300)

#4. combined 3 plot

# Make sure each plot has its own legend at bottom
p.sp.source       <- p.sp.source + theme(legend.position = "bottom")
p.sp.export       <- p.sp.export + theme(legend.position = "bottom")
p.sp.import.top10 <- p.sp.import.top10 + theme(legend.position = "bottom")

# Arrange horizontally
combined_plot <- p.sp.source | p.sp.export | p.sp.import.top10

# Save to file
ggsave("plot/combined_species_plots_horizontal.jpg",
       plot = combined_plot,
       width = 20, height = 9, dpi = 300)




