##Data analysis: Extracting shark consumption record in Indonesia, from consumption ARTIS Database
## 4 April 2025
## Yuli, Modified from Rosa and Leslie

x  <- 3

install.packages("here")
install.packages("readr")
install.packages("ggsankey")
install.packages("ggalluvial")



library(here)
library(dplyr)
library(readr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(ggalluvial)
library(data.table)



##import consumption data (1996 - 2020) from folder input using function, 
##Setting up thresholds year for HS Code,
##include only species from chondrichtyes class.
##combine all into 1 dataset: consumption_df

import_artis_files <- function(folder_path, year_thresholds) {
  # folder_name: name of the folder containing the CSV files
  # year_thresholds: named vector where names are hs versions and values are the start year for that hs code
  # Define the year thresholds for each HS code version
  year_thresholds <- c(
    "HS96" = 1996,
    "HS02" = 2002,
    "HS07" = 2007,
    "HS12" = 2012,
    "HS17" = 2017)
  
  
  # Get all CSV files from the folder
  folder_path <- here("Input/consumption")
  file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Create an empty list to store data
  all_data <- list()
  
  # Iterate through each file in the folder
  for (file_path in file_list) {
    
    # Extract the filename from the file path
    file_name <- basename(file_path)
    
    # Extract the HS version and year from the filename
    file_parts <- strsplit(file_name, "_")[[1]]
    hs_code <- as.numeric(gsub("HS", "", file_parts[3]))  # Extract the number after "HS"
    trade_year <- as.numeric(gsub(".csv", "", file_parts[4]))  # Extract the year of trade interaction
    
    # Determine the appropriate HS code for the year
    hs_version <- NA 
    for (hs in names(year_thresholds)) {
      if (trade_year >= year_thresholds[[hs]]) {
        hs_version <- hs
      } else {
        break
      }
    }
    
    # Check if the HS code in the filename matches the appropriate version
    if (hs_code == as.numeric(gsub("HS", "", hs_version))) {
      data <- read.csv(file_path)
      
      # Filter the dataset to include only rows where method is "capture"
      if ("method" %in% colnames(data)) {
        data <- data |> filter(method == "capture")
      }
      chondri <- read_csv(here("Input/attribute_tables", "condri.csv"))
      #Filter sciname included in chondrichthyes dataset 
      if("sciname" %in% colnames(data)) {
        data<- data |> filter(sciname %in% chondri$sciname)
      }
      # Store the filtered data
      all_data[[file_name]] <- data
    }
  }
  # Return the list of imported and filtered data
  return(all_data)
}

data <- import_artis_files(folder_path, year_thresholds)
data
here()

consumption_df <- bind_rows(data)
glimpse(consumption_df) #rows 511,628 


# filter out Indonesia (source, exporter, or consumer)
cons.ID <- consumption_df %>%
  filter(source_country_iso3c=="IDN" | exporter_iso3c=="IDN" | consumer_iso3c=="IDN")
glimpse(cons.ID) # Rows: 38,149
format(object.size(cons.ID), units = "MB") # "3.4 Mb"
# can store this in the repo
fwrite(cons.ID, here("clean/IDN_consumption_chondri.csv")) # <--- main dataframe