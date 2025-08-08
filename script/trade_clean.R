x <- 27
install.packages("here")
install.packages("pivottabler", dependencies = TRUE)

library(here)
library(tidyverse)




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
  folder_path <- here("Input/trade")
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

trade_df <- bind_rows(data)
trade_df


# filter out Indonesia (source, exporter, or consumer)
trade.ID <- trade_df %>%
  filter(source_country_iso3c=="IDN" | exporter_iso3c=="IDN" | importer_iso3c=="IDN")
glimpse(trade.ID) # Rows: 64,846
format(object.size(trade.ID), units = "MB") # "5.5 Mb"
# can store this in the repo
fwrite(trade.ID, here("clean/IDN_trade_chondri.csv")) # <--- main dataframe
