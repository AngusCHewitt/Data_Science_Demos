library(tidyverse)
library(fs)

# Function to update header and keep the same columns
update_header_csv <- function(folder_path, column_order) {
  # Get a list of all CSV files in the folder
  csv_files <- dir_ls(folder_path, glob = "*.csv")
  
  if (length(csv_files) == 0) {
    cat("No CSV files found in folder:", folder_path, "\n")
    return()
  }
  
  for (file in csv_files) {
    tryCatch({
      # Read the CSV file
      df <- read_csv(file, show_col_types = FALSE)
      
      # Check that the dimensions are the same (very important)
      if (ncol(df) != length(column_order)) {
        stop(paste("Column count mismatch in", file, ". Expected", length(column_order), "columns, found", ncol(df)))
      }
      
      # 2. Rename the columns to use the new column names
      colnames(df) <- column_order # VERY SIMPLE
      
      # Write the reordered data with the new header back to the file
      write_csv(df, file, na = "") # Re-write, handling missing values.
      
      cat("Updated header in:", file, "\n")
      
    }, error = function(e) {
      cat("Error processing", file, ":", conditionMessage(e), "\n")
    })
  }
}


# Example Usage:
folder_path <- "data/CSV_WL/"  # Replace with the path to your folder

# Define the desired column order (your color names) - *IMPORTANT*
column_order <- c("game_date", "home_team", "away_team", "home_score", "away_score",
                  "Opener_home", "Opener_away", "Caesars_Sportbook_home",
                  "Caesars_Sportbook_away", "Bet_365_home", "Bet_365_away",
                  "Bet_MGM_home", "Bet_MGM_away", "Bet_Rivers_home",
                  "Bet_Rivers_away", 
                  "Fanduel_Sportsbook_home", "Fanduel_Sportsbook_away",
                  "SBK_home", "SBK_away", "DraftKings_home", "DraftKings_away",
                  "Sugar_House_home", "Sugar_House_away")  # Replace with your desired column names


# game_date
# home_team
# away_team
# home_score
# away_score
# Opener_home
# Opener_away
# Caesars_Sportbook_home
# Caesars_Sportbook_away
# Bet_365_home
# Bet_365_away
# Bet_MGM_home
# Bet_MGM_away
# Bet_Rivers_home
# Bet_Rivers_away
# Fanduel_Sportsbook_home
# Fanduel_Sportsbook_away
# SBK_home
# SBK_away
# DraftKings_home
# DraftKings_away
# Sugar_House_home
# Sugar_House_away

update_header_csv(folder_path, column_order)