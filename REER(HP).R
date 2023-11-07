setwd("/Users/meylis/Desktop/PC")
# Install and load required packages

#CLEARS ALL: rm(list = ls())
#path: /Users/meylis/Desktop/Gaidar/DATA for НИР/FINAL/data_final.xlsx

########################################################################################

# Install and load necessary packages
# install.packages("readxl")
# install.packages("mFilter")
# install.packages("openxlsx")
# install.packages("ggplot2")
# install.packages("seasonal")
# install.packages("x12")

#CLEARS ALL
rm(list = ls())
library(readxl)
library(mFilter)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(seasonal)
#library(x12)

# Load the Excel file
data_final <- read_excel("/Users/meylis/Desktop/Gaidar/DATA for НИР/FINAL/data_final.xlsx", sheet = "REER")

# Clean the data: Replace "..." and empty cells with NA
data_final[data_final == "..." | data_final == ""] <- NA

# Find the columns corresponding to "1990Q1" and "2023Q2"
start_col <- which(colnames(data_final) == "1990Q1")
end_col <- which(colnames(data_final) == "2023Q2")

# Identify columns for GDP data and country names
reer_data <- data_final[, start_col:end_col]
country_names <- data_final[, 2]

# Convert GDP data to numeric
reer_data <- sapply(reer_data, as.numeric)

# Take natural logarithm of GDP data
log_reer_data <- log(reer_data)

##########################
#Only one time series
# log_reer_data_ts <- ts(log_reer_data[20,],frequency=4,start=c(1990,1))
# reer_hp <- hpfilter(log_reer_data_ts, type = "lambda", freq = 1600)$cycle
# plot(reer_hp) + abline(h = 0, col = "red")

# Create an empty data frame of the specified size with specific column names to store final data
quarters <- colnames(log_reer_data)
final_REER_df <- data.frame(matrix(ncol = 134, nrow = 30))
colnames(final_REER_df) <- quarters
rownames(final_REER_df) <- as.list(data_final$Country)


###############################
ts_list <- list()

# Loop through rows (i) from 1 to 30
for (i in 1:30) {
  
  # Check if all values in the row are NA
  if ( all(is.na(log_reer_data[i,])) | sum(!is.na(log_reer_data[i,]))<=3*4 ) {
    # If all values are NA, skip to the next iteration
    ts_list[[i]] <- NA
    names(ts_list)[i] <- paste(country_names[i,1])  # Naming the time series objects
    final_REER_df[i,] <- NA #fill in table with NAs
    next
  } else {
    # Save the non-NA filtered time series
    log_reer_data_ts <- ts(na.omit(log_reer_data[i,]),frequency=4,start=c(1990,1)) # WARNING: ADDED NA.OMIT, o/w NA after filtering
    
    last_na_index_before <- tail(which(!is.na(log_reer_data_ts)), 1) + 1 #index of first element after last non-NA before new NAs
    first_na_index_after <- head(which(!is.na(log_reer_data_ts)), 1) - 1 #index of first element after first NAs
    
    reer_hp <- hpfilter(log_reer_data_ts, type = "lambda", freq = 1600)$cycle
    
    ts_list[[i]] <- ts(reer_hp, start = 1)  # Convert to time series
    names(ts_list)[i] <- paste(country_names[i,1])  # Naming the time series objects
    
    final_REER_df[i,(first_na_index_after+1):(last_na_index_before-1)] <- (na.omit(ts(reer_hp, start = 1))) #write the final data table
  }
  
}

# Save EXCEL file
write.xlsx(final_REER_df, file = "/Users/meylis/Desktop/PC/REER.xlsx", rowNames = TRUE, colNames = TRUE, keepNA=TRUE, asTable = FALSE, sheetName = "REER")

#PLOTTING A GRAPH
# Define a function that takes user input and utilizes it
custom_function <- function() {
  # Ask the user for input
  user_input <- readline("Please enter a value: ")
  
  # Convert the input to the appropriate data type, if needed
  user_input <- as.character(user_input)  # Convert to numeric, if expecting a numeric value
  
  #Build a graph
  plot(ts_list[[user_input]], col = "blue", xlab = "Cyclical comp.", ylab = "Period", main = paste("Deviation from a trend, seas.adj. for", user_input)) + abline(h = 0, col = "red")
  
  # Return the result
  return(plot(ts_list[[user_input]], col = "blue", xlab = "Cyclical comp.", ylab = "Period", main = paste("Deviation from a trend, seas.adj. for", user_input)) + abline(h = 0, col = "red"))
}

#Call a function
output <- custom_function()


