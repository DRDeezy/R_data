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
data_final <- read_excel("/Users/meylis/Desktop/Gaidar/DATA for НИР/FINAL/data_final.xlsx", sheet = "Real GDP (levels, IMF)")

# Clean the data: Replace "..." and empty cells with NA
data_final[data_final == "..." | data_final == ""] <- NA

# Find the columns corresponding to "1990Q1" and "2023Q2"
start_col <- which(colnames(data_final) == "1990Q1")
end_col <- which(colnames(data_final) == "2023Q2")

# Identify columns for GDP data and country names
gdp_data <- data_final[, start_col:end_col]
country_names <- data_final[, 2]

# Convert GDP data to numeric
gdp_data <- sapply(gdp_data, as.numeric)

# Take natural logarithm of GDP data
log_gdp_data <- log(gdp_data)

##########################
# Create an empty data frame of the specified size with specific column names to store final data
quarters <- colnames(log_gdp_data)
final_GDP_df <- data.frame(matrix(ncol = 134, nrow = 30))
colnames(final_GDP_df) <- quarters
rownames(final_GDP_df) <- as.list(data_final$Country)

# #Only one time series
# log_gdp_data_ts <- ts(log_gdp_data[13,],frequency=4,start=c(1990,1))
# 
# last_na_index_before <- tail(which(!is.na(log_gdp_data_ts)), 1) + 1 #134 element is NA
# first_na_index_after <- head(which(!is.na(log_gdp_data_ts)), 1) - 1 #16 element is NA
# #colnames(log_gdp_data_ts) <- quarters[(first_na_index_after+1):(last_na_index_before-1)]
# #data is bounded between 17<=x<=133
# 
# 
# final_GDP_df[13,(first_na_index_after+1):(last_na_index_before-1)] <- (na.omit(log_gdp_data_ts))
# 
# 
# seas_adj <- seas(log_gdp_data_ts)
# 
# #Plot the final time series after X-13-ARIMA filter
# plot(final(seas_adj))
# 
# #Apply HP filter to the series
# #seas_adj_hp <- hpfilter(final(seas_adj), type = "lambda", freq = 1600)$cycle
# #plot(seas_adj_hp)

##########################___ORIGINAL___##########################

ts_list <- list()

# Loop through rows (i) from 1 to 30
for (i in 1:30) {
  
  # Check if all values in the row are NA
  if ( all(is.na(log_gdp_data[i,])) | sum(!is.na(log_gdp_data[i,]))<=3*4 ) {
    # If all values are NA, skip to the next iteration
    ts_list[[i]] <- NA
    names(ts_list)[i] <- paste(country_names[i,1])  # Naming the time series objects
    final_GDP_df[i,] <- NA #fill in table with NAs
    next
  } else {
    # Save the non-NA filtered time series
    log_gdp_data_ts <- ts(log_gdp_data[i,],frequency=4,start=c(1990,1))
    
    last_na_index_before <- tail(which(!is.na(log_gdp_data_ts)), 1) + 1 #index of first element after last non-NA before new NAs
    first_na_index_after <- head(which(!is.na(log_gdp_data_ts)), 1) - 1 #index of first element after first NAs
    
    seas_adj <- seas(log_gdp_data_ts)
    seas_adj_hp <- hpfilter(final(seas_adj), type = "lambda", freq = 1600)$cycle
    
    ts_list[[i]] <- ts(seas_adj_hp, start = 1)  # Convert to time series
    names(ts_list)[i] <- paste(country_names[i,1])  # Naming the time series objects
    
    final_GDP_df[i,(first_na_index_after+1):(last_na_index_before-1)] <- (na.omit(ts(seas_adj_hp, start = 1))) #write the final data table
  }
  
}


# Save EXCEL file
write.xlsx(final_GDP_df, file = "/Users/meylis/Desktop/PC/GDP(X-12+HP).xlsx", rowNames = TRUE, colNames = TRUE, keepNA=TRUE, asTable = FALSE, sheetName = "REAL_GDP")


####################################____PLOTTING A GRAPH____####################################
################################################################################################

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

# BUILDING A GRAPH (simple)
#desired_country <- "South Africa"

# Plot the data
#plot(ts_list[[desired_country]], col = "blue", xlab = "Cyclical comp.", ylab = "Period", main = paste("Deviation from a trend, seas.adj. for", desired_country)) + abline(h = 0, col = "red")




##########################___a la INFLATION methodology___##########################

ts_list_alt <- list()

# Loop through rows (i) from 1 to 30
for (i in 1:30) {
  
  # Check if all values in the row are NA or if there is enough data to apply filter
  if ( all(is.na(log_gdp_data[i,])) | sum(!is.na(log_gdp_data[i,]))<=3*4 ) {
    # If all values are NA, skip to the next iteration
    ts_list_alt[[i]] <- NA
    names(ts_list_alt)[i] <- paste(country_names[i,1])  # Naming the time series objects
    next
  } else {
    # Save the non-NA filtered time series
    log_gdp_data_ts <- ts(na.omit(log_gdp_data[i,]),frequency=4,start=c(1990,1)) #added na.omit
    #Apply HP filter to get the trend (1)
    gdp_trend <- hpfilter(log_gdp_data_ts, type = "lambda", freq = 1600)$trend
    
    #Get rid of seasonality in factual inflation (2)
    gdp_seas_adj <- seas(log_gdp_data_ts)
    
    #Cyclical fluctuations of inflation (2)-(1)
    diff <- as.numeric(final(gdp_seas_adj) - gdp_trend)
    
    ts_list_alt[[i]] <- ts(diff, start = 1)  # Convert to time series
    names(ts_list_alt)[i] <- paste(country_names[i,1])  # Naming the time series objects
  }
  
}

desired_country = "Peru"
plot(ts_list_alt[[desired_country]], col = "blue", xlab = "Period", ylab = "Cyclical comp.", main = paste("Deviation from a trend infl., seas.adj. for", desired_country)) + abline(h = 0, col = "red")


