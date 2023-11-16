setwd("/Users/meylis/Desktop/PC")
# Install and load required packages

#CLEARS ALL: rm(list = ls())
#path: /Users/meylis/Desktop/Gaidar/DATA for НИР/FINAL/data_final_inf.xlsx

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
data_final_inf <- read_excel("/Users/meylis/Desktop/Gaidar/DATA for НИР/FINAL/data_final_inf_latest.xlsx", sheet = "Inflation (CPI, q-o-q)")

# Clean the data: Replace "..." and empty cells with NA
data_final_inf[data_final_inf == "..." | data_final_inf == ""] <- NA

# Find the columns corresponding to "1990Q1" and "2023Q2"
start_col <- which(colnames(data_final_inf) == "1990Q1")
end_col <- which(colnames(data_final_inf) == "2023Q2")

# Identify columns for GDP data and country names
cpi_data <- data_final_inf[, start_col:end_col]
country_names <- data_final_inf[, 2]

# Convert GDP data to numeric
cpi_data <- sapply(cpi_data, as.numeric)

# Take natural logarithm of GDP data
#log_cpi_data <- (log(cpi_data + 1)) #monotone +1, o/w would be NaNs
log_cpi_data <- cpi_data  #monotone +1, o/w would be NaNs

##########################
# Create an empty data frame of the specified size with specific column names to store final data
quarters <- colnames(log_cpi_data)
final_INFL_df <- data.frame(matrix(ncol = 134, nrow = 30))
colnames(final_INFL_df) <- quarters
rownames(final_INFL_df) <- as.list(data_final_inf$Country)


##########################
#Factual inflation as time series
#log_inflation_data_ts <- ts(log_cpi_data[14,],frequency=4,start=c(1990,1))

#Apply HP filter to get the trend (1)
#cpi_trend <- hpfilter(log_inflation_data_ts, type = "lambda", freq = 1600)$trend
#plot(cpi_trend)

#Get rid of seasonality in factual inflation (2)
#infl_seas_adj <- seas(log_inflation_data_ts)
#Plot the final time series after X-13-ARIMA filter
#plot(final(infl_seas_adj))

#Cyclical fluctuations of inflation (2)-(1)
#diff <- final(infl_seas_adj) - cpi_trend
#plot(diff)


##########################___ORIGINAL___##########################
infl_list <- list()

# Loop through rows (i) from 1 to 30
for (i in 1:30) {
  
  # Check if all values in the row are NA or if there is enough data to apply filter
  if ( all(is.na(log_cpi_data[i,])) | sum(!is.na(log_cpi_data[i,]))<=3*4 ) {
    # If all values are NA, skip to the next iteration
    infl_list[[i]] <- NA
    names(infl_list)[i] <- paste(country_names[i,1])  # Naming the time series objects
    final_INFL_df[i,] <- NA
    next
  } else {
    # Save the non-NA filtered time series
    log_inflation_data_ts <- ts(na.omit(log_cpi_data[i,]),frequency=4,start=c(1990,1)) #added NA.OMIT
    
    last_na_index_before <- tail(which(!is.na(log_inflation_data_ts)), 1) + 1 #index of first element after last non-NA before new NAs
    first_na_index_after <- head(which(!is.na(log_inflation_data_ts)), 1) - 1 #index of first element after first NAs
    

    #Apply HP filter to get the trend (1)
    cpi_trend <- hpfilter(log_inflation_data_ts, type = "lambda", freq = 1600)$trend
    
    #Get rid of seasonality in factual inflation (2)
    infl_seas_adj <- seas(log_inflation_data_ts)
    
    #Cyclical fluctuations of inflation (2)-(1)
    diff <- as.numeric(final(infl_seas_adj) - cpi_trend)
    
    infl_list[[i]] <- ts(diff, start = 1)  # Convert to time series
    names(infl_list)[i] <- paste(country_names[i,1])  # Naming the time series objects
    
    final_INFL_df[i,(first_na_index_after+1):(last_na_index_before-1)] <- (na.omit(ts(diff, start = 1))) #write the final data table
  }
  
}

# Save EXCEL file
write.xlsx(final_INFL_df, file = "/Users/meylis/Desktop/PC/INFLATION.xlsx", rowNames = TRUE, colNames = TRUE, keepNA=TRUE, asTable = FALSE, sheetName = "INFLATION")

####################################____PLOTTING A GRAPH____####################################
################################################################################################

# Define a function that takes user input and utilizes it
custom_function <- function() {
  # Ask the user for input
  user_input <- readline("Please enter a value: ")
  
  # Convert the input to the appropriate data type, if needed
  user_input <- as.character(user_input)  # Convert to numeric, if expecting a numeric value
  
  #Build a graph
  plot(infl_list[[user_input]], col = "blue", xlab = "Period", ylab = "Cyclical comp.", main = paste("Deviation from a trend infl., seas.adj. for", user_input)) + abline(h = 0, col = "red")
  
  # Return the result
  return(plot(infl_list[[user_input]], col = "blue", xlab = "Period", ylab = "Cyclical comp.", main = paste("Deviation from a trend infl., seas.adj. for", user_input)) + abline(h = 0, col = "red"))
}

#Call a function
output <- custom_function()


##########################___First sesonality, then cyclicality (as GDP)___##########################


ts_list_alt <- list()

# Loop through rows (i) from 1 to 30
for (i in 1:30) {
  
  # Check if all values in the row are NA
  if ( all(is.na(log_cpi_data[i,])) | sum(!is.na(log_cpi_data[i,]))<=3*4 ) {
    # If all values are NA, skip to the next iteration
    ts_list_alt[[i]] <- NA
    names(ts_list_alt)[i] <- paste(country_names[i,1])  # Naming the time series objects
    next
  } else {
    # Save the non-NA filtered time series
    log_inflation_data_ts <- ts(log_cpi_data[i,],frequency=4,start=c(1990,1))
    seas_adj <- seas(log_inflation_data_ts)
    seas_adj_hp <- hpfilter(final(seas_adj), type = "lambda", freq = 1600)$cycle
    
    ts_list_alt[[i]] <- ts(seas_adj_hp, start = 1)  # Convert to time series
    names(ts_list_alt)[i] <- paste(country_names[i,1])  # Naming the time series objects
  }
  
}

desired_country = "Peru"
plot(ts_list_alt[[desired_country]], col = "blue", xlab = "Period", ylab = "Cyclical comp.", main = paste("Deviation from a trend infl., seas.adj. for", desired_country)) + abline(h = 0, col = "red")

