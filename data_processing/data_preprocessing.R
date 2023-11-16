#R-file
#Conducts data pre-processing for GDP, inflation and REER

rm(list = ls())
#Load required packages
library(readxl)
library(mFilter)
library(openxlsx)
library(xlsx)
library(ggplot2)
library(tidyr)
library(seasonal)



#Load required files
#Inflation
data_final_inf <- read_excel("/Users/meylis/Desktop/Gaidar/DATA for НИР/FINAL/data_final_latest.xlsx", sheet = "Inflation (CPI, q-o-q)")
#GDP
#Real GDP (MA s.ad.)
data_final_gdp <- read_excel("/Users/meylis/Desktop/Gaidar/DATA for НИР/FINAL/data_final_latest.xlsx", sheet = "Real GDP (levels, IMF)")
#REER
data_final_reer <- read_excel("/Users/meylis/Desktop/Gaidar/DATA for НИР/FINAL/data_final_latest.xlsx", sheet = "REER")
#GDP_deflator
data_final_deflator <- read_excel("/Users/meylis/Desktop/Gaidar/DATA for НИР/FINAL/data_final_latest.xlsx", sheet = "GDP deflator inflation")

#Prepare data
#Replace "..." and empty cells with NA
data_final_inf[data_final_inf == "..." | data_final_inf == ""] <- NA
data_final_gdp[data_final_gdp == "..." | data_final_gdp == ""] <- NA
data_final_reer[data_final_reer == "..." | data_final_reer == ""] <- NA
data_final_deflator[data_final_deflator == "..." | data_final_deflator == ""] <- NA

#Methodology:
#for inflation and GDP: eliminate seasonality, later decompose into cycle and tren
#for REER: decompose into trend


##########################
#Work with INFLATION
##########################

# Find the columns corresponding to "1990Q1" and "2023Q2"
start_col <- which(colnames(data_final_inf) == "1990Q1")
end_col <- which(colnames(data_final_inf) == "2023Q2")

# Identify columns for GDP data and country names
cpi_data <- data_final_inf[, start_col:end_col]
country_names <- data_final_inf[, 2]

# Convert GDP data to numeric
cpi_data <- sapply(cpi_data, as.numeric)

# Take natural logarithm of GDP data
log_cpi_data <- cpi_data  #data in file is already inflation

# Create an empty data frame of the specified size with specific column names to store final data
quarters <- colnames(log_cpi_data)
final_INFL_df <- data.frame(matrix(ncol = 134, nrow = 30))
colnames(final_INFL_df) <- quarters
rownames(final_INFL_df) <- as.list(data_final_inf$Country)

##########################___First sesonality, then cyclicality (as GDP)___##########################

infl_list <- list()

# Loop through rows (i) from 1 to 30
for (i in 1:30) {
  
  # Check if all values in the row are NA
  if ( all(is.na(log_cpi_data[i,])) | sum(!is.na(log_cpi_data[i,]))<=3*4 ) {
    # If all values are NA, skip to the next iteration
    infl_list[[i]] <- NA
    names(infl_list)[i] <- paste(country_names[i,1])  # Naming the time series objects
    final_INFL_df[i,] <- NA #fill in table with NAs
    next
  } else {
    
    # Save the non-NA filtered time series
    log_inflation_data_ts <- ts((log_cpi_data[i,]),frequency=4,start=c(1990,1)) #added NA.OMIT
    
    last_na_index_before <- tail(which(!is.na(log_inflation_data_ts)), 1) + 1 #index of first element after last non-NA before new NAs
    first_na_index_after <- head(which(!is.na(log_inflation_data_ts)), 1) - 1 #index of first element after first NAs
    
    #Eliminate seasonality
    seas_adj <- seas(log_inflation_data_ts)
    #Apply filter
    seas_adj_hp <- hpfilter(final(seas_adj), type = "lambda", freq = 1600)$cycle
    
    infl_list[[i]] <- ts(seas_adj_hp, start = 1)  # Convert to time series
    names(infl_list)[i] <- paste(country_names[i,1])  # Naming the time series objects
    
    final_INFL_df[i,(first_na_index_after+1):(last_na_index_before-1)] <- (na.omit(ts(seas_adj_hp, start = 1))) #write the final data table
  }
  
}

################################################
#Here input desired country for its data to be plotted
################################################
#desired_country = "Mexico" #||China
#plot(infl_list[[desired_country]], col = "blue", xlab = "Period", ylab = "Cyclical comp.", main = paste("Deviation from a trend infl., seas.adj. for", desired_country)) +
#  abline(h = 0, col = "red")

# Save EXCEL file
#openxlsx package
#write.xlsx(final_INFL_df, file = "/Users/meylis/Desktop/PC/INFLATION.xlsx", rowNames = TRUE, colNames = TRUE, keepNA=TRUE, asTable = FALSE, sheetName = "INFLATION")

#xlsx package
#write.xlsx(final_INFL_df, file = "/Users/meylis/Desktop/PC/data_for_regressions/data.xlsx", sheetName="inflation")



##########################
#Work with GDP
##########################

# Find the columns corresponding to "1990Q1" and "2023Q2"
start_col <- which(colnames(data_final_gdp) == "1990Q1")
end_col <- which(colnames(data_final_gdp) == "2023Q2")

# Identify columns for GDP data and country names
gdp_data <- data_final_gdp[, start_col:end_col]
country_names <- data_final_gdp[, 2]

# Convert GDP data to numeric
gdp_data <- sapply(gdp_data, as.numeric)

# Take natural logarithm of GDP data
log_gdp_data <- log(gdp_data)

# Create an empty data frame of the specified size with specific column names to store final data
quarters <- colnames(log_gdp_data)
final_GDP_df <- data.frame(matrix(ncol = 134, nrow = 30))
colnames(final_GDP_df) <- quarters
rownames(final_GDP_df) <- as.list(data_final_gdp$Country)

##########################___First sesonality, then cyclicality___##########################

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

################################################
#Here input desired country for its data to be plotted
################################################
#desired_country = "Peru" #||China
#plot(ts_list[[desired_country]], col = "blue", xlab = "Period", ylab = "Cyclical comp.", main = paste("Deviation from a trend infl., seas.adj. for", desired_country)) +
#  abline(h = 0, col = "red")


# Save EXCEL file
#write.xlsx(final_GDP_df, file = "/Users/meylis/Desktop/PC/GDP(X-12+HP).xlsx", rowNames = TRUE, colNames = TRUE, keepNA=TRUE, asTable = FALSE, sheetName = "REAL_GDP")
#path_to_save <- "/Users/meylis/Desktop/PC/data_for_regressions/data.xlsx"
#write.xlsx(final_GDP_df, file = path_to_save, sheetName="real_gdp",rowNames = TRUE, append = TRUE)







##########################
#Work with REER
##########################


# Find the columns corresponding to "1990Q1" and "2023Q2"
start_col <- which(colnames(data_final_reer) == "1990Q1")
end_col <- which(colnames(data_final_reer) == "2023Q2")

# Identify columns for GDP data and country names
reer_data <- data_final_reer[, start_col:end_col]
country_names <- data_final_reer[, 2]

# Convert  data to numeric
reer_data <- sapply(reer_data, as.numeric)

# Take natural logarithm of  data
log_reer_data <- log(reer_data)

# Create an empty data frame of the specified size with specific column names to store final data
quarters <- colnames(log_reer_data)
final_REER_df <- data.frame(matrix(ncol = 134, nrow = 30))
colnames(final_REER_df) <- quarters
rownames(final_REER_df) <- as.list(data_final_reer$Country)


##########################___Extracts cyclical component of REER___##########################

ts_list_reer <- list()

# Loop through rows (i) from 1 to 30
for (i in 1:30) {
  
  # Check if all values in the row are NA
  if ( all(is.na(log_reer_data[i,])) | sum(!is.na(log_reer_data[i,]))<=3*4 ) {
    # If all values are NA, skip to the next iteration
    ts_list_reer[[i]] <- NA
    names(ts_list_reer)[i] <- paste(country_names[i,1])  # Naming the time series objects
    final_REER_df[i,] <- NA #fill in table with NAs
    next
  } else {
    # Save the non-NA filtered time series
    log_reer_data_ts <- ts(na.omit(log_reer_data[i,]),frequency=4,start=c(1990,1)) # WARNING: ADDED NA.OMIT, o/w NA after filtering
    
    last_na_index_before <- tail(which(!is.na(log_reer_data_ts)), 1) + 1 #index of first element after last non-NA before new NAs
    first_na_index_after <- head(which(!is.na(log_reer_data_ts)), 1) - 1 #index of first element after first NAs
    
    reer_hp <- hpfilter(log_reer_data_ts, type = "lambda", freq = 1600)$cycle
    
    ts_list_reer[[i]] <- ts(reer_hp, start = 1)  # Convert to time series
    names(ts_list_reer)[i] <- paste(country_names[i,1])  # Naming the time series objects
    
    final_REER_df[i,(first_na_index_after+1):(last_na_index_before-1)] <- (na.omit(ts(reer_hp, start = 1))) #write the final data table
  }
  
}


################################################
#Here input desired country for its data to be plotted
################################################
#desired_country = "Pakistan" #||China
#plot(ts_list_reer[[desired_country]], col = "blue", xlab = "Period", ylab = "Cyclical comp.", main = paste("Deviation from a trend infl., seas.adj. for", desired_country)) +
#  abline(h = 0, col = "red")

# Save EXCEL file
#write.xlsx(final_REER_df, file = "/Users/meylis/Desktop/PC/REER_cyclical.xlsx", rowNames = TRUE, colNames = TRUE, keepNA=TRUE, asTable = FALSE, sheetName = "REER")
#write.xlsx(final_REER_df, path_to_save, sheetName="reer", append=TRUE, rowNames = TRUE)

##########################___Create two more REER measures___##########################

#1. reer_aux_1 = [REER_t - REER_(t-8)]/REER_(t-8)
#2. reer_aux_2 = [REER_t - REER_(t-1)]/REER_(t-1)


#To be done in EXCEL



##########################
#Work with GDP_deflator
##########################

# Find the columns corresponding to "1990Q1" and "2023Q2"
start_col <- which(colnames(data_final_deflator) == "1990Q1")
end_col <- which(colnames(data_final_deflator) == "2023Q2")

# Identify columns for GDP data and country names
defl_data <- data_final_deflator[, start_col:end_col]
country_names <- data_final_deflator[, 2]

# Convert GDP data to numeric
defl_data <- sapply(defl_data, as.numeric)

# Create an empty data frame of the specified size with specific column names to store final data
quarters <- colnames(defl_data)
final_deflator_df <- data.frame(matrix(ncol = 134, nrow = 30))
colnames(final_deflator_df) <- quarters
rownames(final_deflator_df) <- as.list(data_final_deflator$Country)


##########################___Extracts cyclical component of deflator##########################


deflt_list <- list()

# Loop through rows (i) from 1 to 30
for (i in 1:30) {
  
  # Check if all values in the row are NA
  if ( all(is.na(defl_data[i,])) | sum(!is.na(defl_data[i,]))<=3*4 ) {
    # If all values are NA, skip to the next iteration
    deflt_list[[i]] <- NA
    names(deflt_list)[i] <- paste(country_names[i,1])  # Naming the time series objects
    final_deflator_df[i,] <- NA #fill in table with NAs
    next
  } else {
    
    # Save the non-NA filtered time series
    defl_data_ts <- ts(na.omit(defl_data[i,]),frequency=4,start=c(1990,1)) #added NA.OMIT
    
    last_na_index_before <- tail(which(!is.na(defl_data_ts)), 1) + 1 #index of first element after last non-NA before new NAs
    first_na_index_after <- head(which(!is.na(defl_data_ts)), 1) - 1 #index of first element after first NAs
    
    #Change notations
    seas_adj <- defl_data_ts
    #Apply filter
    seas_adj_hp <- hpfilter((seas_adj), type = "lambda", freq = 1600)$cycle
    
    deflt_list[[i]] <- ts(seas_adj_hp, start = 1)  # Convert to time series
    names(deflt_list)[i] <- paste(country_names[i,1])  # Naming the time series objects
    final_deflator_df[i,(first_na_index_after+1):(last_na_index_before-1)] <- (na.omit(ts(seas_adj_hp, start = 1))) #write the final data table
  }
  
}

################################################
#Here input desired country for its data to be plotted
################################################
#desired_country = "Peru" #||China
#plot(deflt_list[[desired_country]], col = "blue", xlab = "Period", ylab = "Cyclical comp.", main = paste("Deviation from a trend infl., seas.adj. for", desired_country)) +
#  abline(h = 0, col = "red")

#xlsx package
#write.xlsx(final_deflator_df, path_to_save, sheetName="deflator", append=TRUE, rowNames = TRUE)

################################################
#SAVE THE FILE
################################################

# Specify the Excel file path
path_to_save <- "/Users/meylis/Desktop/PC/data_for_regressions/data.xlsx"
# Create a new workbook
wb <- createWorkbook()

# Add the first data frame to the first worksheet
addWorksheet(wb, sheetName = "inflation")
addWorksheet(wb, sheetName = "real_gdp")
addWorksheet(wb, sheetName = "reer")
addWorksheet(wb, sheetName = "deflator")

# Save data
writeData(wb, sheet = "inflation", x = final_INFL_df, colNames = TRUE, rowNames = TRUE)
writeData(wb, sheet = "real_gdp", x = final_GDP_df, colNames = TRUE, rowNames = TRUE)
writeData(wb, sheet = "reer", x = final_REER_df, colNames = TRUE, rowNames = TRUE)
writeData(wb, sheet = "deflator", x = final_deflator_df, colNames = TRUE, rowNames = TRUE)

# Save the workbook to the specified Excel file
saveWorkbook(wb, file = path_to_save)










