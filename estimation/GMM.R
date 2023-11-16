
#install.packages("")

#Load required packages
library(readxl)
library(xlsx)
library(Hmisc)
library(broom)
library(dplyr)
library(tidyr)
library(lmtest)
library(sandwich)
library(gmm)
library(tibble)
rm(list = ls())

################################################################################################################
#Firstly, load the data
################################################################################################################
#Read data
#reer_aux_1 = [REER_t - REER_(t-8)]/REER_(t-8)
#reer_aux_2 = [REER_t - REER_(t-1)]/REER_(t-1)

reer_aux_1_data <- as.data.frame(read_excel("/Users/meylis/Desktop/PC/data_for_regressions/reer_aux.xlsx", sheet="reer_aux_1"))
reer_aux_2_data <- as.data.frame(read_excel("/Users/meylis/Desktop/PC/data_for_regressions/reer_aux.xlsx", sheet="reer_aux_2"))
gdp_cum_data    <- as.data.frame(read_excel("/Users/meylis/Desktop/PC/data_for_regressions/reer_aux.xlsx", sheet="gdp_cum"))

reer_data      <- as.data.frame(read_excel("/Users/meylis/Desktop/PC/data_for_regressions/data.xlsx", sheet="reer"))
inflation_data <- as.data.frame(read_excel("/Users/meylis/Desktop/PC/data_for_regressions/data.xlsx", sheet="inflation"))
real_gdp_data  <- as.data.frame(read_excel("/Users/meylis/Desktop/PC/data_for_regressions/data.xlsx", sheet="real_gdp"))
deflator_data  <- as.data.frame(read_excel("/Users/meylis/Desktop/PC/data_for_regressions/data.xlsx", sheet="deflator"))


#Assign column names correctly
names(reer_aux_1_data)[1]<- names(reer_aux_2_data)[1]<- names(gdp_cum_data)[1]<- names(reer_data)[1] <- names(inflation_data)[1] <- names(real_gdp_data)[1] <- names(deflator_data)[1] <- "Country"
colnames(reer_aux_1_data)[2:ncol(reer_aux_1_data)] <- colnames(reer_aux_2_data)[2:ncol(reer_aux_2_data)] <- colnames(gdp_cum_data)[2:ncol(gdp_cum_data)] <- colnames(reer_data)[2:ncol(reer_data)]




#########################################################################################################################################################################
#Linear regression GMM model
#########################################################################################################################################################################
#Desired formula to use (no intercept)
#Type: Basic
formula <- inflation ~ 0 + infl_lag + real_gdp + infl_exp +  reer

#Type: real_gdp -> log(gdp_cum)
#formula <- inflation ~ 0 + infl_lag + gdp_cum + infl_exp +  reer

#Type: reer -> reer_aux_1
#formula <- inflation ~ 0 + infl_lag + real_gdp + infl_exp +  reer_aux_1

#Type: reer -> reer_aux_2
#formula <- inflation ~ 0 + infl_lag + real_gdp + infl_exp +  reer_aux_2

#Type: inflation -> deflator (OLS estimation proved deflator to be useless)
#formula <- deflator ~ 0 + Lag(deflator, 1) + real_gdp + deflator_exp +  reer

#####################################################################################################################################################################
# Function to run GMM (with HAC) in a set of DATA

#Create table to store results
country_list <- reer_data[,1]
GMM_results_list <- list()
results_GMM <- data.frame(Country = character(), stringsAsFactors = FALSE)  


runGMM <- function() {
  
  for (j in country_list){
    tryCatch({
      
      country <- j
      
      #Store time series for the chosen country
      reer_aux_1 <- (as.numeric(reer_aux_1_data[reer_aux_1_data$Country == country, ]))
      reer_aux_2 <- (as.numeric(reer_aux_2_data[reer_aux_2_data$Country == country, ]))
      gdp_cum <- (as.numeric(gdp_cum_data[gdp_cum_data$Country == country, ] ))
      gdp_cum <- (log(gdp_cum))
      
      reer <- (as.numeric(reer_data[reer_data$Country == country, ]))
      inflation <- (as.numeric(inflation_data[inflation_data$Country == country, ]))
      real_gdp <- (as.numeric(real_gdp_data[real_gdp_data$Country == country, ] ))
      deflator <- (as.numeric(deflator_data[deflator_data$Country == country, ] ))
      
      #Store dates
      data_dates <- colnames(inflation_data)[-1]
      
      #Full data table
      #full_data <- rbind(ts(inflation), ts(reer), ts(gdp), ts(inflation_exp))
      full_data <- rbind(reer_aux_1, reer_aux_2, gdp_cum, reer, inflation, real_gdp, deflator)
      
      # Convert it to a data frame
      full_data_df <- as.data.frame(t(full_data))
      colnames(full_data_df) <- c("reer_aux_1", "reer_aux_2", "gdp_cum", "reer", "inflation", "real_gdp", "deflator")
      
      #Add expected inflation (CPI)
      expected_inflation <- full_data_df[2:nrow(full_data_df), "inflation"]
      full_data_df$infl_exp <- NA
      full_data_df[1:length(expected_inflation), "infl_exp"] <- expected_inflation
      
      #Add lag of inflation
      lag_inflation <- full_data_df[1:nrow(full_data_df), "inflation"]
      full_data_df$infl_lag <- NA
      # удалить последний элемент
      lag_inflation <- head(lag_inflation, -1)
      full_data_df[2:(nrow(full_data_df)), "infl_lag"] <- lag_inflation
      
      #Add expected inflation (deflator)
      expected_deflator <- full_data_df[2:nrow(full_data_df), "deflator"]
      full_data_df$deflator_exp <- NA
      full_data_df[1:length(expected_deflator), "deflator_exp"] <- expected_deflator
      
      #Add date column
      full_data_df$date <- NA
      full_data_df[2:nrow(full_data_df),"date"] <- data_dates
      
      #Store required names for data filtering
      components <- as.character(formula[3])
      # Split the expression by '+'
      components <- unlist(strsplit(components, "\\+"))
      # Trim leading and trailing whitespace from each component
      components <- trimws(components)
      
      #Delete rows with NAs anywhere
      #full_data_df <- na.omit(full_data_df)
      #Deletes only rows with NA in required regressors
      rows_to_remove <- which(!complete.cases(full_data_df[, components[-1]]))
      full_data_df <- full_data_df[-rows_to_remove, ]
      
      #Store required regressors names for GMM output
      regressor_names <- colnames(model.matrix(formula, data = full_data_df))
      
      #Store dates, for which data is available for the country
      data_available <- full_data_df$date
      #Create data range
      data_range <- paste(c(as.character(head(data_available, 1)), "-", as.character(tail(data_available, 1))), collapse="" )
      
      mom_lm <- function(beta, full_data_df) {
        # df is the data frame with first column as dv
        # This function returns n * q matrix
        # Each column is one moment condition before taking sample average
        # There are totally q moment conditions
        y <- (as.numeric(full_data_df[, "inflation"]))
        x <- data.matrix(full_data_df[,components[-1]])
        # Refer to moment conditions of QMLE
        m <- x * as.vector(y - x %*% (beta))
        return(cbind(m))
      }
      
      set.seed(1310)
      lm_res <- lm(formula, full_data_df)
      summary(lm_res)
      # Initial guess:rnorm(length(coef(lm_res))
      gmm_check <- gmm(mom_lm, full_data_df, rnorm(length(coef(lm_res))),
                       wmatrix = "optimal",
                       vcov = "HAC", #MDS -> HAC (assumption on properties of vector X)
                       optfct = "nlminb",  #coefficient optimization algorithm
                       control = list(eval.max = 10000)
      )
      
      
      gmm_output <- summary(gmm_check)
      rownames(gmm_output$coefficients) <- regressor_names
      
      
      # Store results in a list
      GMM_results_list[[country]] <- gmm_output
      
      # Tidy up the regression results
      tidy_results <- tidy(gmm_check)
      #Set up regressors names in tidy_results
      tidy_results[,1] <- regressor_names
      # Add a column for the country name
      tidy_results$Country <- country
      
      #Add columns for dates
      tidy_results <- add_column(tidy_results, dates= NA, .before = 1)
      #Add dates
      tidy_results$dates <- data_range
      
      # Bind the tidy results to the overall results_GMM
      results_GMM <- bind_rows(results_GMM, tidy_results) #tidy_results->wide_results
      
    }, error = function(e) {
      # Handle errors gracefully (store NA in GMM_results_list)
      cat(paste("Not enough data for regression for:", country, "\n"))
      GMM_results_list[[country]] <- NA
    })
  }
  
  
  #################################
  #Detects significant coefficients
  #################################
  results_GMM$sign <- NA
  for (j in 1:nrow(results_GMM)) {
    
    if(results_GMM[j,"p.value"] <= 0.01){
      
      results_GMM[j,"sign"] <- "***"
      
    } else if(0.01 < results_GMM[j,"p.value"] & results_GMM[j,"p.value"] <= 0.05){
      
      results_GMM[j,"sign"] <- "**"
      
    }
    else if(0.05 < results_GMM[j,"p.value"] & results_GMM[j,"p.value"] <= 0.1){
      
      results_GMM[j,"sign"] <- "*"
      
    } else {
      results_GMM[j,"sign"] <- "Not sign."
    }
  }
  

  # Group by "term" and summarize statistics on "sign"
  summary_stats_OLS <- results_GMM %>%
    group_by(term) %>%
    summarize(
      "Observ." = n(),                  # Count of observations
      "1%" = sum(sign == "***"),   # Count of ***
      "5%" = sum(sign == "**"),   # Count of **
      "10%" = sum(sign == "*"),   # Count of *
      "Not sign." = sum(sign == "Not sign.")   # Count of Not sign
    )
  
  # Display the summary statistics
  print(results_GMM)
  print(summary_stats_OLS)
  print(formula)
  
}

# Run linear regression GMM model
runGMM()

#####################################################################################################################################################################
