library(readxl)
calculate_energy_bill <- function(results_setback, scenario_predictions,i) {
  # Calculate gas bill
  directory_name <-'C:/Users/yimin/Dropbox/TOU_hourly_rates/'
  
  
  electricity_rate_name <- paste(directory_name, city_name,'_hourly_rates.csv', sep="")
  
  rate_name <- paste(directory_name, 'City_List_Rates.xlsx', sep="")
  # rate_data <- read.csv(rate_name)
  rate_data <- read_excel(rate_name)
  
  matching_row <- rate_data[rate_data[[1]] == city_name, ]
  
  
  gas_price <- matching_row$Gas
  propane_price <- matching_row$Propane
  oil_price <- matching_row$Oil
  
  fuel_consumption_df <- scenario_predictions[[as.character(results_setback$build_existing_model.building_id[i])]]
  gas_bill <- sum(( fuel_consumption_df$Natural_Gas+  fuel_consumption_df$Natural_Gas_Base) / 10.38 * gas_price) #One thousand cubic feet is 10.38 thermal we use 2022 gas price
  propane_bill <- sum(( fuel_consumption_df$Propane+  fuel_consumption_df$Propane_Base) / 91.452 *propane_price)
  oil_bill <- sum((fuel_consumption_df$Fuel_Oil+fuel_consumption_df$Fuel_Oil_Base) / 137.381 *oil_price)
  
  total_electric <- fuel_consumption_df$Electricity+fuel_consumption_df$Electricity_Base

  rate_data_electric <- read.csv(electricity_rate_name ,header=TRUE)
  electricity_rates <- subset(rate_data_electric, select = c(2)) #changed from c(1:3) to c(1:4)
 # total_electricity_bill <- sum((total_electric)* 0.3435)
  total_electricity_bill <- sum(total_electric * electricity_rates)
  # Calculate total energy bill
  #energy_bill <- round(gas_bill + electricity_bill, 2)
  energy_bill <- round(gas_bill + total_electricity_bill + propane_bill +oil_bill, 2)
  
  cat("Electricity Bill:", total_electricity_bill, "\n")
  cat("Total Energy Bill:", energy_bill, "\n")
  # Return the calculated energy bill
  return(energy_bill)
}
  
  
  