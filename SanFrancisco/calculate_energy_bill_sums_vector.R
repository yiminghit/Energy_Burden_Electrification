library(readxl)
calculate_energy_bill_sums_vector <- function(results_setback, scenario_predictions, endpoints,i) {
  # Calculate gas bill
  fuel_consumption_df <- scenario_predictions[[as.character(results_setback$build_existing_model.building_id[i])]]
  ##gas_bill <- sum(( fuel_consumption_df$Natural_Gas+  fuel_consumption_df$Natural_Gas_Base) / 10.36 * 11.31)
  #propane_bill <- sum(( fuel_consumption_df$Propane+  fuel_consumption_df$Propane_Base) / 0.091452 * 2.3)
  #oil_bill <- sum((fuel_consumption_df$Fuel_Oil+fuel_consumption_df$Fuel_Oil_Base) / 0.137381 * 3.72)
  
  one_year <- seq(as.POSIXct("2023-01-01 00:00:00"), by = "hour", length.out = 8736)
  
  directory_name <-'C:/Users/yimin/Dropbox/TOU_hourly_rates/'
  
  
  electricity_rate_name <- paste(directory_name, city_name,'_hourly_rates.csv', sep="")
  
 # rate_name <- paste(directory_name, 'city_list.xlsx', sep="")
  rate_name <- paste(directory_name, 'City_List_Rates.xlsx', sep="")
 # rate_data <- read.csv(rate_name)
  rate_data <- read_excel(rate_name)
  
  matching_row <- rate_data[rate_data[[1]] == city_name, ]
  
  
  gas_price <- matching_row$Gas
  propane_price <- matching_row$Propane
  oil_price <- matching_row$Oil
  
  # Create a data frame with the datetime values
  h_8760_setback_df <- data.frame(Time = one_year)
  
  # Split gas segments
  gas_segments <- split((fuel_consumption_df$Natural_Gas+  fuel_consumption_df$Natural_Gas_Base) / 10.38 *   gas_price , findInterval(seq_along((fuel_consumption_df$Natural_Gas+  fuel_consumption_df$Natural_Gas_Base) / 10.38 *   gas_price ), endpoints))
  gas_segment_sums <- lapply(gas_segments, sum)
  gas_sums_vector <- unlist(gas_segment_sums)
  
  propane_segments <- split((fuel_consumption_df$Propane+  fuel_consumption_df$Propane_Base) / 91.452 * propane_price, findInterval(seq_along((fuel_consumption_df$Propane+  fuel_consumption_df$Propane_Base) / 91.452 * propane_price), endpoints))
  propane_segment_sums <- lapply(propane_segments, sum)
  propane_sums_vector <- unlist(propane_segment_sums)
  
  oil_segments <- split((fuel_consumption_df$Fuel_Oil+fuel_consumption_df$Fuel_Oil_Base) / 137.381 *  oil_price, findInterval(seq_along((fuel_consumption_df$Fuel_Oil+fuel_consumption_df$Fuel_Oil_Base) / 137.381 *oil_price), endpoints))
  oil_segment_sums <- lapply(oil_segments, sum)
  oil_sums_vector <- unlist(oil_segment_sums)
  
  total_electric <- fuel_consumption_df$Electricity_Base+fuel_consumption_df$Electricity
  

  rate_data_origin <- read.csv(electricity_rate_name ,header=TRUE)
  electricity_rates <- subset(rate_data_origin, select = c(2)) #changed from c(1:3) to c(1:4)
  
  
  electricity_costs <- total_electric * electricity_rates
  electricity_costs <- electricity_costs$Rate
  # Split electricity costs into segments
  electricity_segments <- split(electricity_costs, findInterval(seq_along(electricity_costs), endpoints))
  
  # Sum costs for each segment
  electricity_segment_sums <- lapply(electricity_segments, sum)
  
  # Convert to a vector
  electricity_sums_vector <- unlist(electricity_segment_sums)
  
  
  # Calculate total energy bill
  #energy_bill <- round(gas_bill + electricity_bill, 2)
  energy_bill <- round(  electricity_sums_vector + gas_sums_vector+ propane_sums_vector+ oil_sums_vector, 2)
  # Return the calculated energy bill
  return(energy_bill)
}
  
  
  