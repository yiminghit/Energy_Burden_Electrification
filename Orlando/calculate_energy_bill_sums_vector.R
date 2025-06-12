calculate_energy_bill_sums_vector <- function(results_setback, scenario_predictions, endpoints,i) {
  # Calculate gas bill
  fuel_consumption_df <- scenario_predictions[[as.character(results_setback$build_existing_model.building_id[i])]]
  ##gas_bill <- sum(( fuel_consumption_df$Natural_Gas+  fuel_consumption_df$Natural_Gas_Base) / 10.36 * 11.31)
  #propane_bill <- sum(( fuel_consumption_df$Propane+  fuel_consumption_df$Propane_Base) / 0.091452 * 2.3)
  #oil_bill <- sum((fuel_consumption_df$Fuel_Oil+fuel_consumption_df$Fuel_Oil_Base) / 0.137381 * 3.72)
  
  one_year <- seq(as.POSIXct("2023-01-01 00:00:00"), by = "hour", length.out = 8736)
  
  # Create a data frame with the datetime values
  h_8760_setback_df <- data.frame(Time = one_year)
  
  # Split gas segments
  gas_segments <- split((fuel_consumption_df$Natural_Gas+  fuel_consumption_df$Natural_Gas_Base) / 10.38 * 26.21, findInterval(seq_along((fuel_consumption_df$Natural_Gas+  fuel_consumption_df$Natural_Gas_Base) / 10.36 * 11.31), endpoints))
  gas_segment_sums <- lapply(gas_segments, sum)
  gas_sums_vector <- unlist(gas_segment_sums)
  
  propane_segments <- split((fuel_consumption_df$Propane+  fuel_consumption_df$Propane_Base) / 91.452 * 4.73, findInterval(seq_along((fuel_consumption_df$Propane+  fuel_consumption_df$Propane_Base) / 91.452 * 2.3), endpoints))
  propane_segment_sums <- lapply(propane_segments, sum)
  propane_sums_vector <- unlist(propane_segment_sums)
  
  oil_segments <- split((fuel_consumption_df$Fuel_Oil+fuel_consumption_df$Fuel_Oil_Base) / 137.381 * 4.29, findInterval(seq_along((fuel_consumption_df$Fuel_Oil+fuel_consumption_df$Fuel_Oil_Base) / 137.381 * 3.72), endpoints))
  oil_segment_sums <- lapply(oil_segments, sum)
  oil_sums_vector <- unlist(oil_segment_sums)
  
  total_electric <- fuel_consumption_df$Electricity_Base+fuel_consumption_df$Electricity 
  one_year <- seq(as.POSIXct("2023-01-01 00:00:00"), by = "hour", length.out = 8736)
  
  # Create a data frame with the datetime values
  h_8760_setback_df <- data.frame(Time = one_year)
  # Create a column to represent the months
  h_8760_setback_df$Month <- as.numeric(format(h_8760_setback_df$Time, "%m"))
  
  # Define rates
  base_charge <- 0.06783+0.04667
  additional_charge <- 0.09283+0.04667
  
  # Initialize total bill
  monthly_consumption <- numeric(12)
  electricity_bill <- numeric(12)
  
  num_days <- length(total_electric) / 24
  
  # Initialize a vector to store daily sums
  daily_consumption <- numeric(num_days)
  
  daily_bills <- numeric(num_days)
  
  
  # Iterate over each month
  
  for (day_index in 1:num_days) {
    # Determine the month for the current day
    month <- as.numeric(format(as.Date("2023-01-01") + (day_index - 1), "%m"))
    
    # Calculate start and end indices for the current day
    start_index <- (day_index - 1) * 24 + 1
    end_index <- day_index * 24
    
    # Sum up energy consumption for the current day
    daily_sum <- sum(total_electric[start_index:end_index])
    # Store the daily sum
    daily_consumption[day_index] <- daily_sum
    #electricity_bill <- electricity_bill +  daily_bills[day_index]
    monthly_consumption[month] <- monthly_consumption[month] + daily_consumption[day_index]
  }
  
  for (i in 1:12) {
    if (monthly_consumption[i] <= 1000) {
      electricity_bill[i] <- base_charge *monthly_consumption[i]+17.5
    } else {
      additional_kwh <- monthly_consumption[i] - 1000
      electricity_bill[i] <- base_charge*monthly_consumption[i] + additional_kwh * additional_charge+17.5
    }
  }
  
  # Calculate total energy bill
  #energy_bill <- round(gas_bill + electricity_bill, 2)
  energy_bill <- round(electricity_bill + gas_sums_vector+ propane_sums_vector+ oil_sums_vector, 2)
  # Return the calculated energy bill
  return(energy_bill)
}
  
  
  