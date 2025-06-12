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
  gas_segments <- split((fuel_consumption_df$Natural_Gas+  fuel_consumption_df$Natural_Gas_Base) / 10.38 * 12.83, findInterval(seq_along((fuel_consumption_df$Natural_Gas+  fuel_consumption_df$Natural_Gas_Base) / 10.36 * 11.31), endpoints))
  gas_segment_sums <- lapply(gas_segments, sum)
  gas_sums_vector <- unlist(gas_segment_sums)
  
  propane_segments <- split((fuel_consumption_df$Propane+  fuel_consumption_df$Propane_Base) / 91.452 * 1.63, findInterval(seq_along((fuel_consumption_df$Propane+  fuel_consumption_df$Propane_Base) / 91.452 * 2.3), endpoints))
  propane_segment_sums <- lapply(propane_segments, sum)
  propane_sums_vector <- unlist(propane_segment_sums)
  
  oil_segments <- split((fuel_consumption_df$Fuel_Oil+fuel_consumption_df$Fuel_Oil_Base) / 137.381 * 4.29, findInterval(seq_along((fuel_consumption_df$Fuel_Oil+fuel_consumption_df$Fuel_Oil_Base) / 137.381 * 3.72), endpoints))
  oil_segment_sums <- lapply(oil_segments, sum)
  oil_sums_vector <- unlist(oil_segment_sums)
  
  total_electric <- fuel_consumption_df$Electricity_Base+fuel_consumption_df$Electricity 
  one_year <- seq(as.POSIXct("2023-01-01 00:00:00"), by = "hour", length.out = 8760)
  
  # Create a data frame with the datetime values
  h_8760_setback_df <- data.frame(Time = one_year)
  # Create a column to represent the months
  h_8760_setback_df$Month <- as.numeric(format(h_8760_setback_df$Time, "%m"))
  
  # Define rates
  lower_rate_summer <- 0.1291
  upper_rate <- 0.1367
  
  # Initialize total bill
  electricity_bill <- numeric(12)
  
  num_days <- length(total_electric) / 24
  
  # Initialize a vector to store daily sums
  daily_consumption <- numeric(num_days)
  
  daily_bills <- numeric(num_days)
  
  
  # Iterate over each month
  
  for (day_index in 1:num_days) {
    # Determine the month for the current day
    month <- as.numeric(format(as.Date("2023-01-01") + (day_index - 1), "%m"))
    
    # Determine block sizes based on season
    if (month %in% 4:9) { # April - September (summer)
      block_size_1 <- 10  # kWh for lower rate
    } else { # October - March (winter)
      block_size_1 <- 16  # kWh for lower rate
    }
    
    # Calculate start and end indices for the current day
    start_index <- (day_index - 1) * 24 + 1
    end_index <- day_index * 24
    
    # Sum up energy consumption for the current day
    daily_sum <- sum(total_electric[start_index:end_index])
    
    # Store the daily sum
    daily_consumption[day_index] <- daily_sum
    if (month %in% 4:9) { # April - September (summer)
      block_size_1 <- 10  # kWh for lower rate
    } else { # October - March (winter)
      block_size_1 <- 16  # kWh for lower rate
    }
    bill_block_1 <- min(daily_consumption[day_index], block_size_1) * lower_rate_summer
    
    # Calculate bill for block 2
    bill_block_2 <- max(0, daily_consumption[day_index] - block_size_1) * upper_rate
    
    # Total bill for the day
    daily_bills[day_index] <- bill_block_1 + bill_block_2+0.2728
    
    #electricity_bill <- electricity_bill +  daily_bills[day_index]
    electricity_bill[month] <- electricity_bill[month] + daily_bills[day_index]
  }
  
  total_electricity_bill <- sum(electricity_bill)
  # Calculate total energy bill
  #energy_bill <- round(gas_bill + electricity_bill, 2)
  energy_bill <- round(electricity_bill + gas_sums_vector+ propane_sums_vector+ oil_sums_vector, 2)
  # Return the calculated energy bill
  return(energy_bill)
}
  
  
  