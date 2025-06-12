calculate_energy_bill_sums_vector <- function(results_setback, scenario_predictions, endpoints,i) {
  # Calculate gas bill
  fuel_consumption_df <- scenario_predictions[[as.character(results_setback$build_existing_model.building_id[i])]]
  ##gas_bill <- sum(( fuel_consumption_df$Natural_Gas+  fuel_consumption_df$Natural_Gas_Base) / 10.36 * 11.31)
  #propane_bill <- sum(( fuel_consumption_df$Propane+  fuel_consumption_df$Propane_Base) / 0.091452 * 2.3)
  #oil_bill <- sum((fuel_consumption_df$Fuel_Oil+fuel_consumption_df$Fuel_Oil_Base) / 0.137381 * 3.72)
  
  # Define your rate structure
  rates <- list(
    May = list(on_peak = 0.15753, off_peak = 0.14516),
    June = list(on_peak = 0.19776, off_peak = 0.14516),
    July = list(on_peak = 0.19776, off_peak = 0.14516),
    August = list(on_peak = 0.19776, off_peak = 0.14516),
    September = list(on_peak = 0.19776, off_peak = 0.14516),
    October = list(on_peak = 0.15753, off_peak = 0.14516),
    November = list(on_peak = 0.15753, off_peak = 0.14516),
    December = list(on_peak = 0.15753, off_peak = 0.14516),
    January = list(on_peak = 0.15753, off_peak = 0.14516),
    February = list(on_peak = 0.15753, off_peak = 0.14516),
    March = list(on_peak = 0.15753, off_peak = 0.14516),
    April = list(on_peak = 0.15753, off_peak = 0.14516)
  )
  
  one_year <- seq(as.POSIXct("2023-01-01 00:00:00"), by = "hour", length.out = 8736)
  
  # Create a data frame with the datetime values
  h_8760_setback_df <- data.frame(Time = one_year)
  
  # Split gas segment8
  gas_segments <- split((fuel_consumption_df$Natural_Gas+  fuel_consumption_df$Natural_Gas_Base) / 10.38 * 11.31, findInterval(seq_along((fuel_consumption_df$Natural_Gas+  fuel_consumption_df$Natural_Gas_Base) / 10.38 * 11.31), endpoints))
  gas_segment_sums <- lapply(gas_segments, sum)
  gas_sums_vector <- unlist(gas_segment_sums)
  
  propane_segments <- split((fuel_consumption_df$Propane+  fuel_consumption_df$Propane_Base) / 91.452 * 2.3, findInterval(seq_along((fuel_consumption_df$Propane+  fuel_consumption_df$Propane_Base) / 91.452 * 2.3), endpoints))
  propane_segment_sums <- lapply(propane_segments, sum)
  propane_sums_vector <- unlist(propane_segment_sums)
  
  oil_segments <- split((fuel_consumption_df$Fuel_Oil+fuel_consumption_df$Fuel_Oil_Base) / 137.381 * 3.72, findInterval(seq_along((fuel_consumption_df$Fuel_Oil+fuel_consumption_df$Fuel_Oil_Base) / 137.381 * 3.72), endpoints))
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
   
    rate <- rates[[month.name[month]]]
    # Calculate start and end indices for the current day
    start_index <- (day_index - 1) * 24 + 1
    end_index <- day_index * 24
    
    # Sum up energy consumption for the current day
    daily_electric <- total_electric[start_index:end_index]
    daily_sum <- sum(total_electric[start_index:end_index])
    if (month %in% 6:9) { # April - September (summer)
      peak <- sum(daily_electric[15:19])* rate$on_peak  # kWh for lower rate
      offpeak <- (daily_sum -sum(daily_electric[15:19]))*rate$off_peak
    } else { # October - March (winter)
      peak <- sum(daily_electric[c(15:19)])* rate$on_peak
      offpeak <- (daily_sum -sum(daily_electric[c(15:19)]))*rate$off_peak
    }
    
    # Store the daily sum
    daily_consumption[day_index] <- daily_sum
    
    # Total bill for the day
    daily_bills[day_index] <- peak+offpeak
    
    #electricity_bill <- electricity_bill +  daily_bills[day_index]
    electricity_bill[month] <- electricity_bill[month] + daily_bills[day_index]
  }
  
  total_electricity_bill <- sum(electricity_bill)
  # Calculate total energy bill
  #energy_bill <- round(gas_bill + electricity_bill, 2)
  energy_bill <- round(electricity_bill+8.5 + gas_sums_vector+ propane_sums_vector+ oil_sums_vector, 2)
  # Return the calculated energy bill
  return( energy_bill)
}


