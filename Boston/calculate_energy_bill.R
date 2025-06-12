calculate_energy_bill <- function(results_setback, scenario_predictions,i) {
  # Calculate gas bill
  fuel_consumption_df <- scenario_predictions[[as.character(results_setback$build_existing_model.building_id[i])]]
  gas_bill <- sum(( fuel_consumption_df$Natural_Gas+  fuel_consumption_df$Natural_Gas_Base) / 10.38 * 20.45) #One thousand cubic feet is 10.38 thermal we use 2022 gas price
  propane_bill <- sum(( fuel_consumption_df$Propane+  fuel_consumption_df$Propane_Base) / 91.452 * 3.47)
  oil_bill <- sum((fuel_consumption_df$Fuel_Oil+fuel_consumption_df$Fuel_Oil_Base) / 137.381 * 4.29)
  
  total_electric <- fuel_consumption_df$Electricity+fuel_consumption_df$Electricity_Base
  
  total_electricity_bill <- sum((total_electric)* 0.3435)
  
  # Calculate total energy bill
  #energy_bill <- round(gas_bill + electricity_bill, 2)
  energy_bill <- round(gas_bill + total_electricity_bill + propane_bill +oil_bill, 2)
  # Return the calculated energy bill
  return(energy_bill)
}
  
  
  