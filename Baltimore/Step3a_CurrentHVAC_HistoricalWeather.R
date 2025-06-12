#rm(list = ls())
library(dplyr)
library("tidyr")
library(progress)
library(readr)
# <-'Phoenix' #To update with the city to run
#directory_name <-'C:/Users/Shuhaib Nawawi/Dropbox (University of Michigan)/Building_RCM/RCM_June/' #Use dropbox path so it's all standardized
directory_name <-'C:/Users/yimin/Dropbox/RCM_June/'
directory_name2 <- 'D:/Umich_Project/Experiments/'

setwd("D:/Umich_Project/Restock_Data")

#local Directory
root_dir = "D:/Umich_Project/Restock_Data"
results <- read.csv(paste('project_singlefamily_detached_',city_name,'/localResults/results.csv', sep=""), header = TRUE)


output_path <-paste(directory_name2, city_name, '/', sep="")
#Read building_id
path_name1 <- paste(directory_name, 'RCM_Building_Id/', city_name, '_building_id.csv', sep="")
building_id <- read.csv(path_name1, header=TRUE)

#Read regression coefficients
path_name2 <- paste(directory_name, 'RCM_Building_Coefficients_v2/', city_name, '_coeff.csv', sep="")
building_coeff <- read.csv(path_name2, header=TRUE)

year_data <- list()
column_sums_list <- list()

results_base <- subset(results, apply_upgrade.run_measure == 0 & apply_upgrade_2.run_measure == 0)

#Exclude houses with "None" or "Other Fuel" base heating
results_base <- subset(results_base, building_characteristics_report.heating_fuel != "None")
results_base <- subset(results_base, building_characteristics_report.heating_fuel != "Other Fuel")

#Exclude houses with 'no heat pump' and 'no cooling' 
results_base <- results_base[results_base$building_characteristics_report.hvac_system_cooling != "None" | results_base$building_characteristics_report.hvac_system_heat_pump != "None",] # Dropped 51 houses

#Only get relevant columns for RCM
results_base_df <- select(results_base, c(X_id, build_existing_model.building_id, building_characteristics_report.cooling_setpoint, building_characteristics_report.heating_fuel, building_characteristics_report.heating_setpoint, building_characteristics_report.heating_fuel, starts_with("building_characteristics_report.hvac_system_")))

#Get baseline heater efficiency
results_base_df$oil_afue <- as.numeric(gsub("\\D", "", results_base_df$building_characteristics_report.hvac_system_heating_fuel_oil))
results_base_df$oil_afue <- replace(results_base_df$oil_afue,is.na(results_base_df$oil_afue),0)

results_base_df$natural_gas_afue <- as.numeric(gsub("\\D", "", results_base_df$building_characteristics_report.hvac_system_heating_natural_gas))
results_base_df$natural_gas_afue <- replace(results_base_df$natural_gas_afue, results_base_df$natural_gas_afue == 925, 92.5)
results_base_df$natural_gas_afue <- replace(results_base_df$natural_gas_afue,is.na(results_base_df$natural_gas_afue),0)

results_base_df$propane_afue <- as.numeric(gsub("\\D", "", results_base_df$building_characteristics_report.hvac_system_heating_propane))
results_base_df$propane_afue <- replace(results_base_df$propane_afue,is.na(results_base_df$propane_afue),0)

results_base_df$electricity_afue <- with(results_base_df, ifelse(building_characteristics_report.heating_fuel == "Electricity", 100, 0))

results_base_df <- mutate(results_base_df, afue = oil_afue + natural_gas_afue + propane_afue + electricity_afue)  

#Keep houses where the afue does not equal zero
results_base_df <- subset(results_base_df, afue != 0) 
#Create df
fuel_consumption_df <- data.frame(matrix(ncol = 8, nrow = 8736))
building_summary2 <- data.frame(matrix(ncol = 9, nrow = nrow(building_coeff)))
building_summary3 <- data.frame(matrix(ncol = 2, nrow = nrow(building_coeff)))
colnames(fuel_consumption_df) <- c("Fuel_Oil", "Propane", "Natural_Gas","Electricity","Fuel_Oil_Base", "Propane_Base", "Natural_Gas_Base","Electricity_Base")
colnames(building_summary2) <- c('year', 'scenario','building_id', 'heating_gt',  'cooling_gt',  'heating_electricity_gt', 'cooling_electricity_gt', 'electricity_base', 'total_electricity_gt')
colnames(building_summary3) <- c('building_id', 'total_electricity')

predictions_list <- list()

agg_data <- as.data.frame(matrix(0, ncol=5, nrow=8736))
colnames(agg_data) <- c('y_pred', 'elec_pred', 'cooling_pred','heating_pred', 'baseline')

i=1

df_summary <- as.data.frame(matrix(0, ncol=1, nrow=8736))
colnames(df_summary) <- c('y_pred')

df_summary2 <- as.data.frame(matrix(0, ncol=1, nrow=8736))
colnames(df_summary2) <- c('y_gt')


#weather_name <- paste(directory_name, '/Weather_CSV/current/', city_name,'_current_weather.csv', sep="")
weather_name <- paste(directory_name, '/Weather_CSV/Energy_Burdens/', city_name,'_historical_1980-2020_2019.csv', sep="")
weather_data <- read.csv(weather_name,header=TRUE)
new_test <- subset(weather_data, select = c(1,3,5,6)) #changed from c(1:3) to c(1:4)

agg_data <- as.data.frame(matrix(0, ncol=7, nrow=8736))
colnames(agg_data) <- c('y_pred', 'elec_pred', 'cooling_pred','heating_pred', 'cooling_elec','heating_elec', 'baseline')
agg_data2 <- as.data.frame(matrix(0, ncol=7, nrow=8736))
colnames(agg_data2) <- c( 'net_gt', 'elec_gt', 'cooling_gt','heating_gt', 'cooling_elec_gt','heating_elec_gt','baseline')

year <- 'TMY3'
scenario <- 'Current'
i=1
for (building in building_id$building_id) {

  
  path <- file.path(root_dir,paste('project_singlefamily_detached_',city_name,'/localResults',sep=""),results_base_df$`X_id`[i],'/data_point.zip')
  h_8760_setback_df <- read_csv(unzip(path, "enduse_timeseries.csv"))
  
  if (results_base_df$building_characteristics_report.hvac_system_heating_electricity[i]=='None' & results_base_df$building_characteristics_report.hvac_system_heat_pump[i]=='None'){
    fuel_consumption_df$Electricity_Base <- h_8760_setback_df$total_site_electricity_kwh[1:8736]-h_8760_setback_df$electricity_cooling_kwh[1:8736]
  } else {
    fuel_consumption_df$Electricity_Base <- h_8760_setback_df$total_site_electricity_kwh[1:8736]-h_8760_setback_df$electricity_heating_kwh[1:8736]-h_8760_setback_df$electricity_cooling_kwh[1:8736]

  }
  
  fuel_consumption_df$Natural_Gas_Base <- h_8760_setback_df$total_site_natural_gas_therm[1:8736]-h_8760_setback_df$natural_gas_heating_therm[1:8736]
  fuel_consumption_df$Fuel_Oil_Base  <- h_8760_setback_df$total_site_fuel_oil_mbtu[1:8736]-h_8760_setback_df$fuel_oil_heating_mbtu[1:8736]
  fuel_consumption_df$Propane_Base <- h_8760_setback_df$total_site_propane_mbtu[1:8736]-h_8760_setback_df$propane_heating_mbtu[1:8736]
  
  h_8760_setback_df$inside_kelvin <- (h_8760_setback_df$`ZONE MEAN AIR TEMPERATURE (LIVING ZONE) [F]` - 32)*(5/9) + 273.15
  #generate new testing data
  new_test$delta_kelvin <- h_8760_setback_df$inside_kelvin[1:8736] - weather_data$p_Ambient_K
  new_test$delta_kelvin_wind <- new_test$delta_kelvin*weather_data$p_Wind
  new_test$p_Solar  <- weather_data$p_Solar
  new_test$delta_kelvin_transient <- c(0, diff(h_8760_setback_df$inside_kelvin[1:8736]))
  
  #get hourly net heating demand from coefficient
  df_summary$y_pred <- (building_coeff$intercept[building_coeff$building_id == building] + building_coeff$delta_kelvin[building_coeff$building_id == building]*new_test$delta_kelvin + building_coeff$delta_kelvin_wind[building_coeff$building_id == building]*new_test$delta_kelvin_wind + building_coeff$solar[building_coeff$building_id == building]*new_test$p_Solar + building_coeff$HIRI[building_coeff$building_id == building]*new_test$p_HIRI + building_coeff$relative_humidity[building_coeff$building_id == building]*new_test$p_Humidity + building_coeff$delta_kelvin_transient[building_coeff$building_id == building]*new_test$delta_kelvin_transient)*building_coeff$range_kWh[building_coeff$building_id == building] + building_coeff$min_demand_kWh[building_coeff$building_id == building]
  
  #heating_pred <-numeric(length(df_summary$y_pred))
  #cooling_pred <- numeric(length(df_summary$y_pred))
  
  heating_pred <- ifelse(df_summary$y_pred > 0, df_summary$y_pred, 0)
  cooling_pred <-ifelse(df_summary$y_pred < 0, df_summary$y_pred, 0)
  
  a=results_base_df$afue[building_id$building_id == building]
  
  fuel_oil_heating_mbtu <-numeric(length(df_summary$y_pred))
  propane_heating_mbtu <- numeric(length(df_summary$y_pred))
  natural_gas_heating_therm <-numeric(length(df_summary$y_pred))
  heating_elec_kWh <- numeric(length(df_summary$y_pred))
  cooling_elec_kWh <- numeric(length(df_summary$y_pred))
  
  fuel_oil_heating_mbtu[results_base_df$building_characteristics_report.heating_fuel[building_id$building_id == building]=="Fuel Oil"] <- heating_pred/(results_base_df$afue[building_id$building_id == building]/100)/0.29307
  propane_heating_mbtu[results_base_df$building_characteristics_report.heating_fuel[building_id$building_id == building]=="Propane"] <- heating_pred/(results_base_df$afue[building_id$building_id == building]/100)/0.29307
  natural_gas_heating_therm[results_base_df$building_characteristics_report.heating_fuel[building_id$building_id == building]=="Natural Gas"]  <- heating_pred/(results_base_df$afue[building_id$building_id == building]/100)/29.30
  
  heating_elec_kWh[results_base_df$building_characteristics_report.hvac_system_heating_electricity[building_id$building_id == building] == "Electric Furnace"] <- heating_pred
  heating_elec_kWh[results_base_df$building_characteristics_report.hvac_system_heating_electricity[building_id$building_id == building] == "Electric Baseboard"] <- heating_pred
  
  
  heating_COP <- rep(0, nrow(new_test))
  
  heating_COP[results_base_df$building_characteristics_report.hvac_system_heat_pump[building_id$building_id == building] == "ASHP, SEER 10, 6.2 HSPF"] <- 3.8 - (0.05*new_test$delta_kelvin) 
  heating_COP[results_base_df$building_characteristics_report0.hvac_system_heat_pump[building_id$building_id == building] == "ASHP, SEER 13, 7.7 HSPF"] <- 4.4 - (0.063*new_test$delta_kelvin) 
  heating_COP[results_base_df$building_characteristics_report.hvac_system_heat_pump[building_id$building_id == building] == "ASHP, SEER 15, 8.5 HSPF"] <- 4.6 - (0.065*new_test$delta_kelvin) 
  heating_COP[heating_COP<1] <- 1 #Min COP 1
  
  heating_elec_kWh[results_base_df$building_characteristics_report.hvac_system_heat_pump[building_id$building_id == building] == "ASHP, SEER 10, 6.2 HSPF"] <-   abs(heating_pred/heating_COP)
  heating_elec_kWh[results_base_df$building_characteristics_report.hvac_system_heat_pump[building_id$building_id == building] == "ASHP, SEER 13, 7.7 HSPF"] <-   abs(heating_pred/heating_COP)
  heating_elec_kWh[results_base_df$building_characteristics_report.hvac_system_heat_pump[building_id$building_id == building] == "ASHP, SEER 15, 8.5 HSPF"] <-   abs(heating_pred/heating_COP)
  
  
  cooling_COP <- rep(0, nrow(new_test)) #new 5/30
  cooling_COP[results_base_df$building_characteristics_report.hvac_system_cooling[building_id$building_id == building] == "FIXME Room AC, EER 8.5, 20% Conditioned"] <- 4.010756 + (0.1750727*new_test$delta_kelvin) 
  cooling_COP[results_base_df$building_characteristics_report.hvac_system_cooling[building_id$building_id == building] == "FIXME Room AC, EER 10.7, 20% Conditioned"] <- 4.196755 + (0.1341632*new_test$delta_kelvin)
  cooling_COP[results_base_df$building_characteristics_report.hvac_system_cooling[building_id$building_id == building] == "AC, SEER 8"] <- 3.840886 + (0.1493123*new_test$delta_kelvin)
  cooling_COP[results_base_df$building_characteristics_report.hvac_system_cooling[building_id$building_id == building] == "AC, SEER 15"] <- 5.052402 + (0.1400393*new_test$delta_kelvin)
  cooling_COP[results_base_df$building_characteristics_report.hvac_system_cooling[building_id$building_id == building] == "AC, SEER 13"] <- 4.527428 + (0.1356946*new_test$delta_kelvin)
  cooling_COP[results_base_df$building_characteristics_report.hvac_system_cooling[building_id$building_id == building] == "AC, SEER 10"] <- 4.010756 + (0.1750727*new_test$delta_kelvin)
  cooling_COP[results_base_df$building_characteristics_report.hvac_system_heat_pump[building_id$building_id == building] == "ASHP, SEER 15, 8.5 HSPF"] <- 4 + (0.18*new_test$delta_kelvin)
  cooling_COP[results_base_df$building_characteristics_report.hvac_system_heat_pump[building_id$building_id == building] == "ASHP, SEER 13, 7.7 HSPF"] <- 4.2 + (0.11*new_test$delta_kelvin)
  cooling_COP[results_base_df$building_characteristics_report.hvac_system_heat_pump[building_id$building_id == building] == "ASHP, SEER 10, 6.2 HSPF"] <- 5.1 + (0.14*new_test$delta_kelvin)
  
  cooling_COP[cooling_COP<1] <- 1 #Min COP 1
  
  cooling_elec_kWh <- abs(cooling_pred/cooling_COP)
  
  #total_elec_kwh <- heating_elec_kWh+cooling_elec_kWh+test_data$baseline

  fuel_consumption_df$Fuel_Oil <- fuel_oil_heating_mbtu
  fuel_consumption_df$Propane <- propane_heating_mbtu
  fuel_consumption_df$Natural_Gas <- natural_gas_heating_therm
  fuel_consumption_df$Electricity <-  heating_elec_kWh+cooling_elec_kWh
  
  #fuel_consumption_df$Fuel_Oil_Base <- test_data$oil_baseline
  #fuel_consumption_df$Propane_Base <- test_data$propane_baseline
  #fuel_consumption_df$Natural_Gas_Base <- test_data$gas_baseline
  #fuel_consumption_df$Electricity_Base <-  test_data$baseline
  if (building==203){
    a=1}
 
  #building_summary3[i,] <- c(building,df_summary$elec_pred)
  predictions_list[[as.character(building)]] <-   fuel_consumption_df 
  
  i = i+1
}

#saveRDS(predictions_list, "predictions_CHHW.rds")

# Define the path variable

# Save the RDS file using the variable
saveRDS(predictions_list, file.path(output_path, "predictions_CHHW.rds"))

