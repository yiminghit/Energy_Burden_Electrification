rm(list = ls())
library(dplyr)
library("tidyr")
library(progress)


city_name <-'Phoenix' #To update with the city to run
#directory_name <-'C:/Users/Shuhaib Nawawi/Dropbox (University of Michigan)/Building_RCM/RCM_June/' #Use dropbox path so it's all standardized
#directory_name <-'C:/Users/yangl11/Dropbox (University of Michigan)/Building_RCM/RCM_June/'
directory_name <-'C:/Users/yimin/Dropbox/RCM_June/'
directory_name2 <- 'D:/Umich_Project/Experiments/'

output_path <-paste(directory_name2, city_name, '/', sep="")

path_name1 <- paste(directory_name, 'RCM_Building_Coefficients_v2/', city_name, '_coeff.csv', sep="")

year_data <- list()
column_sums_list <- list()

setwd("D:/Umich_Project/Restock_Data")

#local Directory
root_dir = "D:/Umich_Project/Restock_Data"
results <- read.csv(paste('project_singlefamily_detached_',city_name,'/localResults/results.csv', sep=""), header = TRUE)

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

fuel_consumption_df <- data.frame(matrix(ncol = 8, nrow = 8736))
colnames(fuel_consumption_df) <- c("Fuel_Oil", "Propane", "Natural_Gas","Electricity","Fuel_Oil_Base", "Propane_Base", "Natural_Gas_Base","Electricity_Base")

#Read regression coefficients
building_coeff <- read.csv(path_name1, header=TRUE)

#Create df
building_summary <- data.frame(matrix(ncol = 9, nrow = nrow(building_coeff)))
colnames(building_summary) <- c('year', 'scenario', 'building_id', 'heating_prediction', 'cooling_prediction', 'heating_electricity', 'cooling_electricity', 'electricity_base', 'total_electricity')
predictions_list <- list()

agg_data <- as.data.frame(matrix(0, ncol=5, nrow=8736))
colnames(agg_data) <- c('y_pred', 'elec_pred', 'cooling_pred','heating_pred', 'baseline')

i=1

df_summary <- as.data.frame(matrix(0, ncol=1, nrow=8736))
colnames(df_summary) <- c('y_pred')

  
#weather_name <- paste(directory_name, '/Weather_CSV/current/', city_name,'_current_weather.csv', sep="")
#weather_data <- read.csv(weather_name,header=TRUE)
#new_test <- subset(weather_data, select = c(1:4)) #changed from c(1:3) to c(1:4)

weather_name <- paste(directory_name, '/Weather_CSV/Energy_Burdens/', city_name,'_historical_1980-2020_2019.csv', sep="")
weather_data <- read.csv(weather_name,header=TRUE)
new_test <- subset(weather_data, select = c(1,3,5,6)) #changed from c(1:3) to c(1:4)

agg_data <- as.data.frame(matrix(0, ncol=7, nrow=8736))

colnames(agg_data) <- c('y_pred', 'elec_pred', 'cooling_pred','heating_pred', 'cooling_elec','heating_elec', 'baseline')

year = 'TMY3'
scenario = 'All Electric'
      
    for (building in building_coeff$building_id) {
          
          
          path <- file.path(root_dir,paste('project_singlefamily_detached_',city_name,'/localResults',sep=""),results_base_df$`X_id`[i],'/data_point.zip')
          h_8760_setback_df <- read_csv(unzip(path, "enduse_timeseries.csv"))
          
          h_8760_setback_df$inside_kelvin <- (h_8760_setback_df$`ZONE MEAN AIR TEMPERATURE (LIVING ZONE) [F]` - 32)*(5/9) + 273.15
          if (results_base_df$building_characteristics_report.hvac_system_heating_electricity[i]=='None' & results_base_df$building_characteristics_report.hvac_system_heat_pump[i]=='None'){
            fuel_consumption_df$Electricity_Base <- h_8760_setback_df$total_site_electricity_kwh[1:8736]-h_8760_setback_df$electricity_cooling_kwh[1:8736]
          } else {
            fuel_consumption_df$Electricity_Base <- h_8760_setback_df$total_site_electricity_kwh[1:8736]-h_8760_setback_df$electricity_heating_kwh[1:8736]-h_8760_setback_df$electricity_cooling_kwh[1:8736]
            
          }
          
          fuel_consumption_df$Natural_Gas_Base <- h_8760_setback_df$total_site_natural_gas_therm[1:8736]-h_8760_setback_df$natural_gas_heating_therm[1:8736]
          fuel_consumption_df$Fuel_Oil_Base  <- h_8760_setback_df$total_site_fuel_oil_mbtu[1:8736]-h_8760_setback_df$fuel_oil_heating_mbtu[1:8736]
          fuel_consumption_df$Propane_Base <- h_8760_setback_df$total_site_propane_mbtu[1:8736]-h_8760_setback_df$propane_heating_mbtu[1:8736]
    
          #generate new testing data
          new_test$delta_kelvin <- h_8760_setback_df$inside_kelvin[1:8736] - weather_data$p_Ambient_K
          new_test$delta_kelvin_wind <- new_test$delta_kelvin*weather_data$p_Wind
          new_test$p_Solar  <- weather_data$p_Solar
          new_test$delta_kelvin_transient <- c(0, diff(h_8760_setback_df$inside_kelvin[1:8736]))
  
          #get hourly net heating demand from coefficient
          df_summary$y_pred <- (building_coeff$intercept[building_coeff$building_id == building] + building_coeff$delta_kelvin[building_coeff$building_id == building]*new_test$delta_kelvin + building_coeff$delta_kelvin_wind[building_coeff$building_id == building]*new_test$delta_kelvin_wind + building_coeff$solar[building_coeff$building_id == building]*new_test$p_Solar + building_coeff$HIRI[building_coeff$building_id == building]*new_test$p_HIRI + building_coeff$relative_humidity[building_coeff$building_id == building]*new_test$p_Humidity + building_coeff$delta_kelvin_transient[building_coeff$building_id == building]*new_test$delta_kelvin_transient)*building_coeff$range_kWh[building_coeff$building_id == building] + building_coeff$min_demand_kWh[building_coeff$building_id == building]

          #elec_pred <- as.data.frame(matrix(0, ncol=1, nrow=8760))
          pCOPHeat = 4.6 -0.071*(h_8760_setback_df$inside_kelvin[1:8736] -weather_data$p_Ambient_K)
          #mininum pCOPHeat pCOPColl should be one.
          pCOPCool = 4.3 -0.11*(weather_data$p_Ambient_K -h_8760_setback_df$inside_kelvin[1:8736])
          pCOPCool[pCOPCool<1] <- 1
          pCOPHeat[pCOPHeat<1] <- 1
          #colnames(elec_pred) <- 'electricity'
          df_summary$elec_pred <- 0

          #colnames(elec_pred) <- 'electricity'
          df_summary$pred_cooling<-0
          df_summary$pred_heating<-0
          df_summary$elec_cooling<-0
          df_summary$elec_heating<-0
          COP<-0
  
          df_summary$elec_pred <- ifelse(df_summary$y_pred > 0, (df_summary$y_pred / pCOPHeat), abs(df_summary$y_pred / pCOPCool))
            
          # Calculate elec_test using vectorized operations
          # Calculate test_heating, test_cooling, pred_heating, pred_cooling using vectorized operations
          df_summary$pred_cooling <- ifelse(df_summary$y_pred > 0, 0, df_summary$y_pred)
          df_summary$pred_heating <- ifelse(df_summary$y_pred > 0, df_summary$y_pred, 0)
          
          
          df_summary$elec_cooling <- ifelse(df_summary$y_pred > 0, 0, abs(df_summary$y_pred / pCOPCool))
          df_summary$elec_heating <- ifelse(df_summary$y_pred > 0, (df_summary$y_pred / pCOPHeat), 0)
          # Calculate COP using vectorized operations
          COP <- ifelse(df_summary$y_pred > 0, pCOPHeat, pCOPCool)
  
          df_summary$baseline <- fuel_consumption_df$Electricity_Base 
          agg_data <- agg_data + df_summary
          
          fuel_consumption_df$Fuel_Oil <- 0
          fuel_consumption_df$Propane <- 0
          fuel_consumption_df$Natural_Gas <- 0
          fuel_consumption_df$Electricity <-  df_summary$elec_pred
  
    #Calculate demands for each building
    heating_predicted <- sum(df_summary$y_pred[df_summary$y_pred > 0])
    cooling_predicted <- abs(sum(df_summary$y_pred[df_summary$y_pred < 0]))
    heating_electricity <- sum(df_summary$elec_pred[df_summary$y_pred > 0])
    cooling_electricity <- abs(sum(df_summary$elec_pred[df_summary$y_pred < 0]))
    Electricity_predicted <- heating_electricity + cooling_electricity
    Electricity_base <- sum(df_summary$baseline)
    Electricity_total <- Electricity_predicted + Electricity_base
    
    building_summary[i,] <- c(year, scenario, building, heating_predicted, cooling_predicted, heating_electricity,cooling_electricity, Electricity_base, Electricity_total)
    predictions_list[[as.character(building)]] <- fuel_consumption_df
    i = i+1
    print(i)
        }
      

# Define the path variable
#output_path <- "C:/Users/yangl11/R_Project/ACS_DATA/Detroit/"

# Save the RDS file using the variable
saveRDS(predictions_list, file.path(output_path, "predictions_FHHW.rds"))
print('saved file')

