# Updated May 25 (to export training & testing data)
rm(list = ls())
library(dplyr)
library("rjson")
library(epwshiftr)
library(eplusr)
library("tidyr")
library(readr)
source("http://klein.uk/R/myfunctions.R")
library(jsonlite)
library(lubridate)
setwd("D:/Umich_Project/Restock_Data")

### Set city to run ###
city_to_run <- "Detroit"
file_name <- "Detroit_random.csv"
#local Directory
root_dir = "D:/Umich_Project/Restock_Data"
routine_path <- "D:/Umich_Project/Experiments/"

results <- read.csv(paste('project_singlefamily_detached_',city_to_run,'/localResults/results.csv', sep=""), header = TRUE)
#saved dropbox directory
#directory_name <-'C:/Users/yangl11/R_Project/ACS_DATA/Seattle/'

# Read an EPW file downloaded from EnergyPlus website (MAKE SURE TO USE THE RIGHT EPW)
#epw <- read_epw("C:/Users/yangl11/R_Project/EPW_Data/USA_WA_Seattle-Tacoma.Intl.AP.727930_TMY3.epw")
###
## Base data wrangling
###
#directory_name2 <- 'D:/Umich_Project/Experiments/'
output_path <-paste(routine_path, city_to_run, '/', sep="")

save_path <- file.path(routine_path, city_to_run, "CHHW_one_year_gas.rds")
###
#save_path <- file.path("C:/Users/yangl11/R_Project/ACS_DATA", city_to_run, "CHHW_one_year.rds")
#Read in data with baseline simulation
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

#Keep base id
id_base <- results_base_df$X_id

###
## Upgrade data wrangling
###

#Get the upgrade data: upgrade 2 keeps the existing heater but has a twice-a-day thermostat setback to help simulate temporal temperature transients of the houses inside temperature
results_upgrade2 <- subset(results, apply_upgrade_2.run_measure == 1) #Use this for training
#results_upgrade2 <- subset(results, apply_upgrade_2.run_measure == 0 & apply_upgrade.run_measure == 0) #Use this for testing
id_upgrade2 <- subset(results_upgrade2, select = c(X_id,build_existing_model.building_id))

#Create a new set of results that uses the characteristics of base but then the _id of upgrade_2 so we get a profile where inside temperature is changing
results_setback <- results_base_df
results_setback <- left_join(results_setback, id_upgrade2, by = 'build_existing_model.building_id')
colnames(results_setback)[29] <- '_id_upgrade2'
colnames(results_setback)[1] <- '_id_base'

###
## EPW data wrangling
###


#EPW_df <- epw$data()
#EPW_df$hour <- c(1:8760)

heat_key_set <- c()
cool_key_set <- c()

#file_path <- "C:/Users/yangl11/R_Project/ACS_DATA/Detroit/Detroit_micro_group_P_5income2.csv"


file_path <- paste0(routine_path, city_to_run, "/", file_name)

# Read the CSV file into a DataFrame
df_ipf <- read.csv(file_path)
file_name2 <- 'predictions_CHHW.rds'
rds_file_path <-paste0(routine_path, city_to_run, "/", file_name2)
file_name3 <- 'calculate_energy_bill.R'
code_path <- paste0(routine_path, city_to_run, "/", file_name3)

#rds_file_path <- "D:/Umich_Project/Experiments/Seattle/predictions_CHFW.rds"
#source("D:/Umich_Project\Experiments/Seattle/calculate_energy_bill.R")
source(code_path)

# Read the CSV file into a DataFrame
#df_ipf <- read.csv(file_path)

#predictions_list <- readRDS("predictions_list.rds")
# Define the path to the RDS file
#rds_file_path <- "C:/Users/yangl11/R_Project/ACS_DATA/Detroit/predictions_CHHW.rds"
#source("C:/Users/yangl11/R_Project/ACS_DATA/Detroit/calculate_energy_bill.R")
# Read the RDS file
predictions_list <- readRDS(rds_file_path)
###
## RCM for net demand
###
i = 1
sample_size <-1000
results_list <- list()
combined_data <- numeric(0)
energy_bill_df <- data.frame(house_id = character(), energy_bill = numeric(), stringsAsFactors = FALSE)

filtered_results_setback <- results_setback[results_setback$building_characteristics_report.heating_fuel == "Natural Gas", ]

# Create an empty dataframe to store energy bills
energy_bill_df <- data.frame(house_id = character(), energy_bill = numeric(), stringsAsFactors = FALSE)

for (house in filtered_results_setback$`_id_base`) {
  
  path <- file.path(root_dir,paste('project_singlefamily_detached_',city_to_run,'/localResults',sep=""),filtered_results_setback$`_id_base`[i],'/data_point.zip')
  #h_8760_setback_df <- read_csv(unzip(path, "enduse_timeseries.csv"))
  
  #h_8760_setback_df$Time <- as.POSIXct(h_8760_setback_df$Time, format = "%Y/%m/%d %H:%M:%S", tz = "UTC")
  # Create a column to represent the months
  #h_8760_setback_df$Month <- as.numeric(format(h_8760_setback_df$Time, "%m"))

  energy_bill <- calculate_energy_bill(filtered_results_setback, predictions_list,i)
  
  print( energy_bill)
  # Specify the path to your JSON file
 # json_file_path <- "results.json"
  energy_bill_df <- rbind(energy_bill_df, data.frame(house_id = filtered_results_setback$`_id_base`[i], energy_bill = round(energy_bill, 2)))
  # Read the JSON data from the file
  json_data <- fromJSON(unzip(path, "results.json"))
  
  
  if(i ==33){
    a=1
  }
  
  # Access the "geometry_house_size" section
  geometry_house_size <- json_data$geometry_house_size
  house_size <- json_data$build_existing_model$geometry_house_size
  AC_type <- json_data$build_existing_model$hvac_system_cooling_type
  #Export testing data
  a <- c(filtered_results_setback$build_existing_model.building_id[i],  energy_bill,geometry_house_size,AC_type )
  range=round(energy_bill/c(15000,35000,75000, 100000),3)
  
  target_row <-round(as.numeric(df_ipf[df_ipf$House.Size == house_size & df_ipf$AC.Type == AC_type,4:8]),4)
  
  b <- character(5)
  
  b[1] <- paste0( ">", range[1])
  b[2] <- paste0(range[2], "-", range[1])
  b[3] <- paste0(range[3], "-", range[2])
  b[4] <- paste0(range[4], "-", range[3])
  b[5] <- paste0("<", range[4])
  
  result <- list(
    Building_ID = filtered_results_setback$build_existing_model.building_id[i],
    Energy_Bill = energy_bill,
    House_Size = house_size,
    AC_Type = AC_type,
    burden_range1 = b[5],
    burden_prob1 = target_row[5],
    burden_range2 = b[4],
    burden_prob2 = target_row[4],
    burden_range3 = b[3],
    burden_prob3 = target_row[3],
    burden_range4 = b[2],
    burden_prob4 = target_row[2],
    burden_range5 = b[1],
    burden_prob5 = target_row[1]
    
  )
  upper_bound =1
  if (range[1]>1) {
    upper_bound=range[1]+0.1
  }
  
  data_case = c(
    runif(floor(sample_size * target_row[5]), 0, range[4]),
    runif(floor(sample_size * target_row[4]), range[4], range[3]),
    runif(floor(sample_size * target_row[3]), range[3], range[2]),
    runif(floor(sample_size * target_row[2]), range[2], range[1]),
    runif(floor(sample_size * target_row[1]), range[1], upper_bound)
  )
  combined_data <- c(combined_data, data_case)
  # Append the result to the results_list3
  results_list[[i]] <- result
 
  print(filtered_results_setback[i,]$build_existing_model.building_id)
  progress <- i/count(filtered_results_setback)*100
  print(paste("Progress %: ", round(progress, digits=2)))
  i = i+1
}

output_csv_path <- file.path(root_dir, paste('energy_bills_', city_to_run, '.csv', sep=""))

# Write the energy bills to a CSV file
write.csv(energy_bill_df, file = output_csv_path, row.names = FALSE)

saveRDS(combined_data, file = save_path)


