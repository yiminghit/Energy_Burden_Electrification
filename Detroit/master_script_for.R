# Define the base directory and city
base_dir <- "D:/Umich_Project/Experiments/"
city <- "Detroit"

# Combine base directory and city to set the project directory
project_dir <- file.path(base_dir, city)

# List of all the R scripts to source
scripts <- c(
  "Step3a_CurrentHVAC_HistoricalWeather.R",
  "Step3b_CurrentHVAC_FutureWeather.R",
  "Step3c_FutureHVAC_HistoricalWeather.R",
  "Step3d_FutureHVAC_FutureWeather.R",
  "Stepn_Energy_Burden_CHFW.R",
  "Stepn_Energy_Burden_CHHW.R",
  "Stepn_Energy_Burden_FHFW.R",
  "Stepn_Energy_Burden_FHHW.R",
  "Stepn_Energy_Burden_CHFW12months.R",
  "Stepn_Energy_Burden_CHHW12months.R",
  "Stepn_Energy_Burden_FHFW12months.R",
  "Stepn_Energy_Burden_FHHW12months.R",
  "Stepn_Energy_Burden_CHHW12months_Electricity.R",
  "Stepn_Energy_Burden_CHHW12months_Gas.R",
  "Stepn_Energy_Burden_CHHW12months_Propane.R",
  "Stepn_Energy_Burden_FHHW12months_Electricity.R",
  "Stepn_Energy_Burden_FHHW12months_Gas.R",
  "Stepn_Energy_Burden_FHHW12months_Propane.R",
  "Plot_Box_1year.R",
  "Plot_Box_1year_Top10.R",
  "Plot_Box_4month.R",
  "Plot_Box_4month_top10.R",
  "Plot_Box_4month_Path.R"
)

# Loop through the scripts and source each one
for (script in scripts) {
  # Define the base directory and city
  base_dir <- "D:/Umich_Project/Experiments/"
  city <- "Detroit"
  
  # Combine base directory and city to set the project directory
  project_dir <- file.path(base_dir, city)
  # Set working directory each time before sourcing the script
  setwd(project_dir)
  
  # Source the script
  source(file.path(project_dir, script))
}
