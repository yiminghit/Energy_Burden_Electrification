# Define the base directory and city


# List of all the R scripts to source
scripts <- c(
#  "Step3c_FutureHVAC_HistoricalWeather.R"
 # "Step3d_FutureHVAC_FutureWeather.R"
  "Step3b_CurrentHVAC_FutureWeather.R"
)

# Loop through the scripts and source each one
for (script in scripts) {
  # Define the base directory and city
  base_dir <- "D:/Umich_Project/Experiments/"
  city <- "Phoenix"
  
  # Combine base directory and city to set the project directory
  project_dir <- file.path(base_dir, city)
  # Set working directory each time before sourcing the script
  setwd(project_dir)
 # cat("Sourcing script:", script, "\n")
  print(script)
  # Source the script
  source(file.path(project_dir, script))
  print("file saved")
}
