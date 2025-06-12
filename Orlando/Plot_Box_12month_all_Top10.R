# Load required libraries
library(ggplot2)
library(gridExtra)

# Read the RDS files
city <- "Orlando"
routine_path <- "D:/Umich_Project/Experiments/"
output_path <- paste(routine_path, city, '/', sep="")

summary_all <- readRDS(file.path(output_path, "CHFW12months.rds"))
FHHW_list <- readRDS(file.path(output_path, "FHHW12months.rds"))
CHHW_list <- readRDS(file.path(output_path, "CHHW12months.rds"))
summary_all2 <- readRDS(file.path(output_path, "FHFW12months.rds"))

CHFW_list1 <- summary_all[['2050']]$rcp45cooler
CHFW_list2 <- summary_all[['2050']]$rcp85hotter
FHFW_list1 <- summary_all2[['2050']]$rcp45cooler
FHFW_list2 <- summary_all2[['2050']]$rcp85hotter

# Define file paths for saving plots
#file_path_top10 <- file.path(output_path, "Figures", "top_10_percent_12months.png")
file_path_top10 <- file.path(output_path, "Figures", paste(city, "_top_10_12months.png", sep=""))
# Create a list to store individual plots for the top 10% data
top10_plot_list <- list()

# Loop through each month (1 to 12)
for (month in 1:12) {
  # Define scenario order
  scenario_order <- c("CHHW", "CHFW_cool", "CHFW_hot", "FHHW", "FHFW_cool", "FHFW_hot")
  
  # Create the data frame with specified scenario order
  boxplot_data <- data.frame(
    scenario = factor(rep(scenario_order, each = length(CHFW_list1[[month]])), levels = scenario_order),
    Value = c(CHHW_list[[month]], CHFW_list1[[month]], CHFW_list2[[month]], FHHW_list[[month]], FHFW_list1[[month]], FHFW_list2[[month]])
  )
  
  # Calculate the 90th percentile for each scenario
  top_10_data_list <- lapply(scenario_order, function(scenario) {
    current_data <- boxplot_data$Value[boxplot_data$scenario == scenario]
    quantile_value <- quantile(current_data, 0.9)
    top_10_data <- current_data[current_data > quantile_value]
    return(data.frame(scenario = scenario, Value = top_10_data))
  })
  
  # Combine the top 10% data for all scenarios in the current month
  top_10_data_combined <- do.call(rbind, top_10_data_list)

  top_10_data_combined$scenario <- factor(top_10_data_combined$scenario, levels = scenario_order)
  cat(paste("Month", month, "Top 10% Median Energy Burden:\n"))
  medians <- tapply(top_10_data_combined$Value, top_10_data_combined$scenario, median)
  print(medians)
  cat("\n")
  # Create a plot for the top 10% data for the current month's data
  top10_plot_list[[month]] <- ggplot(top_10_data_combined, aes(x = scenario, y = Value, fill = scenario,  group = scenario)) +
    geom_boxplot() +
    ggtitle(paste("Month", month, "")) +
    ylab("Tail Energy Burden") +
    #xlab("") +
    coord_cartesian(ylim = c(0, 0.8)) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Combine the plots for the top 10% data into a single figure
top10_combined_plot <- grid.arrange(grobs = top10_plot_list, ncol = 4)  # Adjust ncol according to your preference

# Save the combined plot for the top 10% data as a PNG with high resolution
ggsave(file_path_top10, plot = top10_combined_plot, width = 20, height = 8, dpi = 400)
