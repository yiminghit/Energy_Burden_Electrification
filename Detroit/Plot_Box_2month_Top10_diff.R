# Load required libraries
library(ggplot2)
library(gridExtra)

# Read the RDS files
city <- "Detroit"
routine_path <- "D:/Umich_Project/Experiments/"
output_path <- paste(routine_path, city, '/', sep="")

CHHWCHFW_list<- readRDS(file.path(output_path, "CHHW_CHFW12months.rds"))



CHHWFHHW_list <- readRDS(file.path(output_path, "CHHW_FHHW12months.rds"))

CHHWFHFW_list <- readRDS(file.path(output_path, "CHHW_FHFW12months.rds"))

# Define file paths for saving plots
#file_path_top10 <- file.path(output_path, "Figures", "top_10_percent_12months.png")
file_path_top10 <- file.path(output_path, "Figures", paste(city, "_top_10_2months_diff.png", sep=""))
# Create a list to store individual plots for the top 10% data
top10_plot_list <- list()

# Define the months to plot and their corresponding names
months_to_plot <- c(1, 7)  # January, April, July, October
month_names <- c("January", "July")

# Loop through each selected month
for (i in seq_along(months_to_plot)) {
  month <- months_to_plot[i]
  month_name <- month_names[i]
  
  # Create a data frame for the current month's data
  scenario_order <- c("CHHW→CHFW", "CHHW→FHHW", "CHHW→FHFW")
  
  # Create the boxplot_data data frame with specified scenario order
  boxplot_data <- data.frame(
    scenario = factor(rep(scenario_order, each = length(CHHWCHFW_list[[month]])), levels = scenario_order),
    Value = c(CHHWCHFW_list[[month]], CHHWFHHW_list[[month]], CHHWFHFW_list[[month]])
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
  
  # Print the median for the top 10% of each scenario
  cat(paste(month_name, "Top 10% Median Energy Burden:\n"))
  medians <- tapply(top_10_data_combined$Value, top_10_data_combined$scenario, median)
  print(medians)
  cat("\n")
  
  # Create a plot for the top 10% data for the current month's data
  top10_plot_list[[month_name]] <- ggplot(top_10_data_combined, aes(x = scenario, y = Value, fill = scenario, group = scenario)) +
    geom_boxplot(outlier.size = 0.5, outlier.shape = 16) + # Refine outliers
    ggtitle(paste(month_name)) +
   # ylab("Tail Energy Burden") +
    scale_fill_manual(values = color_palette) +  # Apply custom color palette
    coord_cartesian(ylim = c(-1, 1)) +
    theme_minimal(base_size = 14) +  # Clean theme
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Centered, bold title
      axis.title.x = element_text(size = 14),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Rotate x-axis labels for readability
      axis.text.y = element_text(size = 12),
      panel.grid.major.x = element_blank(),  # Remove vertical grid lines for a cleaner look
      panel.grid.major.y = element_line(color = "grey80"),  # Light grey horizontal grid lines for easier comparison
      panel.grid.minor = element_blank(),
      legend.position = "none"  # Remove legend for simplicity
    )
  if (i == 1) {
    top10_plot_list[[month_name]] <- top10_plot_list[[month_name]] +
      ylab("Tail Energy Burden") # Label only on the first boxplot
  } else {
    top10_plot_list[[month_name]] <- top10_plot_list[[month_name]] +
      ylab("") # No label for the other plots
  }
}

# Combine the boxplots into a single figure with 4 columns and 1 row
combined_plot <- grid.arrange(grobs = top10_plot_list, ncol = 2)

# Create the city title "Detroit" as a text grob
title_grob <- textGrob(city, x = 0.01, y = 0.95, gp = gpar(fontsize = 20, fontface = "bold"), hjust = 0, vjust = 1)

# Combine the title and the plot
final_plot <- arrangeGrob(title_grob, combined_plot, heights = c(0.1, 1))

# Save the final plot with the city title and plots
#file_path_top10 <- "top10_combined_plot_with_city.png"
ggsave(file_path_top10, plot = final_plot, width = 10, height = 3, dpi = 400)
