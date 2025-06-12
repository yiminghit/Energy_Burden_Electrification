# Load required libraries
library(ggplot2)
library(gridExtra)

# Read the RDS files
#city <- "Boston"
routine_path <- "D:/Umich_Project/Experiments/"
output_path <- paste(routine_path, city, '/', sep="")

summary_all <- readRDS(file.path(output_path, "CHFW12months.rds"))
FHHW_list <- readRDS(file.path(output_path, "FHHW12months.rds"))
CHHW_list <- readRDS(file.path(output_path, "CHHW12months.rds"))
summary_all2 <- readRDS(file.path(output_path, "FHFW12months.rds"))

CHFW_list1 <- summary_all[['2050']]$rcp45cooler
CHFW_list2 <- summary_all[['2050']]$rcp85cooler
FHFW_list1 <- summary_all2[['2050']]$rcp45cooler
FHFW_list2 <- summary_all2[['2050']]$rcp85cooler

# Define file paths for saving plots
#file_path_top10 <- file.path(output_path, "Figures", "top_10_percent_12months.png")
file_path_top10 <- file.path(output_path, "Figures", paste(city, "_top_10_2months.png", sep=""))
# Create a list to store individual plots for the top 10% data
top10_plot_list <- list()

# Define the months to plot and their corresponding names
months_to_plot <- c(1, 7)  # January, April, July, October
month_names <- c("January", "July")
color_palette <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")
# Loop through each selected month
# Initialize data frame for summary statistics
# Initialize data frame for summary statistics
top10_summary_stats <- data.frame(
  Month = character(),
  Scenario = character(),
  Median = numeric(),
  Q1 =  numeric(),
  Q3 = numeric(),
  IQR = numeric(),
  stringsAsFactors = FALSE
)

# Loop through the selected months and their names
# Loop through the selected months and their names
for (i in seq_along(months_to_plot)) {
  month <- months_to_plot[i]
  month_name <- month_names[i]
  
  # Define scenario order
  scenario_order <- c("CHHW", "CHFW_cool", "CHFW_hot", "FHHW", "FHFW_cool", "FHFW_hot")
  
  # Create the data frame with specified scenario order
  boxplot_data <- data.frame(
    scenario = factor(rep(scenario_order, each = length(CHFW_list1[[month]])), levels = scenario_order),
    Value = c(CHHW_list[[month]], CHFW_list1[[month]], CHFW_list2[[month]], FHHW_list[[month]], FHFW_list1[[month]], FHFW_list2[[month]])
  )
  
  # Calculate the 90th percentile and extract top 10% data for each scenario
  top_10_data_list <- lapply(scenario_order, function(scenario) {
    current_data <- boxplot_data$Value[boxplot_data$scenario == scenario]
    quantile_value <- quantile(current_data, 0.9, na.rm = TRUE)
    top_10_data <- current_data[current_data > quantile_value]
    return(data.frame(scenario = scenario, Value = top_10_data))
  })
  
  # Combine the top 10% data for all scenarios in the current month
  top_10_data_combined <- do.call(rbind, top_10_data_list)
  top_10_data_combined$scenario <- factor(top_10_data_combined$scenario, levels = scenario_order)
  
  # Calculate medians and IQRs for the top 10% data
  medians <- tapply(top_10_data_combined$Value, top_10_data_combined$scenario, median, na.rm = TRUE)
  q1s <- tapply(top_10_data_combined$Value, top_10_data_combined$scenario, quantile, probs = 0.25, na.rm = TRUE)
  q3s <- tapply(top_10_data_combined$Value, top_10_data_combined$scenario, quantile, probs = 0.75, na.rm = TRUE)
  
  # Print the median for the top 10% of each scenario
  cat(paste(month_name, "Top 10% Median Energy Burden:\n"))
  print(medians)
  cat("\n")
  
  # Print the IQR for the top 10% of each scenario
  cat(paste(month_name, "Top 10% IQR:\n"))
  print(q3s - q1s)
  cat("\n")
  
  # Append medians and IQRs to summary statistics
  for (scenario in names(medians)) {
    top10_summary_stats <- rbind(top10_summary_stats, data.frame(
      Month = month_name,
      Scenario = scenario,
      Median = medians[scenario],
      Q1 = q1s[scenario],
      Q3 = q3s[scenario],
      IQR = q3s[scenario] - q1s[scenario]
    ))
  }
  
  # Create a violin plot for the top 10% data with quartile lines
  top10_plot_list[[month_name]] <- ggplot(top_10_data_combined, aes(x = scenario, y = Value, fill = scenario, group = scenario)) +
    geom_violin(scale = "width", alpha = 0.8) +  # Violin plot for top 10%
    # Add boxplot-like horizontal lines for medians, Q1, and Q3
    stat_summary(fun = "median", geom = "crossbar", width = 0.4, color = "black", fatten = 2) +  # Median line
    stat_summary(fun.min = function(x) quantile(x, probs = 0.25, na.rm = TRUE), 
                 fun.max = function(x) quantile(x, probs = 0.75, na.rm = TRUE), 
                 geom = "errorbar", width = 0.4, color = "black") +  # Q1 and Q3 lines
    ggtitle(paste(month_name)) +
    scale_fill_manual(values = color_palette) +  # Apply custom color palette
    coord_cartesian(ylim = c(0, 1.05)) +
    theme_minimal(base_size = 18) +
    ggtitle(NULL) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),  # Enlarged month title
      axis.title.x = element_blank(),  # Remove X-axis title
     # axis.text.x = element_blank(), 
      axis.text.x = element_text(size = 18, angle = 45, hjust = 1), # Rotate X-axis labels
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey80"),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")  # Further reduce plot margins
    )
  
  # Add the Y-axis ticks and label only for the first (left-most) plot
  if (i == 1) {
    top10_plot_list[[month_name]] <- top10_plot_list[[month_name]] +
      ylab("Tail Energy Burden") +
      theme(
       # axis.ticks.y = element_line(size = 1.5),  # Thicker Y-axis ticks
        axis.ticks.length = unit(0.3, "cm"),     # Longer Y-axis ticks
        axis.text.y = element_text(size = 18),   # Larger Y-axis tick labels
        axis.title.y = element_text(size = 18)  # Larger, bold Y-axis label
      )
  } else {
    top10_plot_list[[month_name]] <- top10_plot_list[[month_name]] +
      ylab("") +
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
  }
}


# Save medians and IQRs for the top 10% data to a CSV file
top10_summary_csv_path <- file.path(output_path, "top10_monthly_summary_stats.csv")
write.csv(top10_summary_stats, top10_summary_csv_path, row.names = FALSE)



# Combine the boxplots into a single figure with 4 columns and 1 row
combined_plot <- grid.arrange(grobs = top10_plot_list, ncol = 2)

# Create the city title "Detroit" as a text grob
title_grob <- textGrob(city, x = 0.01, y = 0.95, gp = gpar(fontsize = 20, fontface = "bold"), hjust = 0, vjust = 1)

# Combine the title and the plot
final_plot <- arrangeGrob(title_grob, combined_plot, heights = c(0.1, 1))

# Save the final plot with the city title and plots
#file_path_top10 <- "top10_combined_plot_with_city.png"
ggsave(file_path_top10, plot = final_plot, width = 10, height = 4.5, dpi = 600)
