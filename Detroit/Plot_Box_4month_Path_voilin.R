# Load required libraries
library(ggplot2)

library(gridExtra)

library(grid)
# Read the RDS file
city <- "Detroit"
routine_path <- "D:/Umich_Project/Experiments/"
output_path <-paste(routine_path, city, '/', sep="")
# Read the RDS file



FHHW_list <- readRDS(file.path(output_path, "FHHW12months_Gas.rds"))


CHHW_list <- readRDS(file.path(output_path, "CHHW12months_Gas.rds"))




CHFW_list1 <- readRDS(file.path(output_path, "CHHW12months_Propane.rds"))
CHFW_list2 <- readRDS(file.path(output_path, "CHHW12months_Electricity.rds"))

FHFW_list1 <- readRDS(file.path(output_path, "FHHW12months_Propane.rds"))
FHFW_list2 <- readRDS(file.path(output_path, "FHHW12months_Electricity.rds"))


# Define the city name


file_path1 <- file.path(output_path,  "Figures", paste(city,"_box_plot_4months_all_Two.png", sep=""))

#file_path2 <- file.path(output_path, "Figures", "box_plot_12months_all2.png")
# Define the file path
#file_path1 <- file.path("C:/Users/yangl11/R_Project/ACS_DATA", city, "Figures", "box_plot_12months_all1.png")
#file_path2 <- file.path("C:/Users/yangl11/R_Project/ACS_DATA", city, "Figures", "box_plot_12months_all2.png")

# Create a list to store individual boxplots
boxplot_list <- list()
#png("C:/Users/yangl11/R_Project/ACS_DATA/Detroit/combined_boxplots_whole.png", width = 3200, height = 2400, res = 300)  # Adjust width, height, and resolution as needed
boxplot_list <- list()
months_to_plot <- c(1, 4, 7, 10)
# Loop through each month (1 to 12)
month_names <- c("January", "April", "July", "October")

color_palette <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")
summary_stats_list <- list()
# Loop through the selected months and their names
for (i in seq_along(months_to_plot)) {
  month <- months_to_plot[i]
  month_name <- month_names[i]
  
  # Create a scenario order
  scenario_order <- c("CHHW_Gas", "CHHW_Propane, Oil", "CHHW_Electricity", "FHHW_Gas", "FHHW_Propane, Oil", "FHHW_Electricity")
  
  # Create the scenario column with each scenario repeated according to the length of its corresponding list
  scenario <- factor(c(
    rep(scenario_order[1], length(CHHW_list[[month]])),
    rep(scenario_order[2], length(CHFW_list1[[month]])),
    rep(scenario_order[3], length(CHFW_list2[[month]])),
    rep(scenario_order[4], length(FHHW_list[[month]])),
    rep(scenario_order[5], length(FHFW_list1[[month]])),
    rep(scenario_order[6], length(FHFW_list2[[month]]))
  ), levels = scenario_order)
  
  # Create a data frame for the current month's data
  boxplot_data <- data.frame(
    scenario = scenario,
    Value = c(CHHW_list[[month]], CHFW_list1[[month]], CHFW_list2[[month]], FHHW_list[[month]], FHFW_list1[[month]], FHFW_list2[[month]])
  )
  # Calculate summary statistics: Median, Q1, and Q3
  summary_stats <- data.frame(
    month = month_name,
    scenario = levels(boxplot_data$scenario),
    median = tapply(boxplot_data$Value, boxplot_data$scenario, median),
    Q1 = tapply(boxplot_data$Value, boxplot_data$scenario, function(x) quantile(x, probs = 0.25, na.rm = TRUE)),
    Q3 = tapply(boxplot_data$Value, boxplot_data$scenario, function(x) quantile(x, probs = 0.75, na.rm = TRUE)),
    Q0 = tapply(boxplot_data$Value, boxplot_data$scenario, function(x) quantile(x, probs = 0.05, na.rm = TRUE)),
    Q4 = tapply(boxplot_data$Value, boxplot_data$scenario, function(x) quantile(x, probs = 0.95, na.rm = TRUE))
  )
  
  # Append to the summary statistics list
  summary_stats_list[[month_name]] <- summary_stats
  
  # Print the median energy burden for the current month
  cat(paste(month_name, "Median Energy Burden:\n"))
  medians <- tapply(boxplot_data$Value, boxplot_data$scenario, median)
  print(medians)
  cat("\n")
  
  # Create a violin plot with horizontal boxplot-like lines
  boxplot_list[[month_name]] <- ggplot(boxplot_data, aes(x = scenario, y = Value, fill = scenario, group = scenario)) +
    geom_violin(scale = "width", alpha = 0.8) +  # Violin plot
    # Add boxplot-like lines
    stat_summary(fun = "median", geom = "crossbar", width = 0.4, color = "black", fatten = 2) +  # Median line
    stat_summary(fun.min = function(x) quantile(x, probs = 0.25), 
                 fun.max = function(x) quantile(x, probs = 0.75), 
                 geom = "errorbar", width = 0.4, color = "black") +  # Q1 and Q3 lines
    ggtitle(paste(month_name)) +
    scale_fill_manual(values = color_palette) +  # Apply custom color palette
    coord_cartesian(ylim = c(0, 0.4)) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),  # Enlarged month title
      axis.title.x = element_blank(),  # Remove X-axis title
     # axis.text.x = element_blank(),
      axis.text.x = element_text(size = 18, angle = 45, hjust = 1),  # Rotate X-axis labels
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey80"),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")  # Further reduce plot margins
    )
  
  # Add the Y-axis ticks and label only for the first (left-most) plot
  if (i == 1) {
    boxplot_list[[month_name]] <- boxplot_list[[month_name]] +
      ylab("Energy Burden") +
      theme(
        # axis.ticks.y = element_line(size = 1.5),  # Thicker Y-axis ticks
        axis.ticks.length = unit(0.3, "cm"),     # Longer Y-axis ticks
        axis.text.y = element_text(size = 18),   # Larger Y-axis tick labels
        axis.title.y = element_text(size = 18)  # Larger, bold Y-axis label
      )
  } else {
    boxplot_list[[month_name]] <- boxplot_list[[month_name]] +
      ylab("") +
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
  }
}

all_summary_stats <- do.call(rbind, summary_stats_list)

# Save the summary statistics to a CSV file
csv_file_path <- file.path(output_path, "parth_summary_stats.csv")
write.csv(all_summary_stats, csv_file_path, row.names = FALSE)

# Create the title "Detroit" as a text grob
title_grob <- textGrob(city, gp = gpar(fontsize = 20, fontface = "bold"), just = "left")

# Combine the boxplots into a single figure with 4 columns and 1 row
combined_plot <- grid.arrange(grobs = boxplot_list, ncol = 4)

# Add "Detroit" to the top-left corner by adding a title grob above the plot
title_grob <- textGrob(city, x = 0.01, y = 0.95, gp = gpar(fontsize = 20, fontface = "bold"), hjust = 0, vjust = 1)

# Combine the title and the plot
final_plot <- arrangeGrob(title_grob, combined_plot, heights = c(0.1, 1))

# Save the final plot
ggsave(file_path1, plot = final_plot, width = 20, height = 5, dpi = 600)