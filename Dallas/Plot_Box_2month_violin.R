# Load required libraries
library(ggplot2)

library(gridExtra)

library(grid)
# Read the RDS file
#city <- "Dallas"
routine_path <- "D:/Umich_Project/Experiments/"
output_path <-paste(routine_path, city, '/', sep="")
# Read the RDS file

summary_all<- readRDS(file.path(output_path, "CHFW12months.rds"))

FHHW_list <- readRDS(file.path(output_path, "FHHW12months.rds"))
CHHW_list <- readRDS(file.path(output_path, "CHHW12months.rds"))

summary_all2 <- readRDS(file.path(output_path, "FHFW12months.rds"))


CHFW_list1 <- summary_all[['2050']]$rcp45cooler
CHFW_list2 <- summary_all[['2050']]$rcp85cooler

FHFW_list1 <- summary_all2[['2050']]$rcp45cooler
FHFW_list2 <- summary_all2[['2050']]$rcp85cooler


# Define the city name


file_path1 <- file.path(output_path,  "Figures", paste(city,"_box_plot_2months_all.png", sep=""))

#file_path2 <- file.path(output_path, "Figures", "box_plot_12months_all2.png")
# Define the file path
#file_path1 <- file.path("C:/Users/yangl11/R_Project/ACS_DATA", city, "Figures", "box_plot_12months_all1.png")
#file_path2 <- file.path("C:/Users/yangl11/R_Project/ACS_DATA", city, "Figures", "box_plot_12months_all2.png")

# Create a list to store individual boxplots
boxplot_list <- list()
#png("C:/Users/yangl11/R_Project/ACS_DATA/Detroit/combined_boxplots_whole.png", width = 3200, height = 2400, res = 300)  # Adjust width, height, and resolution as needed
boxplot_list <- list()
months_to_plot <- c(1, 7)
# Loop through each month (1 to 12)
month_names <- c("January",  "July")

# Custom color palette (muted tones for a professional look)
color_palette <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")

# Create a data frame to store medians and IQRs
summary_stats <- data.frame(
  Month = character(),
  Scenario = character(),
  Median = numeric(),
  Q1= numeric(),
  Q3= numeric(),
  IQR = numeric(),
  Q0= numeric(),
  Q4= numeric(),
  IQR2 = numeric(),
  stringsAsFactors = FALSE
)

# Loop through the selected months and their names
for (i in seq_along(months_to_plot)) {
  month <- months_to_plot[i]
  month_name <- month_names[i]
  
  # Define scenario order
  scenario_order <- c("CHHW", "CHFW_cool", "CHFW_hot", "FHHW", "FHFW_cool", "FHFW_hot")
  
  # Create the boxplot_data data frame with specified scenario order
  boxplot_data <- data.frame(
    scenario = factor(rep(scenario_order, each = length(CHFW_list1[[month]])), levels = scenario_order),
    Value = c(CHHW_list[[month]], CHFW_list1[[month]], CHFW_list2[[month]], FHHW_list[[month]], FHFW_list1[[month]], FHFW_list2[[month]])
  )
  
  # Calculate medians and IQRs
  medians <- tapply(boxplot_data$Value, boxplot_data$scenario, median)
  q1s <- tapply(boxplot_data$Value, boxplot_data$scenario, quantile, probs = 0.25)
  q3s <- tapply(boxplot_data$Value, boxplot_data$scenario, quantile, probs = 0.75)
  
  q0s <- tapply(boxplot_data$Value, boxplot_data$scenario, quantile, probs = 0.05)
  q4s <- tapply(boxplot_data$Value, boxplot_data$scenario, quantile, probs = 0.95)
  
  
  # Print the results
  cat(paste(month_name, "Median Energy Burden:\n"))
  print(medians)
  cat("\n")
  
  cat(paste(month_name, "Interquartile Range (IQR):\n"))
  print(q3s - q1s)
  cat("\n")
  
  # Append results to summary_stats data frame
  for (scenario in names(medians)) {
    summary_stats <- rbind(summary_stats, data.frame(
      Month = month_name,
      Scenario = scenario,
      Median = medians[scenario],
      Q1 = q1s[scenario],
      Q3 = q3s[scenario],
      IQR = q3s[scenario] - q1s[scenario],
      Q0 = q0s[scenario],
      Q4 = q4s[scenario],
      IQR2 = q4s[scenario] - q0s[scenario]
    ))
  }
  
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
    coord_cartesian(ylim = c(0, 0.3)) +
    theme_minimal(base_size = 18) +
    ggtitle(NULL) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),  # Enlarged month title
      axis.title.x = element_blank(),  # Remove X-axis title
      axis.text.x = element_text(size = 18, angle = 45, hjust = 1),  # Rotate X-axis labels
     # axis.text.x = element_blank(),
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

# Save medians and IQRs to a CSV file
summary_csv_path <- file.path(output_path, "monthly_summary_stats.csv")
write.csv(summary_stats, summary_csv_path, row.names = FALSE)
# Create the title "Detroit" as a text grob
title_grob <- textGrob(city, gp = gpar(fontsize = 20, fontface = "bold"), just = "left")

# Combine the boxplots into a single figure with 4 columns and 1 row
combined_plot <- grid.arrange(grobs = boxplot_list, ncol = 2)

# Combine the title "Detroit" with the boxplots in one layout
#final_plot <- grid.arrange(title_grob, combined_plot, ncol = 1, heights = c(0.1, 1))

# Add "Detroit" to the top-left corner by adding a title grob above the plot
title_grob <- textGrob(city, x = 0.01, y = 0.95, gp = gpar(fontsize = 20, fontface = "bold"), hjust = 0, vjust = 1)

# Combine the title and the plot
final_plot <- arrangeGrob(title_grob, combined_plot, heights = c(0.1, 1))

#dev.off()
ggsave(file_path1, plot= final_plot, width = 10, height = 4.5, dpi  = 600)
