# Load required libraries
library(ggplot2)

library(gridExtra)

library(grid)
# Read the RDS file
city <- "Detroit"
routine_path <- "D:/Umich_Project/Experiments/"
output_path <-paste(routine_path, city, '/', sep="")
# Read the RDS file

CHHWCHFW_list<- readRDS(file.path(output_path, "CHHW_CHFW_one_year.rds"))



CHHWFHHW_list <- readRDS(file.path(output_path, "CHHW_FHHW_one_year.rds"))

CHHWFHFW_list <- readRDS(file.path(output_path, "CHHW_FHFW_one_year.rds"))


# Define the city name


file_path1 <- file.path(output_path,  "Figures", paste(city,"_box_plot_oneyear_all_diff.png", sep=""))

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
month_names <- c("January", "July")

# Custom color palette (muted tones for a professional look)
color_palette <- c("#D73027", "#1A9850","#66c2a5", "#fc8d62", "#e78ac3","#8da0cb", "#a6d854", "#ffd92f")

# Loop through the selected months and their names
for (i in seq_along(months_to_plot)) {
  month <- months_to_plot[i]
  month_name <- month_names[i]
  
  # Create a data frame for the current month's data
  scenario_order <- c("CHHW→FHHW", "CHHW→CHFW", "CHHW→FHFW")
  
  # Create the boxplot_data data frame with specified scenario order
  boxplot_data <- data.frame(
    scenario = factor(rep(scenario_order, each = length(CHHWCHFW_list[[month]])), levels = scenario_order),
    Value = c(CHHWFHHW_list[[month]], CHHWCHFW_list[[month]], CHHWFHFW_list[[month]])
  )
  
  # Print the median energy burden for the current month
  cat(paste(month_name, "Median Energy Burden:\n"))
  medians <- tapply(boxplot_data$Value, boxplot_data$scenario, median)
  print(medians)
  cat("\n")
  
  # Create a dynamic color mapping based on medians
  fill_colors <- ifelse(medians > 0, "#D73027", "#1A9850")  # Red for > 0, Green for < 0
  color_mapping <- setNames(fill_colors, names(medians))  # Map colors to scenarios
  
  # Create a boxplot for the current month's data and store it in the list
  boxplot_list[[month_name]] <- ggplot(boxplot_data, aes(x = scenario, y = Value, fill = scenario, group = scenario)) +
    geom_boxplot(outlier.size = 0.5, outlier.shape = 16) + # Smaller outliers
    ggtitle(paste(month_name)) +
    scale_fill_manual(values = color_mapping) + # Apply dynamic color mapping
    coord_cartesian(ylim = c(-0.05, 0.05)) +
    theme_minimal(base_size = 14) + # Clean theme with larger base font size
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5), # Centered, bold title
      axis.title.x = element_text(size = 16),
      axis.text.x = element_text(size = 16, angle = 45, hjust = 1), # Rotate x-axis labels for readability
      axis.text.y = element_text(size = 16),
      panel.grid.major.x = element_blank(),  # Remove vertical grid lines for a cleaner look
      panel.grid.major.y = element_line(color = "grey80"),  # Light grey horizontal grid lines for easier comparison
      panel.grid.minor = element_blank(),
      legend.position = "none"  # Remove legend for simplicity
    )
  
  # Add the y-axis label only for the first (left-most) plot
  if (i == 1) {
    boxplot_list[[month_name]] <- boxplot_list[[month_name]] +
      ylab("Energy Burden") # Label only on the first boxplot
  } else {
    boxplot_list[[month_name]] <- boxplot_list[[month_name]] +
      ylab("") # No label for the other plots
  }
  
  # Save each boxplot as a separate image
  ggsave(
    filename = file.path(output_path, paste0("oneyear_boxplot_", month_name, ".png")),
    plot = boxplot_list[[month_name]],
    width = 4, height = 4, dpi = 400
  )
}


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
ggsave(file_path1, plot= final_plot, width = 10, height = 4, dpi  = 400)