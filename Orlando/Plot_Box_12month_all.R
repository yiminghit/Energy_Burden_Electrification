# Load required libraries
library(ggplot2)

library(gridExtra)
# Read the RDS file
city <- "Orlando"
routine_path <- "D:/Umich_Project/Experiments/"
output_path <-paste(routine_path, city, '/', sep="")
# Read the RDS file

summary_all<- readRDS(file.path(output_path, "CHFW12months.rds"))

FHHW_list <- readRDS(file.path(output_path, "FHHW12months.rds"))
CHHW_list <- readRDS(file.path(output_path, "CHHW12months.rds"))

summary_all2 <- readRDS(file.path(output_path, "FHFW12months.rds"))


CHFW_list1 <- summary_all[['2050']]$rcp45cooler
CHFW_list2 <- summary_all[['2050']]$rcp85hotter

FHFW_list1 <- summary_all2[['2050']]$rcp45cooler
FHFW_list2 <- summary_all2[['2050']]$rcp85hotter


# Define the city name


file_path1 <- file.path(output_path,  "Figures", paste(city,"_box_plot_12months_all.png", sep=""))

file_path2 <- file.path(output_path, "Figures", "box_plot_12months_all2.png")
# Define the file path
#file_path1 <- file.path("C:/Users/yangl11/R_Project/ACS_DATA", city, "Figures", "box_plot_12months_all1.png")
#file_path2 <- file.path("C:/Users/yangl11/R_Project/ACS_DATA", city, "Figures", "box_plot_12months_all2.png")

# Create a list to store individual boxplots
boxplot_list <- list()
#png("C:/Users/yangl11/R_Project/ACS_DATA/Detroit/combined_boxplots_whole.png", width = 3200, height = 2400, res = 300)  # Adjust width, height, and resolution as needed
boxplot_list <- list()

# Loop through each month (1 to 12)
for (month in 1:12) {
  # Create a data frame for the current month's data
  scenario_order <- c("CHHW", "CHFW_cool", "CHFW_hot", "FHHW", "FHFW_cool", "FHFW_hot")
  
  # Create the boxplot_data data frame with specified scenario order
  boxplot_data <- data.frame(
    scenario = factor(rep(scenario_order, each = length(CHFW_list1[[month]])), levels = scenario_order),
    Value = c(CHHW_list[[month]], CHFW_list1[[month]], CHFW_list2[[month]], FHHW_list[[month]], FHFW_list1[[month]], FHFW_list2[[month]])
  )
  cat(paste("Month", month, "Median Energy Burden:\n"))
  medians <- tapply(boxplot_data$Value, boxplot_data$scenario, median)
  print(medians)
  cat("\n")
  # Create a boxplot for the current month's data and store it in the list
  boxplot_list[[month]] <- ggplot(boxplot_data, aes(x = scenario, y = Value, fill = scenario, group = scenario)) +
    ggtitle(paste("Month", month)) +
    geom_boxplot(outlier.shape = NA) + 
    #geom_boxplot() +
    ylab("") +
    coord_cartesian(ylim = c(0, 0.2)) +
    theme(legend.position = "none")
  
}

# Combine the boxplots into a single figure
combined_plot <- grid.arrange(grobs = boxplot_list, ncol = 4)  # Adjust ncol according to your preference

# Save the combined plot as a PNG with high resolution
#print(combined_plot)
#dev.off()
ggsave(file_path1, plot= combined_plot, width = 20, height = 8, dpi  = 400)
