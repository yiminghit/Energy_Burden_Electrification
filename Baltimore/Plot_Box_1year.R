# Load required libraries
library(ggplot2)

library(gridExtra)
city <- "Baltimore"
# Define the path variable
#output_path <- "C:/Users/yangl11/R_Project/ACS_DATA/Detro/"

routine_path <- "D:/Umich_Project/Experiments/"
output_path <-paste(routine_path, city, '/', sep="")
# Read the RDS file
loaded_data1 <- readRDS(file.path(output_path, "CHHW_one_year.rds"))
loaded_data2 <- readRDS(file.path(output_path, "FHHW_one_year.rds"))
loaded_data3 <- readRDS(file.path(output_path, "CHFW_one_year.rds"))
loaded_data4 <- readRDS(file.path(output_path, "FHFW_one_year.rds"))

file_path1 <- file.path(output_path,  "Figures",  paste(city,"_multiple_box_1year.png", sep=""))
#file_path2 <- file.path(output_path, "Figures", "multiple_box_1year.png")

calculate_kde_cdf <- function(data) {
  # Calculate KDE
  kde <- density(data, bw = "bcv")
  
  # Normalize the density values
  norm_factor <- sum(kde$y * diff(kde$x))
  kde$y <- kde$y / norm_factor
  
  # Compute the CDF
  cdf <- cumsum(kde$y * diff(kde$x))
  
  return(list(kde = kde, cdf = cdf))
}

result_loaded_data1 <- (loaded_data1)
result_loaded_data2 <- (loaded_data3[['2050']]$rcp45cooler)
result_loaded_data3 <- (loaded_data3[['2050']]$rcp85hotter)

result_loaded_data4 <- (loaded_data2)
result_loaded_data5 <- (loaded_data4[['2050']]$rcp45cooler)
result_loaded_data6 <- (loaded_data4[['2050']]$rcp85hotter)

# Create a list to store individual boxplots
boxplot_list <- list()
#png("C:/Users/yangl11/R_Project/ACS_DATA/Detroit/combined_boxplots_whole.png", width = 3200, height = 2400, res = 300)  # Adjust width, height, and resolution as needed
# Combine all datasets into a single list
all_loaded_data <- list(
  result_loaded_data1,
  result_loaded_data2,
  result_loaded_data3,
  result_loaded_data4,
  result_loaded_data5,
  result_loaded_data6
)
#name <- c("CHHW", "FHHW", "CHFW_hot", "CHFW_cool", "FHFW_hot", "FHFW_cool")
name <- c("CHHW", "CHFW_cool", "CHFW_hot", "FHHW","FHFW_cool", "FHFW_hot")
# Loop through each month (1 to 12)
for (month in 1:6) {
  # Convert the current month's data to a data frame
  boxplot_data <- data.frame(
    scenario = factor(rep(name[month], each = length(all_loaded_data[[month]]))),
    Value = all_loaded_data[[month]]
  )
  
  median_value <- median(all_loaded_data[[month]])
  cat(paste("Case:", month, "-", name[month], "Median Energy Burden:", median_value, "\n"))
  
  # Create a boxplot for the current month's data and store it in the list
  boxplot_list[[month]] <- ggplot(boxplot_data, aes(x = scenario, y = Value)) +
    geom_boxplot(fill = "skyblue", color = "blue") +
    #geom_boxplot(fill = "skyblue", color = "blue", outlier.shape = NA) +
    ylab("Energy Burden") +xlab("") +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 0.20))
}
# Combine the boxplots into a single figure
combined_plot <- grid.arrange(grobs = boxplot_list, ncol = 6)  # Adjust ncol according to your preference

# Save the combined plot as a PNG with high resolution
#print(combined_plot)
#dev.off()
ggsave(file_path1, plot= combined_plot, width = 12, height = 4, dpi  = 300)

