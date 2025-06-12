# Load required libraries
library(ggplot2)
library(gridExtra)

# Define city and paths
#city <- "Phoenix"
routine_path <- "D:/Umich_Project/Experiments/"
output_path <- paste(routine_path, city, '/', sep="")

# Read the RDS files
loaded_data1 <- readRDS(file.path(output_path, "CHHW_one_year.rds"))
loaded_data2 <- readRDS(file.path(output_path, "FHHW_one_year.rds"))
loaded_data3 <- readRDS(file.path(output_path, "CHFW_one_year.rds"))
loaded_data4 <- readRDS(file.path(output_path, "FHFW_one_year.rds"))


file_path_top10<- file.path(output_path,  "Figures",  paste(city,"_top_10_box_1year.png", sep=""))

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
result_loaded_data3 <- (loaded_data3[['2050']]$rcp85cooler)

result_loaded_data4 <- (loaded_data2)
result_loaded_data5 <- (loaded_data4[['2050']]$rcp45cooler)
result_loaded_data6 <- (loaded_data4[['2050']]$rcp85cooler)

# Create a list to store individual boxplots
boxplot_list <- list()

# Combine all datasets into a single list
all_loaded_data <- list(
  result_loaded_data1,
  result_loaded_data2,
  result_loaded_data3,
  result_loaded_data4,
  result_loaded_data5,
  result_loaded_data6
)

# Define Joule journal-inspired color palette
color_palette <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")

# Define the names of scenarios for each plot
scenario_order <- c("CHHW", "CHFW_cool", "CHFW_hot", "FHHW", "FHFW_cool", "FHFW_hot")

# Calculate top 10% data for each scenario
top_10_data <- lapply(all_loaded_data, function(data) {
  quantile_value <- quantile(data, 0.9)
  return(data[data > quantile_value])
})

# Create a combined data frame for the top 10% data
top10_boxplot_data <- data.frame(
  scenario = factor(rep(scenario_order, sapply(top_10_data, length)), levels = scenario_order),
  Value = unlist(top_10_data)
)

# Print the median energy burden for the top 10% data
cat("Top 10% Median Energy Burden by Scenario:\n")
top10_medians <- tapply(top10_boxplot_data$Value, top10_boxplot_data$scenario, median)
print(top10_medians)
cat("\n")


top10_boxplot <- ggplot(top10_boxplot_data, aes(x = scenario, y = Value, fill = scenario)) +
  geom_violin(scale = "width", alpha = 0.8) +  # Violin plot
  # Add boxplot-like lines
  stat_summary(fun = "median", geom = "crossbar", width = 0.4, color = "black", fatten = 2) +  # Median line
  stat_summary(fun.min = function(x) quantile(x, probs = 0.25), 
               fun.max = function(x) quantile(x, probs = 0.75), 
               geom = "errorbar", width = 0.4, color = "black") +  # Q1 and Q3 lines
  #ggtitle(paste(month_name)) +
  scale_fill_manual(values = color_palette) +  # Apply custom color palette
  coord_cartesian(ylim = c(0, 1)) +
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
  #  plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")  # Further reduce plot margins
  ) +
  ylab("Tail Energy Burden")

# Calculate the median and IQR for the top 10% data for each scenario
top10_summary_stats <- data.frame(
  scenario = levels(top10_boxplot_data$scenario),
  median = tapply(top10_boxplot_data$Value, top10_boxplot_data$scenario, median),
  Q1 = tapply(top10_boxplot_data$Value, top10_boxplot_data$scenario, function(x) quantile(x, probs = 0.25)),
  Q3 = tapply(top10_boxplot_data$Value, top10_boxplot_data$scenario, function(x) quantile(x, probs = 0.75)),
  IQR = tapply(top10_boxplot_data$Value, top10_boxplot_data$scenario, function(x) diff(quantile(x, probs = c(0.25, 0.75)))),
  Q0 = tapply(top10_boxplot_data$Value, top10_boxplot_data$scenario, function(x) quantile(x, probs = 0.05)),
  Q4 = tapply(top10_boxplot_data$Value, top10_boxplot_data$scenario, function(x) quantile(x, probs = 0.95)),
  IQR2 = tapply(top10_boxplot_data$Value, top10_boxplot_data$scenario, function(x) diff(quantile(x, probs = c(0.05, 0.95))))
  
)

# Print the summary statistics
print(top10_summary_stats)

# Save the summary statistics to a CSV file
csv_file_path_top10 <- file.path(output_path, "top10_yearly_summary_stats.csv")
write.csv(top10_summary_stats, csv_file_path_top10, row.names = FALSE)
# Create the title grob for the city name with adjusted position
title_grob <- textGrob(city, gp = gpar(fontsize = 20, fontface = "bold"), x = 0, y = 1, hjust = 0, vjust = 1)

# Combine the title and the boxplot into one layout
final_plot <- arrangeGrob(
  title_grob,
  top10_boxplot,
  ncol = 1,
  heights = c(0.1, 1)
)

# Save the plot as a PNG with high resolution
#file_path_top10 <- "top10_scenarios_boxplot.png"  # Specify your file path here
ggsave(file_path_top10, plot = final_plot, width = 5, height = 4.5, dpi = 600)
