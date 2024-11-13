
# Load necessary libraries
library(ggplot2)

# Create a dataframe for the heart rate data
heart_rate_data <- data.frame(
  Time = factor(c("0", "5", "10", "15", "20", "25", "30"), 
                levels = c("0", "5", "10", "15", "20", "25", "30")),
  MeanDifference = c(3.8, 4.1, 6.6, 9.7, 12, 16.1, 17.4),
  StdError = c(5.601, 2.795, 2.609, 3.337, 3.73, 5.271, 5.61),
  Sig = c(0.515, 0.176, 0.032, 0.017, 0.011, 0.014, 0.013),
  LowerCI = c(-8.871, -2.222, 0.699, 0.699, 2.152, 3.563, 4.175),
  UpperCI = c(16.471, 10.422, 12.501, 17.248, 20.437, 28.025, 30.091)
)

# Calculate s-values from p-values
heart_rate_data$s_value <- -log2(heart_rate_data$Sig)

# Create significance markers
heart_rate_data$significance <- ifelse(heart_rate_data$Sig < 0.05, "*", "")

# Create the forest plot
forest_plot_hr <- ggplot(heart_rate_data, aes(x = MeanDifference, y = Time)) +
  # Add grey zone for ±5 beats
  geom_rect(aes(xmin = -5, xmax = 5, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) +
  geom_point(size = 3) +
  # Adjust the position of p and s values with significance markers
  geom_text(aes(x = 50,  # Fixed position on right side
                label = paste0("p = ", round(Sig, 3), significance, "\ns = ", round(s_value, 2))), 
            hjust = 0, size = 3) +
  labs(x = expression("Mean Difference in Heart Rate (beats.min"^-1*")"),
       y = "Time (mins)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.ticks = element_line(color = "black")) +  # Add axis ticks
  # Set x-axis limits to ±50
  scale_x_continuous(limits = c(-50, 70),  # Extended to 70 to accommodate text
                     breaks = seq(-50, 50, by = 10))  # Add breaks every 10 units

# Display the plot
print(forest_plot_hr)

# Save the plot as PDF
ggsave("Heart_Rate_Forest_Plot.pdf", forest_plot_hr, width = 10, height = 8, units = "in")

# Load necessary libraries
library(ggplot2)

# Create a dataframe for the RPE data
rpe_data <- data.frame(
  Time = factor(c("5", "10", "15", "20", "25", "30"), 
                levels = c("5", "10", "15", "20", "25", "30")),
  MeanDifference = c(-0.778, -0.222, 0, 0.444, 0.556, 1.111),
  StdError = c(0.722, 0.909, 1.106, 1.334, 1.324, 1.532),
  Sig = c(0.313, 0.813, 1, 0.748, 0.686, 0.489),
  LowerCI = c(-2.443, -2.319, -2.549, -2.633, -2.498, -2.421),
  UpperCI = c(0.888, 1.875, 2.549, 3.522, 3.609, 4.643)
)

# Calculate s-values from p-values
rpe_data$s_value <- -log2(rpe_data$Sig)

# Create significance markers
rpe_data$significance <- ifelse(rpe_data$Sig < 0.05, "*", "")

# Create the forest plot with expanded x-axis
forest_plot_rpe <- ggplot(rpe_data, aes(x = MeanDifference, y = Time)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) +
  geom_point(size = 3) +
  # Adjust the position of p and s values with significance markers
  geom_text(aes(x = 6,  # Moved further right
                label = paste0("p = ", round(Sig, 3), significance, "\ns = ", round(s_value, 2))), 
            hjust = 0, size = 3) +
  labs(x = "Mean Difference in Rating of Perceived Exertion (RPE)",
       y = "Time (mins)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),  
        axis.ticks = element_line(color = "black")) +
  # Expanded x-axis limits to show p and s values clearly
  scale_x_continuous(limits = c(-5, 10),  # Extended right limit to 10
                     breaks = seq(-5, 5, by = 1))  # Kept breaks the same

# Display the plot
print(forest_plot_rpe)

# Save the plot as PDF
ggsave("RPE_Forest_Plot.pdf", forest_plot_rpe, width = 10, height = 8, units = "in")

# Load necessary libraries
library(ggplot2)

# Create a dataframe for the Core Temperature data
core_temp_data <- data.frame(
  Time = factor(c("0", "5", "10", "15", "20", "25", "30"), 
                levels = c("0", "5", "10", "15", "20", "25", "30")),
  MeanDifference = c(0.514, 0.343, 0, 0.243, 0.371, 0.3, 0.714),
  StdError = c(0.256, 0.149, 0.225, 0.165, 0.245, 0.253, 0.328),
  Sig = c(0.091, 0.061, 1, 0.19, 0.18, 0.28, 0.073),
  LowerCI = c(-0.112, -0.023, -0.55, -0.16, -0.227, -0.318, -0.089),
  UpperCI = c(1.14, 0.708, 0.55, 0.645, 0.97, 0.918, 1.518)
)

# Calculate s-values from p-values
core_temp_data$s_value <- -log2(core_temp_data$Sig)

# Create significance markers
core_temp_data$significance <- ifelse(core_temp_data$Sig < 0.05, "*", "")

# Create the forest plot with expanded x-axis
forest_plot_core_temp_zoomed <- ggplot(core_temp_data, aes(x = MeanDifference, y = Time)) +
  # Add grey zone for ±0.5 °C
  geom_rect(aes(xmin = -0.5, xmax = 0.5, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) +
  geom_point(size = 3) +
  # Adjust the position of p and s values with significance markers
  geom_text(aes(x = 3.5,  # Moved further right
                label = paste0("p = ", round(Sig, 3), significance, "\ns = ", round(s_value, 2))), 
            hjust = 0, size = 3) +
  labs(x = "Mean Difference in Core Temperature (°C)",
       y = "Time (mins)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.ticks = element_line(color = "black")) +  # Add axis ticks
  # Expanded x-axis limits to show p and s values clearly
  scale_x_continuous(limits = c(-3, 4),  # Extended right limit to 4
                     breaks = seq(-3, 3, by = 0.5))  # Kept breaks the same

# Display the plot
print(forest_plot_core_temp_zoomed)

# Save the plot as PDF
ggsave("Core_Temperature_Forest_Plot.pdf", forest_plot_core_temp_zoomed, width = 10, height = 8, units = "in")

# Load necessary libraries
library(ggplot2)

# Create a dataframe for the Blood Lactate data
blood_lactate_data <- data.frame(
  Time = factor(c("0", "10", "20", "30"), 
                levels = c("0", "10", "20", "30")),
  MeanDifference = c(0.133, 0.533, 0.767, -0.533),
  StdError = c(0.291, 0.285, 0.338, 0.902),
  Sig = c(0.691, 0.202, 0.152, 0.614),
  LowerCI = c(-1.117, -0.692, -0.689, -4.416),
  UpperCI = c(1.384, 1.759, 2.222, 3.35)
)

# Calculate s-values from p-values
blood_lactate_data$s_value <- -log2(blood_lactate_data$Sig)

# Create significance markers
blood_lactate_data$significance <- ifelse(blood_lactate_data$Sig < 0.05, "*", "")

# Create the forest plot
forest_plot_lactate_zoomed_out <- ggplot(blood_lactate_data, aes(x = MeanDifference, y = Time)) +
  # Add grey zone for ±0.5 mmol/L
  geom_rect(aes(xmin = -0.5, xmax = 0.5, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) +
  geom_point(size = 3) +
  # Adjust the position of p and s values with significance markers
  geom_text(aes(x = 6,  # Moved further right
                label = paste0("p = ", round(Sig, 3), significance, "\ns = ", round(s_value, 2))), 
            hjust = 0, size = 3) +
  labs(x = "Mean Difference in Blood Lactate (mmol/L)",
       y = "Time (mins)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.ticks = element_line(color = "black")) +  # Add axis ticks
  # Expanded x-axis limits to show p and s values clearly
  scale_x_continuous(limits = c(-5, 7),  # Extended right limit to 7
                     breaks = seq(-5, 5, by = 1))  # Kept breaks the same

# Display the plot
print(forest_plot_lactate_zoomed_out)

# Save the plot as PDF
ggsave("Blood_Lactate_Forest_Plot.pdf", forest_plot_lactate_zoomed_out, width = 10, height = 8, units = "in")

# Load necessary libraries
library(ggplot2)

# Create a dataframe for the VO2 data
vo2_data <- data.frame(
  Time = factor(c("10", "20", "30"), 
                levels = c("10", "20", "30")),
  MeanDifference = c(-0.006, 0.257, 0.296),
  StdError = c(0.107, 0.237, 0.169),
  Sig = c(0.96, 0.31, 0.118),
  LowerCI = c(-0.253, -0.289, -0.094),
  UpperCI = c(0.242, 0.802, 0.685)
)

# Calculate s-values from p-values
vo2_data$s_value <- -log2(vo2_data$Sig)

# Create significance markers
vo2_data$significance <- ifelse(vo2_data$Sig < 0.05, "*", "")

# Create the forest plot
forest_plot_vo2_zoomed <- ggplot(vo2_data, aes(x = MeanDifference, y = Time)) +
  # Add grey zone for ±0.5 L/min
  geom_rect(aes(xmin = -0.5, xmax = 0.5, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) +
  geom_point(size = 3) +
  # Adjust the position of p and s values with significance markers
  geom_text(aes(x = 2.5,  # Moved further right
                label = paste0("p = ", round(Sig, 3), significance, "\ns = ", round(s_value, 2))), 
            hjust = 0, size = 3) +
  labs(x = "Mean Difference in VO2 (L/min)",
       y = "Time (mins)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.ticks = element_line(color = "black")) +  # Add axis ticks
  # Extended x-axis limits to show p and s values more clearly
  scale_x_continuous(limits = c(-2.0, 3.5),  # Extended right limit to 3.5
                     breaks = seq(-2.0, 2.0, by = 0.5))  # Kept breaks the same

# Display the plot
print(forest_plot_vo2_zoomed)

# Save the plot as PDF
ggsave("VO2_Forest_Plot.pdf", forest_plot_vo2_zoomed, width = 10, height = 8, units = "in")

# Load necessary libraries
library(ggplot2)

# Create a dataframe for the VO2 data
vo2_data <- data.frame(
  Time = factor(c("10", "20", "30"), 
                levels = c("10", "20", "30")),
  MeanDifference = c(-0.006, 0.257, 0.296),
  StdError = c(0.107, 0.237, 0.169),
  Sig = c(0.96, 0.31, 0.118),
  LowerCI = c(-0.253, -0.289, -0.094),
  UpperCI = c(0.242, 0.802, 0.685)
)

# Calculate s-values from p-values
vo2_data$s_value <- -log2(vo2_data$Sig)

# Create significance markers
vo2_data$significance <- ifelse(vo2_data$Sig < 0.05, "*", "")

# Create the forest plot
forest_plot_vo2_adjusted_grey <- ggplot(vo2_data, aes(x = MeanDifference, y = Time)) +
  # Add grey zone for ±0.25 L/min
  geom_rect(aes(xmin = -0.25, xmax = 0.25, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) +
  geom_point(size = 3) +
  # Adjust the position of p and s values with significance markers
  geom_text(aes(x = 2.5,  # Moved further right
                label = paste0("p = ", round(Sig, 3), significance, "\ns = ", round(s_value, 2))), 
            hjust = 0, size = 3) +
  labs(x = "Mean Difference in VO2 (L/min)",
       y = "Time (mins)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.ticks = element_line(color = "black")) +  # Add axis ticks
  # Extended x-axis limits to show p and s values more clearly
  scale_x_continuous(limits = c(-2.0, 3.5),  # Extended right limit to 3.5
                     breaks = seq(-2.0, 2.0, by = 0.5))  # Kept breaks the same

# Display the plot
print(forest_plot_vo2_adjusted_grey)

# Save the plot as PDF
ggsave("VO2_Forest_Plot_Adjusted_Grey.pdf", forest_plot_vo2_adjusted_grey, width = 10, height = 8, units = "in")

# Load necessary libraries
library(ggplot2)

# Create a dataframe for the VO2 data
vo2_data <- data.frame(
  Time = factor(c("10", "20", "30"), 
                levels = c("10", "20", "30")),
  MeanDifference = c(-0.006, 0.257, 0.296),
  StdError = c(0.107, 0.237, 0.169),
  Sig = c(0.96, 0.31, 0.118),
  LowerCI = c(-0.253, -0.289, -0.094),
  UpperCI = c(0.242, 0.802, 0.685)
)

# Calculate s-values from p-values
vo2_data$s_value <- -log2(vo2_data$Sig)

# Create significance markers
vo2_data$significance <- ifelse(vo2_data$Sig < 0.05, "*", "")

# Create the forest plot
forest_plot_vo2_subscript <- ggplot(vo2_data, aes(x = MeanDifference, y = Time)) +
  # Add grey zone for ±0.25 L/min
  geom_rect(aes(xmin = -0.25, xmax = 0.25, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) +
  geom_point(size = 3) +
  # Adjust the position of p and s values with significance markers
  geom_text(aes(x = 2.5,  # Moved further right
                label = paste0("p = ", round(Sig, 3), significance, "\ns = ", round(s_value, 2))), 
            hjust = 0, size = 3) +
  labs(x = expression(paste("Mean Difference in ", dot(V), O[2], " (L/min)")),
       y = "Time (mins)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.ticks = element_line(color = "black")) +  # Add axis ticks
  # Extended x-axis limits to show p and s values more clearly
  scale_x_continuous(limits = c(-2.0, 3.5),  # Extended right limit to 3.5
                     breaks = seq(-2.0, 2.0, by = 0.5))  # Kept breaks the same

# Display the plot
print(forest_plot_vo2_subscript)

# Save the plot as PDF
ggsave("VO2_Forest_Plot_Subscript.pdf", forest_plot_vo2_subscript, width = 10, height = 8, units = "in")

# Load necessary libraries
library(ggplot2)

# Create a dataframe for the VE data
ve_data <- data.frame(
  Time = factor(c("10", "20", "30"), 
                levels = c("10", "20", "30")),
  MeanDifference = c(1.844, 2.856, 3.5),
  StdError = c(2.721, 2.232, 4.475),
  Sig = c(0.517, 0.237, 0.457),
  LowerCI = c(-4.43, -2.29, -6.82),
  UpperCI = c(8.119, 8.001, 13.82)
)

# Calculate s-values from p-values
ve_data$s_value <- -log2(ve_data$Sig)

# Create significance markers
ve_data$significance <- ifelse(ve_data$Sig < 0.05, "*", "")

# Create the forest plot without the grey zone
forest_plot_ve_no_grey <- ggplot(ve_data, aes(x = MeanDifference, y = Time)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) +
  geom_point(size = 3) +
  # Adjust the position of p and s values with significance markers
  geom_text(aes(x = 15,  # Moved further right based on CI range
                label = paste0("p = ", round(Sig, 3), significance, "\ns = ", round(s_value, 2))), 
            hjust = 0, size = 3) +
  labs(x = expression(paste("Mean Difference in ", dot(V)[E], " (L/min)")),
       y = "Time (mins)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black")) +
  # Set x-axis limits based on CI ranges with extra space for p-values
  scale_x_continuous(limits = c(-8, 18),
                     breaks = seq(-8, 14, by = 2))

# Display the plot
print(forest_plot_ve_no_grey)

# Save the plot as PDF
ggsave("VE_Forest_Plot.pdf", forest_plot_ve_no_grey, width = 10, height = 8, units = "in")