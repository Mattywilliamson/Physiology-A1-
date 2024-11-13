# Load required library
library(ggplot2)

# Create dataframe for Heart Rate with staggered time points for hot condition
hr_data <- data.frame(
  temp = factor(c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2), 
                levels = c(1, 2), 
                labels = c("Thermoneutral", "Hot")),
  time = c(0, 5, 10, 15, 20, 25, 30,
           0.5, 5.5, 10.5, 15.5, 20.5, 25.5, 30.5),
  mean = c(78.9, 123.2, 129.3, 129.3, 131.7, 132.9, 135.5, 
           82.7, 127.3, 135.9, 139, 143.7, 149, 152.9),
  lower_bound = c(67.383, 116.064, 120.101, 121.15, 124.197, 125.133, 125.748, 
                  75.189, 117.857, 127.09, 128.373, 132.234, 135.853, 138.795),
  upper_bound = c(90.417, 130.336, 138.499, 137.45, 139.203, 140.667, 145.252, 
                  90.211, 136.743, 144.71, 149.627, 155.166, 162.147, 167.005)
)

# Create the line graph for Heart Rate with error bars
hr_plot <- ggplot(hr_data, aes(x = time, y = mean, color = temp, group = temp)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                width = 0.5, 
                linewidth = 0.5) +
  scale_color_manual(values = c("blue", "red")) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  labs(x = "Time (mins)", 
       y = "Heart Rate (beats.min-1)", 
       color = "Condition") +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid = element_blank()
  )

# Print the Heart Rate plot
print(hr_plot)

# Save the Heart Rate plot as PDF
ggsave("heart_rate_plot.pdf", 
       hr_plot, 
       width = 12, 
       height = 8, 
       dpi = 300)

# Load required library
library(ggplot2)

# Create dataframe for Core Temperature with staggered time points for hot condition
core_temp_data <- data.frame(
  temp = factor(c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2), 
                levels = c(1, 2), 
                labels = c("Thermoneutral", "Hot")),
  time = c(0, 5, 10, 15, 20, 25, 30,
           0.5, 5.5, 10.5, 15.5, 20.5, 25.5, 30.5),
  mean = c(36.614, 36.886, 37.000, 36.900, 36.857, 36.957, 36.843, 
           37.129, 37.229, 37.000, 37.143, 37.229, 37.257, 37.557),
  lower_bound = c(35.357, 35.596, 35.596, 35.054, 34.972, 35.017, 34.636, 
                  36.149, 36.209, 35.100, 35.382, 35.425, 35.182, 35.872),
  upper_bound = c(37.872, 38.175, 38.404, 38.746, 38.742, 38.898, 39.050, 
                  38.108, 38.248, 38.900, 38.904, 39.032, 39.332, 39.242)
)

# Create the line graph for Core Temperature with error bars
core_temp_plot <- ggplot(core_temp_data, aes(x = time, y = mean, color = temp, group = temp)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                width = 0.5, 
                linewidth = 0.5) +
  scale_color_manual(values = c("blue", "red")) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  labs(x = "Time (mins)", 
       y = "Core Temperature (°C)", 
       color = "Condition") +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid = element_blank()
  )

# Print the Core Temperature plot
print(core_temp_plot)

# Save the Core Temperature plot as PDF
ggsave("core_temperature_plot.pdf", 
       core_temp_plot, 
       width = 12, 
       height = 8, 
       dpi = 300)

# Load required library
library(ggplot2)

# Create dataframe for Blood Lactate with staggered time points for hot condition
blood_lactate_data <- data.frame(
  temp = factor(c(1, 1, 1, 1, 2, 2, 2, 2), 
                levels = c(1, 2), 
                labels = c("Thermoneutral", "Hot")),
  time = c(0, 10, 20, 30,
           0.5, 10.5, 20.5, 30.5),
  mean = c(1.267, 2.133, 1.633, 2.700, 
           1.400, 2.667, 2.400, 2.167),
  lower_bound = c(0.508, -0.093, 0.513, -2.059, 
                  0.539, -0.234, 0.004, -0.128),
  upper_bound = c(2.026, 4.360, 2.753, 7.459, 
                  2.261, 5.567, 4.796, 4.461)
)

# Create the line graph for Blood Lactate with error bars
blood_lactate_plot <- ggplot(blood_lactate_data, aes(x = time, y = mean, color = temp, group = temp)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                width = 0.5, 
                linewidth = 0.5) +
  scale_color_manual(values = c("blue", "red")) +
  scale_x_continuous(breaks = c(0, 10, 20, 30)) +
  scale_y_continuous(breaks = seq(-3, 8, by = 1.0)) +  # Y-axis from -3 to 8 in increments of 1.0
  labs(x = "Time (mins)", 
       y = "Blood Lactate (mmol/L)", 
       color = "Condition") +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid = element_blank()
  )

# Print the Blood Lactate plot
print(blood_lactate_plot)

# Save the Blood Lactate plot as PDF
ggsave("blood_lactate_plot.pdf", 
       blood_lactate_plot, 
       width = 12, 
       height = 8, 
       dpi = 300)

# Load required library
library(ggplot2)

# Create dataframe for Rating of Perceived Exertion (RPE) with staggered time points for hot condition
rpe_data <- data.frame(
  temp = factor(c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2), 
                levels = c(1, 2), 
                labels = c("Thermoneutral", "Hot")),
  time = c(5, 10, 15, 20, 25, 30,
           5.5, 10.5, 15.5, 20.5, 25.5, 30.5),
  mean = c(4.667, 5.444, 5.889, 6.222, 6.778, 7.111, 
           3.889, 5.222, 5.889, 6.667, 7.333, 8.222),
  lower_bound = c(2.670, 2.950, 3.214, 3.201, 3.661, 4.123, 
                  2.068, 3.160, 3.569, 4.117, 4.432, 5.058),
  upper_bound = c(6.664, 7.939, 8.564, 9.243, 9.895, 10.099, 
                  5.710, 7.284, 8.209, 9.216, 10.235, 11.386)
)

# Create the line graph for RPE with error bars
rpe_plot <- ggplot(rpe_data, aes(x = time, y = mean, color = temp, group = temp)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                width = 0.5, 
                linewidth = 0.5) +
  scale_color_manual(values = c("blue", "red")) +
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30)) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +  # Y-axis in increments of 2
  labs(x = "Time (mins)", 
       y = "Rating of Perceived Exertion (RPE)", 
       color = "Condition") +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid = element_blank()
  )

# Print the RPE plot
print(rpe_plot)

# Save the RPE plot as PDF
ggsave("rpe_plot.pdf", 
       rpe_plot, 
       width = 12, 
       height = 8, 
       dpi = 300)

# Load required library
library(ggplot2)

# Create dataframe for VO2 with staggered time points for hot condition
vo2_data <- data.frame(
  temp = factor(c(1, 1, 1, 2, 2, 2), 
                levels = c(1, 2), 
                labels = c("Thermoneutral", "Hot")),
  time = c(10, 20, 30,
           10.5, 20.5, 30.5),
  mean = c(1.972, 1.914, 2.026,
           1.967, 2.171, 2.321),
  lower_bound = c(1.490, 1.548, 1.714,
                  1.418, 1.600, 1.830),
  upper_bound = c(2.455, 2.281, 2.337,
                  2.515, 2.742, 2.812)
)

# Create the line graph for VO2 with error bars
vo2_plot <- ggplot(vo2_data, aes(x = time, y = mean, color = temp, group = temp)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                width = 0.5, 
                linewidth = 0.5) +
  scale_color_manual(values = c("blue", "red")) +
  scale_x_continuous(breaks = c(10, 20, 30)) +
  scale_y_continuous(breaks = seq(1, 3, by = 0.5)) +  # Y-axis in increments of 0.5
  labs(x = "Time (mins)", 
       y = "VO2 (L/min)",  # Adjusted to avoid subscript issue
       color = "Condition") +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid = element_blank()
  )

# Print the VO2 plot
print(vo2_plot)

# Save the VO2 plot as PDF
ggsave("vo2_plot.pdf", 
       vo2_plot, 
       width = 12, 
       height = 8, 
       dpi = 300)

# Load required library
library(ggplot2)

# Create dataframe for VE with staggered time points for hot condition
ve_data <- data.frame(
  temp = factor(c(1, 1, 1, 2, 2, 2), 
                levels = c(1, 2), 
                labels = c("Thermoneutral", "Hot")),
  time = c(10, 20, 30,
           10.5, 20.5, 30.5),
  mean = c(39.433, 42.411, 45.511,
           41.278, 45.267, 49.011),
  lower_bound = c(29.613, 35.558, 36.147,
                  33.353, 38.426, 40.602),
  upper_bound = c(49.254, 49.265, 54.875,
                  49.203, 52.107, 57.420)
)

# Create the line graph for VE with error bars
ve_plot <- ggplot(ve_data, aes(x = time, y = mean, color = temp, group = temp)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                width = 0.5, 
                linewidth = 0.5) +
  scale_color_manual(values = c("blue", "red")) +
  scale_x_continuous(breaks = c(10, 20, 30)) +
  scale_y_continuous(limits = c(NA, 60), breaks = seq(0, 60, by = 10)) +  # Updated y-axis to extend to 60
  labs(x = "Time (mins)", 
       y = "VE (L/min)", 
       color = "Condition") +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid = element_blank()
  )

# Print the VE plot
print(ve_plot)

# Save the VE plot as PDF
ggsave("ve_plot.pdf", 
       ve_plot, 
       width = 12, 
       height = 8, 
       dpi = 300)