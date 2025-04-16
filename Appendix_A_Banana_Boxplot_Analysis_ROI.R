# ============================================================
# Title: Environmental Boxplot Comparison of Banana Presence in Region of Interest
# Author: Cressida Hoddell
# Description: This script loads environmental raster data,
#              separates banana and non-banana pixels,
#              and visualises distributions of environmental variables
#              using custom 90th-percentile boxplots.
# ============================================================

# -------------------- Step 1: Install and Load Required Libraries --------------------
install.packages(c("terra", "ggplot2", "dplyr", "tidyr"))  # Install required packages
library(terra)   # For raster handling
library(ggplot2) # For visualization
library(dplyr)   # For data manipulation
library(tidyr)   # For reshaping data

# -------------------- Step 2: Load the GeoTIFF File --------------------
# Set the path to exported GeoTIFF file from Google Earth Engine
file_path <- "Banana_and_Environmental_Factors_1km_Final5.tif"

# Load the raster stack
raster_data <- rast(file_path)

# Print raster metadata
print(raster_data)
names(raster_data)

# -------------------- Step 3: Convert Raster to Data Frame --------------------
df <- as.data.frame(raster_data, xy = TRUE, na.rm = TRUE)
colnames(df) <- c("x", "y", "Banana_Presence", "Soil_pH", "Population_Density",
                  "Elevation", "Annual_Precipitation", "Precip_Seasonality", 
                  "Temperature_Seasonality", "Diurnal_Temperature_Range", "Temperature_Max", 
                  "Temperature_Min", "Average_Temperature")
df <- df %>% mutate(Soil_pH = Soil_pH / 10)

# -------------------- Step 4: Separate Banana and Non-Banana Pixels --------------------
banana_df <- df %>% filter(Banana_Presence == 1)
non_banana_df <- df %>% filter(Banana_Presence == 0)

# -------------------- Step 5: Reshape Data for Visualisation --------------------
variable_labels <- c("Soil_pH" = "ph", "Elevation" = "ele", "Annual_Precipitation" = "prec", "Precip_Seasonality" = "psea", 
                     "Population_Density" = "pop", "Temperature_Seasonality" = "tsea", "Diurnal_Temperature_Range" = "tdir", 
                     "Temperature_Max" = "tmax", "Temperature_Min" = "tmin", "Average_Temperature" = "tavg")
variables <- names(variable_labels)

banana_long <- banana_df %>%
  select(all_of(variables)) %>%
  mutate(Banana = factor("Presence", levels = c("Presence", "Absence"))) %>%
  pivot_longer(cols = all_of(variables), names_to = "Variable", values_to = "Value")

non_banana_long <- non_banana_df %>%
  select(all_of(variables)) %>%
  mutate(Banana = factor("Absence", levels = c("Presence", "Absence"))) %>%
  pivot_longer(cols = all_of(variables), names_to = "Variable", values_to = "Value")

boxplot_data <- rbind(banana_long, non_banana_long)
boxplot_data$Banana <- factor(boxplot_data$Banana, levels = c("Presence", "Absence"))
boxplot_data$Variable <- factor(boxplot_data$Variable, levels = names(variable_labels), labels = variable_labels)

# -------------------- Step 6: Compute Percentiles for Boxplot --------------------
percentile_summary <- boxplot_data %>%
  group_by(Banana, Variable) %>%
  summarise(
    Q05 = quantile(Value, 0.05, na.rm = TRUE),
    Q25 = quantile(Value, 0.25, na.rm = TRUE),
    Median = quantile(Value, 0.50, na.rm = TRUE),
    Q75 = quantile(Value, 0.75, na.rm = TRUE),
    Q95 = quantile(Value, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  drop_na()

# -------------------- Step 7: Generate Combined Boxplot --------------------
p <- ggplot(percentile_summary, aes(x = Banana, fill = Banana)) +
  geom_boxplot(
    aes(ymin = Q05, lower = Q25, middle = Median, upper = Q75, ymax = Q95),
    stat = "identity"
  ) +
  facet_wrap(~Variable, scales = "free_y") +
  scale_fill_manual(values = c("Presence" = "lightblue", "Absence" = "coral1")) +
  theme_minimal() +
  labs(x = "Banana", y = "Value") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 10, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(color = "black", fill = NA)
  )

print(p)

# -------------------- End --------------------
cat("Boxplot generation complete.\n")
