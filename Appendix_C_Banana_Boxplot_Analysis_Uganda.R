# ============================================================
# Title: Combined Boxplot of Banana Presence Points in Region of Interest vs Absence Points from Uganda Extent
# Author: Cressida Hoddell
# Description: This script combines raster-based banana presence
#              data with absence points from CSV, and visualises
#              environmental distributions using custom boxplots.
# ============================================================

# -------------------- Step 1: Install and Load Required Libraries --------------------
install.packages(c("terra", "ggplot2", "dplyr", "tidyr", "readr"))
library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# -------------------- Step 2: Load the GeoTIFF File --------------------
file_path <- "Banana_and_Environmental_Factors_1km_Final5.tif"
raster_data <- rast(file_path)

# Convert raster to dataframe and rename columns
df <- as.data.frame(raster_data, xy = TRUE, na.rm = TRUE)
colnames(df) <- c("x", "y", "Banana_Presence", "Soil_pH", "Population_Density",
                  "Elevation", "Annual_Precipitation", "Precip_Seasonality", 
                  "Temperature_Seasonality", "Diurnal_Temperature_Range", "Temperature_Max", 
                  "Temperature_Min", "Average_Temperature")

# Adjust pH scale
df <- df %>% mutate(Soil_pH = Soil_pH / 10)

# Filter only banana presence pixels
banana_df <- df %>% filter(Banana_Presence == 1)

# -------------------- Step 3: Load Absence Points from CSV --------------------
absence_points <- read_csv("Banana_Absence_Points_With_Environmental_Variables_Final.csv")

absence_points <- absence_points %>%
  select(-c(`system:index`, random, .geo)) %>%
  mutate(
    Soil_pH = Soil_pH / 10,
    Banana_Presence = 0
  ) %>%
  drop_na()

# -------------------- Step 4: Combine Presence and Absence --------------------
full_df <- bind_rows(banana_df, absence_points)

# -------------------- Step 5: Reshape Data for Visualisation --------------------
variable_labels <- c("Soil_pH" = "ph", "Elevation" = "ele", "Annual_Precipitation" = "prec", "Precip_Seasonality" = "psea", 
                     "Population_Density" = "pop", "Temperature_Seasonality" = "tsea", "Diurnal_Temperature_Range" = "tdir", 
                     "Temperature_Max" = "tmax", "Temperature_Min" = "tmin", "Average_Temperature" = "tavg")
variables <- names(variable_labels)

long_df <- full_df %>%
  select(all_of(variables), Banana_Presence) %>%
  mutate(Banana = factor(ifelse(Banana_Presence == 1, "Banana", "All"),
                         levels = c("Banana", "All"))) %>%
  pivot_longer(cols = all_of(variables), names_to = "Variable", values_to = "Value")

long_df$Variable <- factor(long_df$Variable, levels = names(variable_labels), labels = variable_labels)

# -------------------- Step 6: Calculate Percentiles --------------------
percentile_summary <- long_df %>%
  group_by(Banana, Variable) %>%
  summarise(
    Q05 = quantile(Value, 0.05, na.rm = TRUE),
    Q25 = quantile(Value, 0.25, na.rm = TRUE),
    Median = quantile(Value, 0.5, na.rm = TRUE),
    Q75 = quantile(Value, 0.75, na.rm = TRUE),
    Q95 = quantile(Value, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------- Step 7: Generate Boxplots --------------------
p <- ggplot(percentile_summary, aes(x = Banana, fill = Banana)) +
  geom_boxplot(
    aes(ymin = Q05, lower = Q25, middle = Median, upper = Q75, ymax = Q95),
    stat = "identity"
  ) +
  facet_wrap(~Variable, scales = "free_y") +
  scale_fill_manual(values = c("Banana" = "lightblue", "All" = "coral1")) +
  theme_minimal() +
  labs(x = NULL, y = "Value") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 10, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(color = "black", fill = NA)
  )

print(p)

# -------------------- End --------------------
cat("Combined boxplot generation complete.\n")
