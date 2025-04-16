# ============================================================
# Title: Environmental Variable Quantiles for Banana Presence in Region of Interest
# Author: Cressida Hoddell
# Description: This script loads raster data, separates banana
#              presence/absence, and computes quantile summaries
#              for key environmental variables.
# ============================================================

# -------------------- Step 1: Install and Load Required Libraries --------------------
install.packages(c("terra", "ggplot2", "dplyr", "tidyr"))
library(terra)   # For raster handling
library(ggplot2) # For visualization (not used here, but loaded)
library(dplyr)   # For data manipulation
library(tidyr)   # For reshaping data

# -------------------- Step 2: Load the GeoTIFF File --------------------
file_path <- "Banana_and_Environmental_Factors_1km_Final5.tif"
raster_data <- rast(file_path)

# Display metadata and band names
print(raster_data)
names(raster_data)

# -------------------- Step 3: Convert Raster to Data Frame --------------------
df <- as.data.frame(raster_data, xy = TRUE, na.rm = TRUE)
print(names(df))

# Rename columns for clarity
colnames(df) <- c("x", "y", "Banana_Presence", "Soil_pH", "Population_Density",
                  "Elevation", "Annual_Precipitation", "Precip_Seasonality", "Temperature_Seasonality", 
                  "Diurnal_Temperature_Range", "Temperature_Max", "Temperature_Min", "Average_Temperature")

# Remove rows with NA in Banana_Presence
df <- df %>% filter(!is.na(Banana_Presence))

# -------------------- Step 4: Separate Banana and Non-Banana Pixels --------------------
banana_df <- df %>% filter(Banana_Presence == 1)
non_banana_df <- df %>% filter(Banana_Presence == 0)

# -------------------- Step 5: Compute Quantiles for Each Variable --------------------
variables <- c("Soil_pH", "Elevation", "Annual_Precipitation", "Precip_Seasonality", "Population_Density",
               "Temperature_Seasonality", "Diurnal_Temperature_Range", "Temperature_Max", 
               "Temperature_Min", "Average_Temperature")

str(banana_df)
str(non_banana_df)

# Function to compute quantiles
compute_quantiles <- function(df, category) {
  df <- as.data.frame(df)
  
  df %>%
    dplyr::select(all_of(variables)) %>%
    dplyr::summarise(across(where(is.numeric), list(
      Q05 = ~ quantile(., 0.05, na.rm = TRUE),
      Q25 = ~ quantile(., 0.25, na.rm = TRUE),
      Q50 = ~ quantile(., 0.50, na.rm = TRUE),
      Q75 = ~ quantile(., 0.75, na.rm = TRUE),
      Q95 = ~ quantile(., 0.95, na.rm = TRUE)
    ))) %>%
    mutate(Banana_Presence = category) %>%
    relocate(Banana_Presence)
}

# Check data frames
if (!is.data.frame(banana_df)) stop("Error: banana_df is not a valid dataframe.")
if (!is.data.frame(non_banana_df)) stop("Error: non_banana_df is not a valid dataframe.")

# Compute and display quantiles
banana_quantiles <- compute_quantiles(banana_df, category = "Presence")
non_banana_quantiles <- compute_quantiles(non_banana_df, category = "Absence")

cat("Quantiles for Banana Presence:\n")
print(banana_quantiles)

cat("Quantiles for Banana Absence:\n")
print(non_banana_quantiles)

# -------------------- End --------------------
cat("Quantile computation complete.\n")
