# ============================================================
# Title: Correlation, PERMANOVA, and PCA of Banana Suitability
# Author: Cressida Hoddell
# Description: This script performs correlation analysis,
#              PERMANOVA, and PCA on banana presence data
#              and associated environmental variables.
# ============================================================

# -------------------- Step 1: Install and Load Required Libraries --------------------
install.package <- c("vegan")
install.packages("corrplot")
library(corrplot)
install.packages("caret")
library(caret)

library(terra)
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)

# -------------------- Step 2: Load the GeoTIFF Raster Data --------------------
file_path <- "Banana_and_Environmental_Factors_1km_Final5.tif"
raster_data <- rast(file_path)
df <- as.data.frame(raster_data, xy = TRUE, na.rm = TRUE)
df$Soil_pH <- df$Soil_pH / 10
colnames(df) <- c("x", "y", "Banana_Presence", "Soil_pH", "Population_Density",
                  "Elevation", "Annual_Precipitation", "Precip_Seasonality", "Temperature_Seasonality", 
                  "Diurnal_Temperature_Range", "Temperature_Max", "Temperature_Min", "Average_Temperature")

# -------------------- Step 3: Prepare Clean Data --------------------
df_clean <- df %>%
  dplyr::select(Banana_Presence, Soil_pH, Elevation, Annual_Precipitation, Precip_Seasonality,
                Population_Density, Temperature_Seasonality, Diurnal_Temperature_Range,
                Temperature_Max, Temperature_Min, Average_Temperature) %>%
  drop_na() %>%
  mutate(Banana_Presence = factor(Banana_Presence, levels = c(0,1), labels = c("Absence", "Presence")))

variables <- c("Soil_pH", "Elevation", "Annual_Precipitation", "Precip_Seasonality",
               "Population_Density", "Temperature_Seasonality", "Diurnal_Temperature_Range",
               "Temperature_Max", "Temperature_Min", "Average_Temperature")

# -------------------- Downsample for Balance --------------------
set.seed(123)
presence <- df_clean %>% filter(Banana_Presence == "Presence")
absence  <- df_clean %>% filter(Banana_Presence == "Absence")
n_min <- min(nrow(presence), nrow(absence))
presence_sample <- presence %>% sample_n(n_min)
absence_sample  <- absence %>% sample_n(n_min)
df_sub <- bind_rows(presence_sample, absence_sample)

# -------------------- Step 4A: Correlation Matrix --------------------
variable_labels <- c("Soil_pH" = "ph", "Elevation" = "ele", "Annual_Precipitation" = "prec", 
                     "Precip_Seasonality" = "psea", "Population_Density" = "pop", 
                     "Temperature_Seasonality" = "tsea", "Diurnal_Temperature_Range" = "tdir", 
                     "Temperature_Max" = "tmax", "Temperature_Min" = "tmin", 
                     "Average_Temperature" = "tavg")

cor_matrix <- cor(df_sub[, variables], method = "pearson")
colnames(cor_matrix) <- variable_labels[variables]
rownames(cor_matrix) <- variable_labels[variables]

cat("\nCorrelation Matrix of Environmental Variables:\n")
print(round(cor_matrix, 2))

corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.cex = 0.9,
         number.cex = 0.7,
         addCoef.col = "black", 
         title = "",
         mar = c(0, 0, 1, 0))

title(main = "Correlation Between Environmental Variables", cex.main = 1)

# -------------------- Step 4B: PERMANOVA --------------------
permanova_result <- adonis2(
  df_sub[, variables] ~ Banana_Presence,
  data = df_sub,
  method = "euclidean",
  permutations = 999
)

cat("PERMANOVA Results:\n")
print(permanova_result)

# -------------------- Step 5: PCA --------------------
pca <- prcomp(df_sub[, variables], scale. = TRUE)
pca_df <- as.data.frame(pca$x) %>%
  mutate(Banana = df_sub$Banana_Presence)

loadings <- as.data.frame(pca$rotation[, 1:2])
loadings$Variable <- rownames(loadings)
loadings$Label <- recode(loadings$Variable, !!!variable_labels)

arrow_scale <- 4
loadings_scaled <- loadings %>%
  mutate(PC1 = PC1 * arrow_scale,
         PC2 = PC2 * arrow_scale)

pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Banana)) +
  geom_density2d(linewidth = 1) +
  geom_segment(data = loadings_scaled,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black") +
  geom_text(data = loadings_scaled,
            aes(x = PC1, y = PC2, label = Label),
            color = "black", vjust = 1.5, size = 4) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) +
  labs(title = "PCA of Environmental Conditions with Variable Loadings",
       subtitle = "Arrows show variable correlations with PC axes",
       x = paste0("PC1 (", round(summary(pca)$importance[2,1]*100, 1), "%)"),
       y = paste0("PC2 (", round(summary(pca)$importance[2,2]*100, 1), "%)"),
       color = "Banana")

print(pca_plot)
ggsave("PCA_Banana_with_Arrows.png", pca_plot, width = 8, height = 6, dpi = 300)

# -------------------- Done --------------------
cat("\nAnalysis complete. PCA plot saved and PERMANOVA results printed.\n")
