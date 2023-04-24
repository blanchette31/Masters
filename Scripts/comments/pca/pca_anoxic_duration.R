# Prepare workspace
rm(list = ls())

# Load libraries
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(RColorBrewer)
library(MetBrewer)
library(ggrepel)
library(cowplot)

# Load data
df <- read.csv("Data/Processed/comments/comp_years_V2_with_rad_fall_per_day_new_hypoxic_duration.csv", header = TRUE, row.names = 1)

# Remove columns that won't be used for PCA
df_pca <- df %>% 
  select(-c(g_C_CO2_per_day, max_wind_fall, sum_wind_fall, cum_temp_sum, avg_temp_sum, offset, width, stretch, departure_slp_abs, cum_rad_sum,cum_rad_fall,hypoxic_dur))

df_pca_2 <- df_pca %>% 
  select(-c(leaf_fall_dur))

# df_pca <- df_pca %>% 
  # rename(
  #   `∆CO[2] accum` = delta_co2_rate,
  #   `∆O[2] depletion` = delta_o2_rate,
  #   `Mean temperature fall` = avg_temp_fall,
  #   `Rain volume fall` = vol_rain_fall_perc,
  #   `Hypolimnetic volume exchanged` = vol_hypo_exch,
  #   `Anoxic duration` = anoxic_duration,
  #   `Fall destratification rate` = schmidt_rate_leaf_fall,
  #   `Mean wind fall` = mean_wind_fall,
  #   `Leaf fall duration` = leaf_fall_dur,
  #   `EQ` = EQ,
  #   `Cumm. fall radiation`= cum_rad_fall
  # )

# PCA
res.pca <- PCA(df_pca, graph = FALSE)
res.pca_2 <- PCA(df_pca_2, graph = FALSE)
# Variable names to color in blue
blue_vars <- c("delta_co2_rate", "delta_o2_rate", "EQ")

# Create color vector for variable arrows
arrow_colors <- ifelse(rownames(res.pca_2$var$coord) %in% blue_vars, "blue", "black")

# Individual colors (already assigned)
col_ind <- c("#44adce", "#8ad4dd", "#f2393a", "#1f5486", "#fde0a1", "#fe691d", "#ffc047")

# Get the number of individuals
n <- nrow(res.pca$ind$coord)

# Assign colors from the palette to each individual
ind_colors <- col_ind[1:n]

# Visualize biplot with updated arrow and point colors
p2 <- fviz_pca_biplot(res.pca_2, repel = TRUE,
                      col.var = arrow_colors, # Custom arrow colors
                      col.ind = ind_colors,  # Retain original point colors
                      geom.ind = "point",    # Points for individuals
                      pointshape = 19) + 
  geom_text_repel(aes(label = rownames(res.pca$ind$coord)),
                  color = "black", size = 4, fontface = "bold") + 
  scale_color_identity() +  # Prevents the creation of a legend for points
  theme(legend.position = "none",       # Remove legend
        axis.text.x = element_text(color = "black"),  # Set axis text color
        axis.text.y = element_text(color = "black")) +
  labs(title = NULL) +
  theme_cowplot() 

p2

ggsave("Data/Figures/comments/PCA_rad_per_day_hypoxic_duration.jpg", p2, dpi = 300, height = 6, width = 10, units = "in")



# Extract the percentages of variance explained
pc1_var <- round(res.pca_2$eig[1, 2], 1)  # Percentage of variance for Component 1
pc2_var <- round(res.pca_2$eig[2, 2], 1)  # Percentage of variance for Component 2

# Update plot with new axis labels
p3 <- fviz_pca_biplot(res.pca_2, 
                      label = "none",          # Remove both variable and individual labels
                      col.var = arrow_colors,  # Custom arrow colors
                      col.ind = ind_colors,    # Retain original point colors
                      geom.ind = "point",      # Only display points for individuals
                      pointshape = 19,         # Use solid circles for points
                      pointsize = 3) + 
  scale_color_identity() +                     # Prevents the creation of a legend for points
  theme_cowplot()+
  theme(legend.position = "none",              # Remove legend
        axis.text.x = element_text(color = "black"),  # Set axis text color
        axis.text.y = element_text(color = "black")) +
  labs(title = NULL,
       x = paste0("Component 1 (", pc1_var, "%)"),
       y = paste0("Component 2 (", pc2_var, "%)"))

p3


ggsave("Data/Figures/PCA_rap_per_day_no_labels.jpg", p3, dpi = 300, height = 6, width = 10, units = "in")
