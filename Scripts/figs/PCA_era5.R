## prepare workspace
rm(list = ls())

#load libraries
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(RColorBrewer)
library(MetBrewer)
library(ggrepel)
library(cowplot)

# load data
df <- read.csv("Data/Processed/combined/comp_years_V2.csv", header = TRUE, row.names = 1)

# Remove columns that won't be used for PCA
df_pca <- df %>% 
select(-c(g_C_CO2_per_day, max_wind_fall, sum_wind_fall, cum_temp_sum, avg_temp_sum, offset, width, stretch,departure_slp_abs, cum_rad_sum))

df_pca_2 <- df_pca %>% 
  select(-c(leaf_fall_dur))


df_pca <- df_pca %>% 
  rename(
    `∆CO[2] accum` = delta_co2_rate,
    `∆O[2] depletion` = delta_o2_rate,
   `Mean temperature fall` = avg_temp_fall,
   `Rain volume fall` = vol_rain_fall_perc,
   `Hypolimnetic volume exchanged` = vol_hypo_exch,
   `Hypoxic duration` = hypoxic_dur,
   `Fall destratification rate` = schmidt_rate_leaf_fall,
   `Mean wind fall` = mean_wind_fall ,
    `Leaf fall duration` = leaf_fall_dur,
   `EQ` = EQ,
   `Cumm. fall radiation`= cum_rad_fall
  )
# PCA
res.pca <- PCA(df_pca, graph = FALSE)


res.pca_2 <- PCA(df_pca_2, graph = FALSE)

# # Replace special characters with superscript/subscript markup
# var_labels <- c(
#   md("CO<sub>2\accum.") = expression(paste(Delta*CO[2], "accum.")),
#   "∆O[2] accum." = expression(paste(Delta*O[2], "accum.")),
#   "Cumm. summer radiation" = "Cumm. summer radiation",
#   "Mean temperature summer" = "Mean temperature summer",
#   "Rain volume fall" = "Rain volume fall",
#   "Hypolimnetic volume exchanged" = "Hypolimnetic volume exchanged",
#   "Hypoxic duration" = "Hypoxic duration",
#   "Fall destratification rate" = "Fall destratification rate",
#   "Mean wind fall" = "Mean wind fall",
#   "Leaf fall duration" = "Leaf fall duration",
#   "EQ" = "EQ",
#   "Cumm. fall radiation" = "Cumm. fall radiation"
# )

col_ind <- c("#44adce", "#8ad4dd", "#f2393a", "#1f5486", "#fde0a1", "#fe691d", "#ffc047")

# Get the number of individuals
n <- nrow(res.pca$ind$coord)

# Assign colors from the palette to each individual
ind_colors <- col_ind[1:n]

# Visualize biplot with updated labels
p <- fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "black", 
                col.ind = ind_colors,
                geom.ind = "point",
                pointshape = 19)+ 
  geom_text_repel(aes(label = rownames(res.pca$ind$coord)),
                  color = "black", size = 4, fontface = "bold") + 
  scale_color_identity() +  # Prevents the creation of a legend
  theme(legend.position = "none",       # Remove legend
        axis.text.x = element_text(color = "black"),  # Set individual names color to black
        axis.text.y = element_text(color = "black")) +
  labs(title = NULL) +
  theme_cowplot() 

p

ggsave("Data/Figures/PCA_V3.jpg", p, dpi = 300, height = 6, width = 10, units = "in")


# Visualize biplot with updated labels
p2 <- fviz_pca_biplot(res.pca_2, repel = TRUE,
                     col.var = "black", 
                     col.ind = ind_colors,
                     geom.ind = "point",
                     pointshape = 19)+ 
  geom_text_repel(aes(label = rownames(res.pca$ind$coord)),
                  color = "black", size = 4, fontface = "bold") + 
  scale_color_identity() +  # Prevents the creation of a legend
  theme(legend.position = "none",       # Remove legend
        axis.text.x = element_text(color = "black"),  # Set individual names color to black
        axis.text.y = element_text(color = "black")) +
  labs(title = NULL) +
  theme_cowplot() 

p2

ggsave("Data/Figures/PCA_no_lf_duration.jpg", p2, dpi = 300, height = 6, width = 10, units = "in")
  # theme(legend.position = "none") +

# # Adjust the position of individual labels
# p + geom_text_repel(aes(label = rownames(res.pca$ind$coord)),
#                     color = "black", size = 3,
#         0            box.padding = unit(0.35, "lines"),
#                     point.padding = unit(0.3, "lines"))
#
# # Visualize biplot with updated labels
# p <- fviz_pca_biplot(res.pca, repel = TRUE,
#                      col.var = "black", # Variables color
#                      col.ind = ind_colors, # Assign colors to individuals
#                      geom.ind = "point", # Use points for individuals
#                      pointshape = 19) + # Set point shape for individuals
#   scale_color_identity() +  # Prevents the creation of a legend
#   theme(legend.position = "none",       # Remove legend
#         axis.text.x = element_text(color = "black"),  # Set individual names color to black
#         axis.text.y = element_text(color = "black")) +
#   labs(title = NULL)
#
# # Adjust the position of individual labels
# p + geom_text_repel(aes(label = rownames(res.pca$ind$coord)),
#                     color = "black", size = 4,
#                     box.padding = unit(0.5, "lines"),
#                     point.padding = unit(0.4, "lines"))

# Visualize biplot (variables and individuals)
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "black", # Variables color
                col.ind = "grey30"  # Individuals color
                
)+
  labs(title = NULL)+
  theme_minimal()
# # Visualize eigenvalues 
# fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
# 
# # Visualize variables of PCA
# fviz_pca_var(res.pca, col.var = "black", repel= TRUE)
# 
# ind <- get_pca_ind(res.pca)



cor_pca <- cor(df_pca)

# Correlation plot for PCA variables
corrplot(cor_pca, method = "circle", type = "upper",col=brewer.pal(n=8, name="RdYlBu"))

# Correlation plot for PCA variables
corrplot(cor_pca, method = "circle", type = "upper", order = "hclust",col=brewer.pal(n=8, name="RdYlBu"))

