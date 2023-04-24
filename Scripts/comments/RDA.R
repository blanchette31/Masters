# --- Prepare workspace ---
rm(list = ls())

# --- Load packages ---
library(tidyverse)
library(vegan)

# --- Import data ---
df <- read.csv("Data/Processed/comments/comp_years_V2_with_rad_fall_per_day.csv")

# --- Define matrices ---
Y <- df %>% select(delta_co2_rate, delta_o2_rate, EQ)
X <- df %>% select(-c(year, delta_co2_rate, delta_o2_rate, EQ, cum_rad_fall, width, stretch, cum_temp_sum, cum_rad_sum, offset,sum_wind_fall, max_wind_fall,leaf_fall_dur,departure_slp_abs, g_C_CO2_per_day, avg_temp_sum))
groups <- as.factor(df$year)

# --- Scale predictors ---
X_scaled <- as.data.frame(scale(X))

# --- Run RDA (after reducing collinear vars if needed) ---
rda_model <- rda(Y ~ ., data = X_scaled)

# --- Extract scores ---
site_scores <- as.data.frame(scores(rda_model, display = "sites", scaling = 2))
site_scores$year <- groups

env_scores <- as.data.frame(scores(rda_model, display = "bp", scaling = 2))
env_scores$variable <- rownames(env_scores)

resp_scores <- as.data.frame(scores(rda_model, display = "species", scaling = 2))
resp_scores$response <- rownames(resp_scores)

# --- Calculate variance explained ---
var_expl <- summary(rda_model)$cont$importance[2, 1:2] * 100

# --- ggplot2 biplot ---
ggplot() +
  # 1. Points for each year
  geom_point(data = site_scores, aes(x = RDA1, y = RDA2, color = year), size = 4) +
  geom_text(data = site_scores, aes(x = RDA1, y = RDA2, label = year, color = year),
            vjust = -1, size = 4, show.legend = FALSE) +
  
  # 2. Arrows for environmental variables
  geom_segment(data = env_scores,
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.25, "cm")), color = "gray40") +
  geom_text(data = env_scores,
            aes(x = RDA1 * 1.1, y = RDA2 * 1.1, label = variable),
            color = "gray20", size = 3) +
  
  # 3. Arrows for dependent variables (response)
  geom_segment(data = resp_scores,
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.25, "cm")), color = "red") +
  geom_text(data = resp_scores,
            aes(x = RDA1 * 1.15, y = RDA2 * 1.15, label = response),
            color = "red3", size = 4, fontface = "bold") +
  
  labs(
    x = paste0("RDA1 (", round(var_expl[1], 1), "%)"),
    y = paste0("RDA2 (", round(var_expl[2], 1), "%)"),
    title = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.position = "right") +
  coord_equal()



anova(rda_model)
