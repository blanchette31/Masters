# Prepare workspace
rm(list = ls())

# Load libraries 
library(tidyverse)
library(scales)
library(ggpubr)
library(MetBrewer)
library(patchwork)
library(nlme)

# Load data 
df <- read.csv("Data/Processed/combined/comp_years.csv", header = TRUE)
lf <- read.csv("Data/Processed/lake/co2_o2_post_lf.csv", header = TRUE)

# Convert 'year' to a factor for grouping in the model
lf$year <- as.factor(lf$year)

# Filter data to only include points between DOY 257 and 298
lf <- lf %>% filter(doy >= 257 & doy <= 305)

comp <- df %>% 
  select(year, delta_co2_rate, delta_o2_rate)
comp$year <- as.factor(comp$year)

labels_df <- data.frame(
  doy = c(244, 274, 305),
  label = c("Sept", "Oct", "Nov")
)
lf$year <- factor(lf$year, levels = rev(levels(lf$year)))
lf$year <- factor(lf$year, levels(lf$year)[c(5, 2, 1, 3, 6, 7, 4)])

# Initialize an empty data frame to store model statistics
model_stats <- data.frame(
  year = character(),
  slope_co2 = numeric(),
  intercept_co2 = numeric(),
  r_squared_co2 = numeric(),
  rmse_co2 = numeric(),
  slope_o2 = numeric(),
  intercept_o2 = numeric(),
  r_squared_o2 = numeric(),
  rmse_o2 = numeric(),
  stringsAsFactors = FALSE
)

# Fit GLS models with AR1 correlation structure, predict values for plotting, and store statistics
lf <- lf %>%
  group_by(year) %>%
  filter(!is.na(co2) & !is.na(o2)) %>%  # Remove rows with missing values
  do({
    co2_model <- gls(co2 ~ doy, data = ., correlation = corAR1(form = ~ doy))
    o2_model <- gls(o2 ~ doy, data = ., correlation = corAR1(form = ~ doy))
    
    # Calculate RMSE and R-squared for CO2
    co2_pred <- predict(co2_model)
    co2_actual <- .$co2
    r_squared_co2 <- 1 - (sum((co2_actual - co2_pred)^2) / sum((co2_actual - mean(co2_actual))^2))
    rmse_co2 <- sqrt(mean((co2_actual - co2_pred)^2))
    
    # Calculate RMSE and R-squared for O2
    o2_pred <- predict(o2_model)
    o2_actual <- .$o2
    r_squared_o2 <- 1 - (sum((o2_actual - o2_pred)^2) / sum((o2_actual - mean(o2_actual))^2))
    rmse_o2 <- sqrt(mean((o2_actual - o2_pred)^2))
    
    # Store statistics in a temporary data frame
    temp_stats <- data.frame(
      year = unique(.$year),
      slope_co2 = coef(co2_model)["doy"],
      intercept_co2 = coef(co2_model)["(Intercept)"],
      r_squared_co2 = r_squared_co2,
      rmse_co2 = rmse_co2,
      slope_o2 = coef(o2_model)["doy"],
      intercept_o2 = coef(o2_model)["(Intercept)"],
      r_squared_o2 = r_squared_o2,
      rmse_o2 = rmse_o2
    )
    
    # Append temp_stats to model_stats
    model_stats <<- bind_rows(model_stats, temp_stats)
    
    data.frame(
      .,
      gls_co2 = co2_pred,
      gls_o2 = o2_pred
    )
  })

# Plot GLS regression for CO2
p1_1 <- ggplot(lf, aes(x = doy, y = co2, color = year)) +
  geom_line(aes(y = gls_co2), linetype = "solid", linewidth = 1.1) +  # GLS regression line
  geom_point(alpha = 0.35) +
  theme_classic() +
  scale_color_manual(values = met.brewer("Hiroshige", 8), name = "Year") +
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label) +
  labs(title = "", y = "", x = "") +
  theme(legend.position = "none")

# Plot GLS regression for O2
p2_1 <- ggplot(lf, aes(x = doy, y = o2, color = year)) +
  geom_line(aes(y = gls_o2), linetype = "solid", linewidth = 1.1) +  # GLS regression line
  geom_point(alpha = 0.35) +
  theme_classic() +
  scale_color_manual(values = met.brewer("Hiroshige", 8)) +
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label) +
  labs(title = "", y = "", x = "") +
  theme(legend.position = "right")

# Display plots
p1_1 + p2_1

# Display model statistics table
print(model_stats)


write.csv(model_stats, "Data/Processed/comments/amodel_stats_summary_14-sept-2024_nov-1.csv", row.names = FALSE)