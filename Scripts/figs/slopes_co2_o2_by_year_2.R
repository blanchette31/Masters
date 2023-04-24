## prepare workspace 

rm(list = ls())

## load libraries 

library(emmeans)
library(sjPlot)
library(tidyverse)
library(lme4)
library(ggpubr)
library(cowplot)
library(corrplot)
library(lattice)





## fixed effects need to be independant
## residuals have to be normally distributed
## load data 

lf <- read.csv("Data/Processed/lake/co2_o2_post_lf.csv", header = TRUE)

# Convert 'year' to a factor for grouping in the model
lf$year <- as.factor(lf$year)

# Fit linear models for each year
co2_slopes_df <- lf %>%
  group_by(year) %>%
  summarize(co2_intercept = coef(lm(co2 ~ doy))[1],
            co2_slope = coef(lm(co2 ~ doy))[2],
            co2_R_squared = summary(lm(co2 ~ doy))$r.squared)

o2_slopes_df <- lf %>%
  group_by(year) %>%
  summarize(o2_intercept = coef(lm(o2 ~ doy))[1],
            o2_slope = coef(lm(o2 ~ doy))[2],
            o2_R_squared = summary(lm(o2 ~ doy))$r.squared)

# Merge slope information back into the main data frame
lf <- left_join(lf, co2_slopes_df, by = "year")
lf <- left_join(lf, o2_slopes_df, by = "year")

labels_df <- data.frame(
  doy = c(244, 274, 305),
  label = c("September", "October", "November")
)

# Plot for CO2 with facet labels
p_co2 <- ggplot(lf, aes(x = doy, y = co2)) +
  geom_smooth(method = lm, se = FALSE, formula = y ~ x) +
  geom_point() +
  theme_classic() +
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label) +
  facet_wrap(~year, ncol = 3) +
  labs(title = expression(paste("Yearly ", Delta*CO[2], " accumulation rates post start of leaf fall")), x = "")+
  ylab(expression(paste("Average daily ", Delta*CO[2],  " deviation from saturation ( ", mu,"mol/L)")))+
  geom_text(aes(label = paste("y =", round(co2_slope, 3), "x +", round(co2_intercept, 2),
                              "\nR^2:", round(co2_R_squared, 2))),
            x = 259, y = 60, size = 3, data = co2_slopes_df)

# Plot for O2 with facet labels
p_o2 <- ggplot(lf, aes(x = doy, y = o2)) +
  geom_smooth(method = lm, se = FALSE, formula = y ~ x) +
  geom_point() +
  theme_classic() +
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label) +
  facet_wrap(~year, ncol = 3) +
  labs(title = expression(paste("Yearly ", Delta*O[2], " depletion rates post start of leaf fall")), x = "")+
  ylab(expression(paste("Average daily ", Delta*O[2],  " deviation from saturation ( ", mu,"mol/L)")))+
  geom_text(aes(label = paste("y =", round(o2_slope, 3), "x +", round(o2_intercept, 2),
                              "\nR^2:", round(o2_R_squared, 2))),
            x = 256, y = -90, size = 3, data = o2_slopes_df)

# Print the plots
print(p_co2)
print(p_o2)

ggsave("Data//Figures//lf_co2_over_doy2.jpg",p_co2, dpi = 300, width = 10, height = 6, units = "in")
ggsave("Data//Figures//lf_o2_over_doy2.jpg",p_o2, dpi = 300, width = 10, height = 6, units = "in")

# Function to calculate metrics and sample information
calculate_model_metrics <- function(model, response_variable, year) {
  predictions <- predict(model)
  residuals <- residuals(model)
  rmse <- sqrt(mean(residuals^2))
  rsquared <- summary(model)$r.squared
  p_value <- summary(model)$coefficients[2, 4]  # p-value for the predictor variable
  num_samples <- length(residuals)
  min_value <- min(predictions)
  max_value <- max(predictions)
  
  # Create a data frame with dynamically named columns
  result <- data.frame(
    year = year,  # Use the provided year value
    response_variable = response_variable,
    rmse = rmse,
    rsquared = rsquared,
    p_value = p_value,
    num_samples = num_samples,
    min_value = min_value,
    max_value = max_value
  )
  
  return(result)
}

# Fit linear models for each year and calculate metrics and sample information
co2_metrics_df <- lf %>%
  group_by(year) %>%
  do({
    model <- lm(co2 ~ doy, data = .)
    metrics <- calculate_model_metrics(model, "co2", unique(.$year))
    metrics
  }) %>%
  ungroup()  # Remove grouping

o2_metrics_df <- lf %>%
  group_by(year) %>%
  do({
    model <- lm(o2 ~ doy, data = .)
    metrics <- calculate_model_metrics(model, "o2", unique(.$year))
    metrics
  }) %>%
  ungroup()  # Remove grouping

# Display the result
print(co2_metrics_df)
print(o2_metrics_df)
