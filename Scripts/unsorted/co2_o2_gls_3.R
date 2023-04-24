## Prepare workspace 
rm(list = ls())

## Load libraries 
library(emmeans)
library(sjPlot)
library(tidyverse)
library(lme4)
library(ggpubr)
library(cowplot)
library(corrplot)
library(lattice)
library(nlme)
library(Metrics) # for RMSE calculation

## Load data 
lf <- read.csv("Data/Processed/lake/co2_o2_post_lf.csv", header = TRUE)

# Convert 'year' to a factor for grouping in the model
lf$year <- as.factor(lf$year)

# Function to fit GLS models and check residuals for each year
check_gls_residuals <- function(df, response, predictor, use_ar1 = FALSE) {
  if (use_ar1) {
    models <- df %>%
      group_by(year) %>%
      do(model = gls(as.formula(paste(response, "~", predictor)), 
                     data = ., na.action = na.omit,
                     correlation = corAR1(form = ~ doy)))
  } else {
    models <- df %>%
      group_by(year) %>%
      do(model = gls(as.formula(paste(response, "~", predictor)), data = ., na.action = na.omit))
  }
  
  # Extract residuals, fitted values, and calculate additional metrics
  metrics_df <- models %>%
    rowwise() %>%
    mutate(aic = AIC(model),
           rmse = rmse(na.omit(df[df$year == unique(year), response]), fitted(model)),
           slope = coef(model)[[2]],
           r_squared = cor(fitted(model), na.omit(df[df$year == unique(year), response]))^2) %>%
    select(year, aic, rmse, slope, r_squared)
  
  # Create residuals vs fitted values and QQ plot
  results_df <- models %>%
    rowwise() %>%
    mutate(residuals = list(residuals(model)),
           fitted_values = list(fitted(model))) %>%
    unnest(cols = c(residuals, fitted_values))
  
  # Plot residuals vs fitted values
  p1 <- ggplot(results_df, aes(x = fitted_values, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(~ year) +
    labs(title = paste("Residuals vs Fitted for", response),
         x = "Fitted Values",
         y = "Residuals")
  
  # Plot QQ plot of residuals
  p2 <- ggplot(results_df, aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~ year) +
    labs(title = paste("QQ Plot for", response),
         x = "Theoretical Quantiles",
         y = "Sample Quantiles")
  
  return(list(metrics = metrics_df, 
              residuals_vs_fitted = p1, 
              qq_plot = p2))
}

# Check residuals and metrics for CO2 with AR(1)
co2_gls_ar1 <- check_gls_residuals(lf, "co2", "doy", use_ar1 = TRUE)

# Check residuals and metrics for O2 with AR(1)
o2_gls_ar1 <- check_gls_residuals(lf, "o2", "doy", use_ar1 = TRUE)

# Combine metrics into a single table
combined_metrics <- bind_rows(
  co2_gls_ar1$metrics %>% mutate(model = "CO2_AR1"),
  o2_gls_ar1$metrics %>% mutate(model = "O2_AR1")
) %>% arrange(year)

# Display combined metrics table
print(combined_metrics)

# Display plots for CO2
print(co2_gls_ar1$residuals_vs_fitted)
print(co2_gls_ar1$qq_plot)

# Display plots for O2
print(o2_gls_ar1$residuals_vs_fitted)
print(o2_gls_ar1$qq_plot)
