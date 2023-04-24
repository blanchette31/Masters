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
      do(model = gls(as.formula(paste(response, "~", predictor)), data = ., na.action = na.omit,))
  }
  
  # Extract residuals and fitted values
  residuals_df <- models %>%
    rowwise() %>%
    mutate(residuals = list(residuals(model)),
           fitted_values = list(fitted(model))) %>%
    unnest(cols = c(residuals, fitted_values))
  
  # Extract AIC
  aic_values <- models %>%
    rowwise() %>%
    mutate(aic = AIC(model))
  
  # Plot residuals vs fitted values
  p1 <- ggplot(residuals_df, aes(x = fitted_values, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(~ year) +
    labs(title = paste("Residuals vs Fitted for", response),
         x = "Fitted Values",
         y = "Residuals")
  
  # Plot QQ plot of residuals
  p2 <- ggplot(residuals_df, aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~ year) +
    labs(title = paste("QQ Plot for", response),
         x = "Theoretical Quantiles",
         y = "Sample Quantiles")
  
  return(list(aic_values = aic_values, residuals_vs_fitted = p1, qq_plot = p2))
}

# Check residuals and AIC for CO2 without AR(1)
co2_gls_no_ar1 <- check_gls_residuals(lf, "co2", "doy", use_ar1 = FALSE)

# Check residuals and AIC for CO2 with AR(1)
co2_gls_ar1 <- check_gls_residuals(lf, "co2", "doy", use_ar1 = TRUE)

# Check residuals and AIC for O2 without AR(1)
o2_gls_no_ar1 <- check_gls_residuals(lf, "o2", "doy", use_ar1 = FALSE)

# Check residuals and AIC for O2 with AR(1)
o2_gls_ar1 <- check_gls_residuals(lf, "o2", "doy", use_ar1 = TRUE)

# Display AIC values
print("AIC values for CO2 models without AR(1):")
print(co2_gls_no_ar1$aic_values)
print("AIC values for CO2 models with AR(1):")
print(co2_gls_ar1$aic_values)
print("AIC values for O2 models without AR(1):")
print(o2_gls_no_ar1$aic_values)
print("AIC values for O2 models with AR(1):")
print(o2_gls_ar1$aic_values)

# Display plots for CO2
print(co2_gls_no_ar1$residuals_vs_fitted)
print(co2_gls_no_ar1$qq_plot)
print(co2_gls_ar1$residuals_vs_fitted)
print(co2_gls_ar1$qq_plot)

# Display plots for O2
print(o2_gls_no_ar1$residuals_vs_fitted)
print(o2_gls_no_ar1$qq_plot)
print(o2_gls_ar1$residuals_vs_fitted)
print(o2_gls_ar1$qq_plot)
