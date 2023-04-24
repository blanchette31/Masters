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

# Function to fit GLS models with AR(1) and check residuals for each year
check_gls_residuals <- function(df, response, predictor) {
  models <- df %>%
    group_by(year) %>%
    do(model = gls(as.formula(paste(response, "~", predictor)), 
                   data = ., na.action = na.omit,
                   correlation = corAR1(form = ~ doy)))
  
  # Extract residuals and fitted values
  residuals_df <- models %>%
    rowwise() %>%
    mutate(residuals = list(residuals(model)),
           fitted_values = list(fitted(model))) %>%
    unnest(cols = c(residuals, fitted_values))
  
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
  
  return(list(residuals_vs_fitted = p1, qq_plot = p2))
}

# Check residuals for CO2
co2_residuals_plots <- check_gls_residuals(lf, "co2", "doy")

# Check residuals for O2
o2_residuals_plots <- check_gls_residuals(lf, "o2", "doy")

# Display plots
print(co2_residuals_plots$residuals_vs_fitted)
print(co2_residuals_plots$qq_plot)
print(o2_residuals_plots$residuals_vs_fitted)
print(o2_residuals_plots$qq_plot)
