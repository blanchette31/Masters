# prepare workspace
rm(list = ls())

# load libraries 
library(tidyverse)
library(lmodel2)
library(purrr)


# load data
df <- read.csv("Data/Processed/lake/co2_o2_post_lf.csv", header = TRUE)

years_to_run <- c(2015:2019, 2021, 2022)
nboot <- 10000
set.seed(123)

results <- data.frame(
  year = integer(),
  MA_slope = numeric(),
  CI_lower = numeric(),
  CI_upper = numeric(),
  p_value = numeric(),
  sig_vs_minus1 = character()
)

for (yr in years_to_run) {
  df_year <- df %>% filter(year == yr & !is.na(co2) & !is.na(o2))
  
  # Bootstrap MA slopes
  boot_slopes <- replicate(nboot, {
    samp <- df_year[sample(1:nrow(df_year), replace = TRUE), ]
    suppressMessages(mod <- lmodel2(o2 ~ co2, data = samp))
    mod$regression.results$Slope[2]  # Major Axis
  })
  
  # Observed slope
  ma_slope <- mean(boot_slopes)
  
  # 95% CI
  ci_lower <- quantile(boot_slopes, 0.025)
  ci_upper <- quantile(boot_slopes, 0.975)
  
  # Two-tailed p-value vs -1
  p_val <- 2 * min(mean(boot_slopes <= -1), mean(boot_slopes >= -1))
  p_val <- min(p_val, 1)  # cap at 1
  
  # Significance based on CI
  sig <- ifelse(-1 < ci_lower | -1 > ci_upper, "Yes", "No")
  
  # Add row
  results <- rbind(results, data.frame(
    year = yr,
    MA_slope = ma_slope,
    CI_lower = ci_lower,
    CI_upper = ci_upper,
    p_value = p_val,
    sig_vs_minus1 = sig
  ))
}

results

write.csv(results, "Data/Processed/comments/slopes_vs_hypothetical.csv")