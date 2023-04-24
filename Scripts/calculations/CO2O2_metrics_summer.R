
#Metrics calculation for analyzing O2 and CO2 depature timeseries
#by Dominic Vachon, domvachon@gmail.com
#2019-10-08
#O2-CO2 metrics framework is presented in Vachon et al. (in revision), Limnology and Oceanography Letters

# Adapted by Brandon Blanchette 12/12/2023
#prepare workspace
rm(list = ls())

# load libraries

#Load library to plot ellipse and to calcuate type II regressions
library(ellipse)
library(lmodel2)
library(tidyverse)
library(ggpubr)

#load data
df <- read.csv("Data//Processed//boue//delta_co2-2_model_2015.csv", header = TRUE)
pheno <- read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)

df = merge(df, pheno[, c("doy_start", "doy_end", "year")],
           by = "year")

df <- df %>%
  rowwise() %>%
  mutate( period_rel_leaf_fall = case_when(
    doy >= doy_start ~ "after",
    TRUE ~ "summer"))

df_date <- df %>% 
  group_by(date, year, doy, period_rel_leaf_fall) %>% 
  summarise(co2 = mean(delta.CO2),
            o2 = mean(do_deviation),
            o2_meta = mean(do_deviation_meta),
            o2_hypo = mean(do_deviation_hypo))

date_filt <- df_date %>% 
  filter(!is.na(co2) | !is.na(o2))



summer_lf <- df_date[df_date$period_rel_leaf_fall == "summer",]




col.keep <- data.frame(year = summer_lf$year, co2dep=summer_lf$co2, o2dep = summer_lf$o2)
df_keep <- na.omit(col.keep)


# Create an empty list to store metrics for each year
metrics_by_year <- list()

# Loop through unique years in lf DataFrame
for (year in unique(df_keep$year)) {
  # Subset data for the current year
  subset_data <- df_keep[df_keep$year == year, c("co2dep", "o2dep")]
  
  # Calculate means
  mu_year <- colMeans(subset_data, na.rm = TRUE)
  
  # Ellipse axis length calculation
  corMat_year <- cor(subset_data)
  covMat_year <- var(subset_data)
  evals_year <- eigen(covMat_year)$values
  ell.len_year <- 2 * sqrt(5.991 * evals_year)
# 
#   # OLS regression for slope calculation
#   reg_year <- lm(subset_data$o2dep ~ subset_data$co2dep)
#   slope_year <- coef(reg_year)[2]  # Extract the slope coefficient
# 
  # Type II regression for slope calculation
  reg_year <- lmodel2(subset_data$o2dep ~ subset_data$co2dep, nperm = 99)
  slope_year <- reg_year$regression.results[2, 3]

  # Gathering metrics for the current year
  metrics_year <- data.frame(
    year = year,
    meanCO2dep = mu_year[1],
    meanO2dep = mu_year[2],
    offset = mu_year[1] + mu_year[2],
    EQ = 1 / abs(slope_year),
    width = ell.len_year[2],
    stretch = ell.len_year[1]
  )
  
  # Append metrics for the current year to the list
  metrics_by_year[[as.character(year)]] <- metrics_year
  
  
}




# Combine metrics for all years into a single data frame
metrics_all_years <- do.call(rbind, metrics_by_year)
metrics_all_years

write.csv(metrics_all_years, "Data/Processed/combined/metrics_all_years_summer.csv")
