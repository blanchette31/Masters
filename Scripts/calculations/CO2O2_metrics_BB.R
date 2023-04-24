#Metrics calculation for analyzing O2 and CO2 depature timeseries
#by Dominic Vachon, domvachon@gmail.com
#2019-10-08
#O2-CO2 metrics framework is presented in Vachon et al. (in revision), Limnology and Oceanography Letters

# Adapted by Brandon Blanchette 12/12/2023

# prepare workspace 
rm(list = ls())

#Load library to plot ellipse and to calcuate type II regressions
library(ellipse)
library(lmodel2)

# Load O2 and CO2 data

lf <- read.csv("Data/Processed/lake/co2_o2_post_lf.csv")
col.keep <- data.frame(year = lf$year, co2dep=lf$co2, o2dep = lf$o2)
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
  # # OLS regression for slope calculation
  # reg_year <- lm(subset_data$o2dep ~ subset_data$co2dep)
  # slope_year <- coef(reg_year)[2]  # Extract the slope coefficient
  
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
    stretch = ell.len_year[1],
    slope_year = slope_year
  )
  
  # Append metrics for the current year to the list
  metrics_by_year[[as.character(year)]] <- metrics_year
  

}




# Combine metrics for all years into a single data frame
metrics_all_years <- do.call(rbind, metrics_by_year)

write.csv(metrics_all_years, "Data/Processed/combined/metrics_all_years.csv")

