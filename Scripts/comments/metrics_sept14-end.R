# prepare workspace 
rm(list = ls())

# Load libraries
library(ellipse)
library(lmodel2)

# Load O2 and CO2 data
lf <- read.csv("Data/Processed/boue/delta_co2-2_model_2015.csv")

# Rename variables for clarity
lf <- lf %>%
  rename(
    co2 = delta.CO2,
    o2 = do_deviation
  )

# Filter for DOY >= 244 and remove missing values
col.keep <- lf %>%
  filter(doy >= 244) %>%
  select(year, co2dep = co2, o2dep = o2) %>%
  na.omit()

df_keep <- col.keep

# Create an empty list to store metrics for each year
metrics_by_year <- list()

# Loop through unique years
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
  
  # Type II regression for slope calculation
  reg_year <- lmodel2(subset_data$o2dep ~ subset_data$co2dep, nperm = 99)
  slope_year <- reg_year$regression.results[2, 3]
  
  # Gather metrics
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
  
  # Append to list
  metrics_by_year[[as.character(year)]] <- metrics_year
}

# Combine metrics for all years
metrics_all_years <- do.call(rbind, metrics_by_year)

# Save to CSV
write.csv(metrics_all_years, "Data/Processed/combined/metrics_all_years.csv")

