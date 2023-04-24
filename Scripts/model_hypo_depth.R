# prepare workspace
rm(list = ls())

## load libraries
library(tidyverse)
library(rLakeAnalyzer)
library(zoo)

library(odemR)

## load data
# wtr_2015 <- read.csv("Data//Processed//lake//wtr_temp_for_schmidt_2015.csv")
wtr <- read.csv("Data//model//results//temp_all_years.csv")
bathy <- read.csv("Data//Processed//lake//CRO_bassin_1_bathy.csv")
pheno <- read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)

# Rename date column to datetime and fix formatting
wtr <- wtr %>% 
  rename(datetime = date)
wtr$datetime <- as.Date(wtr$datetime, format = "%Y-%m-%d")

#remove first column
wtr <- wtr[, -1]

#add year column
# wtr$year = year(wtr$datetime) # can't do this here as it bricks the code

#subset for 2015
wtr_2015 <- subset(wtr, year == 2015 )


#create 3x3 plot environment
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) 

# Extract unique years from the datetime column
years <- unique(year(wtr$datetime))

for (year in years) {
  cat("Generating plots for year", year, "\n")
  
  # Subset the data for the current year
  wtr_year <- wtr[year(wtr$datetime) == year, ]
  
  # Create wtr.heatmap.layers plot
  # wtr.heatmap.layers(wtr_year)
  
  # Create wtr.plot.temp plot 
  wtr.plot.temp(wtr_year)
  
  # Add the year as text to the plot
  text(x = max(wtr_year$datetime), y = max(wtr_year$depths_m), labels = year, pos = 3)
  
  
  # Add the year as text to the plot
  text(x = max(wtr_year$datetime), y = max(wtr_year$depths_m), labels = year, pos = 3)
}
# Reset the layout and margins to default (1 plot)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

#calculate meta depths
m.d <- ts.meta.depths(wtr, slope = 0.01, na.rm = TRUE)

#calculate thermocline depths
t.d <- ts.thermo.depth(wtr, Smin = 0.01, na.rm = TRUE)
write.csv(t.d, "Data/Processed/lake/thermo_depth.csv")

s.s <-ts.schmidt.stability(wtr, bathy, na.rm = TRUE)
write.csv(s.s, 'Data/Processed/lake/schmidt_stability_model.csv')

# create object holding depth values at which to interpolate bathymetry
depth_values <- seq(0, 11, by = 0.1)

#round meta depths to the first decimal place
m.d.1 <- round(m.d$bottom, 1)

# Linear interpolation
int_bathy <- approx(bathy$depths_m, bathy$areas_m2, xout = depth_values, method = "linear", rule = 2)

# create dataframe off of interpolated hypsometry
int_bathy <- data.frame(depths_m = depth_values, areas_m2 = int_bathy$y)

# round to the nearest 0.5
m.d_round <- round(m.d$bottom * 2)/2

#create dataframe of rounded meta depths 
m.d.r <- data.frame(datetime = m.d$datetime, meta_depth_rounded = m.d_round)


# plot top and bottom of the metalimnion but all in the same plot space

# plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
# lines(m.d$datetime, m.d$bottom, col='red')

for (year in years) {
  cat("Generating plots for year", year, "\n")
  
  # Subset the data for the current year
  wtr_year <- wtr[year(wtr$datetime) == year, ]
  
  # Create wtr.heatmap.layers plot
  wtr.heatmap.layers(wtr_year)
  
  # # Create wtr.plot.temp plot 
  # wtr.plot.temp(wtr_year)
  
  # Add the year as text to the plot
  text(x = max(wtr_year$datetime), y = max(wtr_year$depths_m), labels = year, pos = 3)
  
  
  # Add the year as text to the plot
  text(x = max(wtr_year$datetime), y = max(wtr_year$depths_m), labels = year, pos = 3)
}

# Reset the layout and margins to default (1 plot)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)



calculate_hypolimnion_volume <- function(depth_top_hypolimnion, hypsometry_curve) {
  
  #  hypsometry_curve is a data frame with columns: depths_m, areas_m2
  lake_vol <- sum(diff(hypsometry_curve$depths_m) * hypsometry_curve$areas_m2[-length(hypsometry_curve$areas_m2)])
  
  # Subtract the cumulative area up to the depth of the top of the hypolimnion
  hypolimnion_volume <- lake_vol - sum(hypsometry_curve$areas_m2[hypsometry_curve$depths_m <= depth_top_hypolimnion])
  
  return(hypolimnion_volume)
}
# Hypsometry_curve data
hypsometry_curve <- bathy

# hypolimnion data
depth_top_hypolimnion <- t.d$thermo.depth


# Create an empty data frame to store results
result_df <- data.frame(Date = character(), Hypolimnion_Volume = numeric(), stringsAsFactors = FALSE)

# Loop through each day's data
for (i in seq_along(depth_top_hypolimnion)) {
  
  # Calculate hypolimnion volume for each day
  daily_hypolimnion_volume <- calculate_hypolimnion_volume(depth_top_hypolimnion[i], hypsometry_curve)
  
  # Append the result to the data frame
  result_df <- rbind(result_df, data.frame(Date = paste("Day", i), hypo_vol = daily_hypolimnion_volume))
}







# Print the result data frame
result_df$date <- m.d$datetime
result_df <- result_df[,-1]
result_df$date = as.Date(result_df$date)
result_df$year <- as.integer(format(result_df$date, "%Y"))
result_df$year  <- as.factor(result_df$year)
print(result_df)

result_df <- result_df %>% 
  mutate(percent_of_lake = hypo_vol/377064*100,
         epi_vol = 377064-hypo_vol,
         epi_vol_perc = 100 - hypo_vol/377064*100
  )


result_df <- merge(result_df, pheno[,c("year", "doy_start","buoy_end")], by = "year")
result_df <- result_df %>% 
  mutate(doy = strftime(date, format = "%j"))


result_df = result_df %>% 
  rowwise() %>%
  mutate(period  = 
           case_when(doy < doy_start ~ "summer", 
                     doy >= doy_start & doy <= buoy_end ~ "lf", 
                   TRUE ~ "winter"))

lf_df <- subset(result_df, period == "lf")


  

write.csv(lf_df, "Data/Processed/lake/hypo_depth_lf.csv")

res_2015 <- subset(result_df, year == 2015)
res_2017 <- subset(result_df, year == 2017)
res_2018 <- subset(result_df, year == 2018)

write.csv(result_df, "Data/Processed/lake/hypo_depth.csv")
scipen = 999
#2015
vol_2015 <- ggplot(res_2015, aes(x = date, y = percent_of_lake))+
  geom_line()+
  geom_point()+
  theme_minimal()+ 
  labs(title = "% Volume of lake the hypolimnion represents in 2015")
vol_2015
  scale_y_continuous(labels = scales::comma)

ggsave("Data//Figures//vol_2015.jpg", vol_2015)


#2017
vol_2017 <- ggplot(res_2017, aes(x = date, y = percent_of_lake))+
  geom_line()+
  geom_point()+
  theme_minimal()+ 
  scale_y_continuous(labels = scales::comma)+
  labs(title = "% Volume of lake the hypolimnion represents in 2017")
vol_2017
ggsave("Data//Figures//vol_2017.jpg", vol_2017)

#2018
ggplot(res_2018, aes(date, y = hypo_vol))+
  geom_line()+
  geom_point()+
  theme_minimal()
t.d <- t.d %>% 
  mutate(
    doy = strftime(datetime, format = "%j")
  )
t.d$year <- year(t.d$datetime)
ggplot(t.d, aes(x = datetime, y = thermo.depth))+
  geom_line()+
  # facet_wrap(~year, ncol = 3)+
  theme_minimal()
  