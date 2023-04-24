# prepare workspace
rm(list = ls())

library(tidyverse)
library(here)
library(cowplot)
library(lubridate)


# load data
schmidt <- read.csv(here("Data/Processed/lake/schmidt_stability_model.csv"), header = TRUE)
pheno <- read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)

schmidt <- schmidt %>%
  rename(date = datetime) %>%           
  mutate(
    year = year(date),                  
    doy = yday(date))

head(schmidt)

#daily schmidt 
schmidt_day <- schmidt %>% 
  group_by(date,doy,year) %>% 
  summarise(schmidt = mean(schmidt.stability, na.rm = TRUE))
# Merge the summary_df with climatic_data to get the day of year information
merged <- merge(schmidt_day, pheno[, c("year", "doy_start", "buoy_end")], by = "year")

merged <- merged %>% 
  rowwise() %>%
  mutate(period = 
           case_when(doy < doy_start ~ "summer", 
                     doy >= doy_start & doy <= buoy_end ~ "lf", 
                     TRUE ~ "winter"))
merged_cut <- subset(merged, period == "lf")

merged_cut <- merged_cut %>%
  filter(schmidt >= 1)

# merged_cut = merged_cut %>% 
  # filter(!(year == 2017 & doy < 273))

# Create an empty data frame to store the results
slope_results <- data.frame(year = integer(), slope = double())

# Loop through each year and fit a linear model
for (yr in unique(merged_cut$year)) {
  # Subset the data for the current year
  subset_data <- merged_cut[merged_cut$year == yr, ]
  
  # Fit a linear model
  lm_model <- lm(schmidt ~ doy, data = subset_data)
  
  # Extract the slope from the model coefficients
  slope <- coef(lm_model)[2]
  
  # Append the results to the data frame
  slope_results <- rbind(slope_results, data.frame(year = yr, slope = slope))
}

# Print or use the slope_results data frame as needed
print(slope_results)

p1 <- ggplot(merged_cut, aes(x = doy, y = schmidt)) +
  geom_line() +
  facet_wrap(~year, ncol = 3) +
  theme_classic(base_size = 19) +
  labs(x = "", y = expression("Schmidt stability (J m"^2*")")) +
  scale_x_continuous(breaks = c(244,274,305), labels = c("Sept", "Oct", "Nov"))


ggsave("Data/Figures/comments/schimdt_stability_fixed_v2.jpg",p1,  dpi= 300, width = 10, height = 6, units = "in")

