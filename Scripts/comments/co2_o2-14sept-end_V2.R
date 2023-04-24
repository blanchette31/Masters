# Clear workspace
rm(list = ls())

# Load required libraries
library(tidyverse)
library(scales)
library(ggpubr)
library(MetBrewer)
library(patchwork)
library(nlme)

# Load data
lf <- read.csv("Data/Processed/boue/delta_co2-2_model_2015.csv", header = TRUE)

# Rename variables for clarity
lf <- lf %>%
  rename(
    co2 = delta.CO2,
    o2 = do_deviation
  )

# Convert 'year' to factor and reorder levels
lf$year <- factor(lf$year)
lf$year <- factor(lf$year, levels = rev(levels(lf$year)))
lf$year <- factor(lf$year, levels(lf$year)[c(5, 2, 1, 3, 6, 7, 4)])

# Create labels for x-axis
labels_df <- data.frame(
  doy = c(244, 274, 305),
  label = c("Sept", "Oct", "Nov")
)

# Fit GLS models from DOY 244 onward and store predictions and slopes
lf_gls <- lf %>%
  group_by(year) %>%
  filter(!is.na(co2) & !is.na(o2)) %>%
  do({
    filtered_data <- filter(., doy >= 244)
    
    co2_model <- gls(co2 ~ doy, data = filtered_data, correlation = corAR1(form = ~ doy))
    o2_model <- gls(o2 ~ doy, data = filtered_data, correlation = corAR1(form = ~ doy))
    
    data.frame(
      filtered_data,
      gls_co2 = predict(co2_model),
      gls_o2 = predict(o2_model)
    ) %>%
      bind_rows(
        data.frame(
          year = unique(filtered_data$year),
          slope_co2 = coef(co2_model)["doy"],
          slope_o2 = coef(o2_model)["doy"]
        )
      )
  })

# Extract slopes
slopes <- lf_gls %>%
  select(year, slope_co2, slope_o2) %>%
  distinct()

# Plot GLS regression for CO2
p_co2 <- ggplot(lf_gls, aes(x = doy, y = co2, color = year)) +
  geom_line(aes(y = gls_co2), linetype = "solid", linewidth = 1.1) +
  geom_point(alpha = 0.35) +
  theme_classic() +
  scale_color_manual(values = met.brewer("Hiroshige", 8), name = "Year") +
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label) +
  labs(title = "GLS Regression: CO₂", y = "CO₂", x = "Day of Year") +
  theme(legend.position = "none")

# Plot GLS regression for O2
p_o2 <- ggplot(lf_gls, aes(x = doy, y = o2, color = year)) +
  geom_line(aes(y = gls_o2), linetype = "solid", linewidth = 1.1) +
  geom_point(alpha = 0.35) +
  theme_classic() +
  scale_color_manual(values = met.brewer("Hiroshige", 8)) +
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label) +
  labs(title = "GLS Regression: O₂", y = "O₂", x = "Day of Year") +
  theme(legend.position = "right")

# Display plots side by side
p_co2 + p_o2

# Print slopes table
print(slopes)
