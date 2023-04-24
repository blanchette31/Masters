# Prepare workspace
rm(list = ls())

# Load libraries 
library(tidyverse)
library(scales)
library(ggpubr)
library(MetBrewer)
library(patchwork)
library(nlme)

# Load data 
df <- read.csv("Data/Processed/combined/comp_years.csv", header = TRUE)
lf <- read.csv("Data/Processed/lake/co2_o2_post_lf.csv", header = TRUE)

# Convert 'year' to a factor for grouping in the model
lf$year <- as.factor(lf$year)

# Filter data to only include points between DOY 257 and 305
lf <- lf %>% filter(doy >= 257 & doy <= 305)

comp <- df %>% 
  select(year, delta_co2_rate, delta_o2_rate)
comp$year <- as.factor(comp$year)

labels_df <- data.frame(
  doy = c(244, 274, 305),
  label = c("Sept", "Oct", "Nov")
)
lf$year <- factor(lf$year, levels = rev(levels(lf$year)))
lf$year <- factor(lf$year, levels(lf$year)[c(5, 2, 1, 3, 6, 7, 4)])

# Custom color palette
# custom_palette <- c("#44adce", "#8ad4dd", "#f2393a", "#1f5486", "#fde0a1", "#fe691d", "#ffc047")

# Initialize an empty data frame to store slopes
slopes_df <- data.frame(year = character(), slope_co2 = numeric(), slope_o2 = numeric(), stringsAsFactors = FALSE)

# Fit GLS models with AR1 correlation structure, predict values for plotting, and store slopes
lf <- lf %>%
  group_by(year) %>%
  filter(!is.na(co2) & !is.na(o2)) %>%  # Remove rows with missing values
  do({
    co2_model <- gls(co2 ~ doy, data = ., correlation = corAR1(form = ~ doy))
    o2_model <- gls(o2 ~ doy, data = ., correlation = corAR1(form = ~ doy))
    data.frame(
      .,
      gls_co2 = predict(co2_model),
      gls_o2 = predict(o2_model)
    ) %>%
      bind_rows(
        data.frame(
          year = unique(.$year),
          slope_co2 = coef(co2_model)["doy"],
          slope_o2 = coef(o2_model)["doy"]
        )
      )
  })

# Extract slopes to a separate data frame
slopes <- lf %>%
  select(year, slope_co2, slope_o2) %>%
  distinct()

# Plot GLS regression for CO2
p1_1 <- ggplot(lf, aes(x = doy, y = co2, color = year)) +
  geom_line(aes(y = gls_co2), linetype = "solid", linewidth = 1.1) +  # GLS regression line
  geom_point(alpha = 0.35) +
  theme_classic() +
  scale_color_manual(values = met.brewer("Hiroshige", 8), name = "Year") +
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label) +
  labs(title = "", y = "", x = "") +
  theme(legend.position = "none")

# Plot GLS regression for O2
p2_1 <- ggplot(lf, aes(x = doy, y = o2, color = year)) +
  geom_line(aes(y = gls_o2), linetype = "solid", linewidth = 1.1) +  # GLS regression line
  geom_point(alpha = 0.35) +
  theme_classic() +
  scale_color_manual(values = met.brewer("Hiroshige", 8)) +
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label) +
  labs(title = "", y = "", x = "") +
  theme(legend.position = "right")

# Display plots
p1_1 + p2_1

# Display slopes table
print(slopes)
