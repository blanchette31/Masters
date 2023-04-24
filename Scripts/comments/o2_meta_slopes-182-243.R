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
lf <- read.csv("Data/Processed/boue/delta_co2-2_model_2015.csv", header = TRUE)

# Subset data to July 1st – August 31st
lf <- lf %>% filter(doy >= 182 & doy <= 243)



# Convert 'year' to a factor and reorder levels
lf$year <- as.factor(lf$year)
lf$year <- factor(lf$year, levels = rev(levels(lf$year)))
lf$year <- factor(lf$year, levels(lf$year)[c(5, 2, 1, 3, 6, 7, 4)])

# Month labels for x-axis
labels_df <- data.frame(
  doy = c(182, 213, 243),
  label = c("July", "Aug", "End Aug")
)


# Fit GLS model for do_meta and predict values
lf <- lf %>%
  group_by(year) %>%
  filter(!is.na(do_meta)) %>%
  do({
    do_meta_model <- gls(do_meta ~ doy, data = ., correlation = corAR1(form = ~ doy))
    
    data.frame(
      .,
      gls_do_meta = predict(do_meta_model)
    ) %>%
      bind_rows(
        data.frame(
          year = unique(.$year),
          slope_do_meta = coef(do_meta_model)["doy"]
        )
      )
  })

# Extract slopes
slopes <- lf %>%
  select(year, slope_do_meta) %>%
  distinct()

# Plot GLS regression for do_meta
p_do_meta <- ggplot(lf, aes(x = doy, y = do_meta, color = year)) +
  geom_line(aes(y = gls_do_meta), linetype = "solid", linewidth = 1.1) +
  geom_point(alpha = 0.35) +
  theme_classic() +
  scale_color_manual(values = met.brewer("Hiroshige", 8)) +
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label) +
  labs(title = "O2 Meta GLS Regression july 1st-august 31st (doy 182-243)", y = "O2 Meta", x = "Day of Year") +
  theme(legend.position = "right")

# Display plot
print(p_do_meta)

# Display slopes table
print(slopes)

ggsave("Data/Figures/comments/o2_Meta_doy_182-243_GLS_Regression.jpg", plot = p_do_meta, dpi = 600, width = 10, height = 6, units = "in")
