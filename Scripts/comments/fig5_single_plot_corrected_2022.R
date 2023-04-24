# prepare workspace 
rm(list = ls())

library(tidyverse)
library(cowplot)
library(lmodel2)

# Load data
df <- read.csv("Data//Processed//boue//delta_co2-2_model_2015.csv", header = TRUE)
pheno <- read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)

df <- merge(df, pheno[, c("doy_start", "doy_end", "year")], by = "year")

df <- df %>%
  rowwise() %>%
  mutate(period_rel_leaf_fall = case_when(
    doy >= doy_start ~ "after",
    doy < (doy_start - 14) ~ "summer",
    TRUE ~ "before_leaf_fall"
  ))

df_date <- df %>% 
  group_by(date, year, doy, period_rel_leaf_fall) %>% 
  summarise(co2 = mean(delta.CO2),
            o2 = mean(do_deviation),
            o2_meta = mean(do_deviation_meta),
            o2_hypo = mean(do_deviation_hypo),
            .groups = "drop")

post_lf <- df_date %>% filter(period_rel_leaf_fall == "after")

# Split data by year
post_lf_split <- post_lf %>% group_by(year) %>% group_split()

# Calculate MA slopes and line ranges for each year
ma_lines <- map_dfr(post_lf_split, ~{
  yr <- unique(.x$year)
  mod <- lmodel2(o2 ~ co2, data = .x)
  slope <- mod$regression.results$Slope[mod$regression.results$Method == "MA"]
  intercept <- mod$regression.results$Intercept[mod$regression.results$Method == "MA"]
  x_range <- range(.x$co2, na.rm = TRUE)
  tibble(
    year = yr,
    x = x_range,
    y = intercept + slope * x_range
  )
})

# Make sure 'year' is a factor
ma_lines$year <- as.factor(ma_lines$year)
# Define colors for each year
col_ind <- c("2015" = "#44adce", "2016" = "#8ad4dd", "2017" = "#f2393a",
             "2018" = "#1f5486", "2019" = "#fde0a1", "2021" = "#fe691d", "2022" = "#ffc047")

# Plot
combined_plot <- ggplot(post_lf, aes(x = co2, y = o2, color = as.factor(year))) +
  geom_point(alpha = 0.5, size = 3) +
  geom_line(data = ma_lines, aes(x = x, y = y, group = year, color = year), size = 1) +
  geom_abline(intercept = 0, slope = -1, lty = 2) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = col_ind) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(-100, 0)) +
  theme_cowplot() +
  labs(x = expression(paste("Average daily ", CO[2], " departure (", mu, "mol L"^-1, ")")),
       y = expression(paste("Average daily ", O[2], " departure (", mu, "mol L"^-1, ")")),
       color = "Year") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.position = c(0.8, 0.2))

combined_plot

# Save
ggsave("Data/Figures/comments/combined_co2_o2_departures_v2.jpg",
       combined_plot, dpi = 300, width = 10, height = 6, units = "in")
