# --- Prepare workspace ---

rm(list = ls())

library(tidyverse)
library(cowplot)
library(lmodel2)

# --- Load data ---

df <- read.csv("Data//Processed//boue//delta_co2-2_model_2015.csv", header = TRUE)
pheno <- read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)

# Merge with phenology info

df <- merge(df, pheno[, c("doy_start", "doy_end", "year")], by = "year")

# Define periods relative to leaf fall

df <- df %>%
  rowwise() %>%
  mutate(period_rel_leaf_fall = case_when(
    doy >= doy_start ~ "after",
    doy < (doy_start - 14) ~ "summer",
    TRUE ~ "before_leaf_fall"
  ))

# Average daily values per date

df_date <- df %>%
  group_by(date, year, doy, period_rel_leaf_fall) %>%
  summarise(
    co2 = mean(delta.CO2, na.rm = TRUE),
    o2 = mean(do_deviation, na.rm = TRUE),
    o2_meta = mean(do_deviation_meta, na.rm = TRUE),
    o2_hypo = mean(do_deviation_hypo, na.rm = TRUE),
    .groups = "drop"
  )

# Filter for after leaf fall period

post_lf <- df_date %>% filter(period_rel_leaf_fall == "after")

# Make 'year' a factor

years <- sort(unique(post_lf$year))
post_lf$year <- factor(post_lf$year, levels = years)

# Define colors for each year

col_ind <- setNames(c("#44adce", "#8ad4dd", "#f2393a", "#1f5486", "#fde0a1", "#fe691d", "#ffc047"), years)

ma_lines <- post_lf %>%
  group_by(year) %>%
  group_modify(~{
    # Run MA regression
    mod <- lmodel2(o2 ~ co2, data = .x)
    slope <- mod$regression.results$Slope[mod$regression.results$Method == "MA"]
    intercept <- mod$regression.results$Intercept[mod$regression.results$Method == "MA"]
    
    # Use min and max of co2 to create line points
    x_vals <- range(.x$co2, na.rm = TRUE)
    y_vals <- intercept + slope * x_vals
    
    tibble(
      x = x_vals,
      y = y_vals
    )
  }) %>%
  ungroup() %>%
  mutate(
    year = factor(rep(levels(post_lf$year), each = 2), levels = levels(post_lf$year))
  )


combined_plot <- ggplot(post_lf, aes(x = co2, y = o2, color = year)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_line(data = ma_lines, aes(x = x, y = y, group = year, color = year), size = 1.5) +
  geom_abline(intercept = 0, slope = -1, lty = 2) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = col_ind,
                     guide= guide_legend(
                       override.aes = list(
                         size = 8,
                         linetype = 1,
                         linewidth = 2
                       )
                     )) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(min(ma_lines$y) - 10, 0)) +
  theme_cowplot() +
  labs(
    x = expression(paste("Average daily ", CO[2], " departure (", mu, "mol L"^-1, ")")),
    y = expression(paste("Average daily ", O[2], " departure (", mu, "mol L"^-1, ")")),
    color = "Year"
  ) +
  theme(
    text = element_text(size = 32),       # increases all text by default
    axis.text = element_text(size = 32),  # tick labels
    axis.title = element_text(size = 32), # axis titles
    legend.text = element_text(size = 32),
    legend.title = element_text(size = 36),
    legend.position = "right",            # outside the plot
    legend.key.size = unit(1.5, "lines"), # bigger legend symbols
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# --- Show plot ---

combined_plot

# --- Save ---

ggsave("Data/Figures/comments/combined_co2_o2_departures_v3.jpg",
       combined_plot, dpi = 300, width = 16, height = 9, units = "in")
