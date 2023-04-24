# Prepare workspace
rm(list = ls())

# Load libraries
library(tidyverse)
library(ggpubr)

# Load data
df <- read.csv("Data//Processed//boue//delta_co2-2_model_2015.csv", header = TRUE)
pheno <- read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)

df = merge(df, pheno[, c("doy_start", "doy_end", "year")], by = "year")

df <- df %>%
  rowwise() %>%
  mutate(period_rel_leaf_fall = case_when(
    doy >= doy_start ~ "after",
    doy < (doy_start - 14) ~ "summer",
    TRUE ~ "before_leaf_fall"
  ))

df_date <- df %>%
  group_by(date, year, doy, period_rel_leaf_fall) %>%
  summarise(
    co2 = mean(delta.CO2),
    o2 = mean(do_deviation),
    o2_meta = mean(do_deviation_meta),
    o2_hypo = mean(do_deviation_hypo)
  )

date_filt <- df_date %>%
  filter(!is.na(co2) | !is.na(o2))

post_lf <- df_date[df_date$period_rel_leaf_fall == "after",]

# Calculate slope after leaf fall by year for CO2 and O2
slopes <- df_date %>%
  filter(period_rel_leaf_fall == "after") %>%
  group_by(year) %>%
  summarise(co2_slope = lm(co2 ~ doy)$coefficients[2], o2_slope = lm(o2 ~ doy)$coefficients[2])

slopes <- merge(slopes, df[, c("doy_start", "doy_end", "year")], by = "year")

summer_lf <- df_date[df_date$period_rel_leaf_fall == "summer",]

filt_doy <- date_filt %>%
  group_by(doy) %>%
  summarise(
    co2 = mean(co2, na.rm = TRUE),
    o2 = mean(o2, na.rm = TRUE)
  )

df_june <- df_date %>%
  filter(doy >= 152)
df_june <- merge(df_june, df[, c("doy_start", "doy_end", "year")], by = "year")

# Create individual plots for each year
years <- unique(df_june$year)
plots <- list()

for (yr in years) {
  df_june_year <- df_june %>% filter(year == yr)
  
  p <- ggplot(df_june_year, aes(x = doy, y = co2)) + 
    geom_line(linewidth = 1.3) +
    geom_line(aes(y = o2), linewidth = 1.3) +
    geom_rect(aes(xmin = doy_start, xmax = doy_end, ymin = -400, ymax = 100), 
              fill = "#FFC107", alpha = 0.2) +
    scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), labels = c("June", "July", "Aug", "Sept", "Oct", "Nov")) +
    geom_text(aes(x = 175, y = 50), label = expression(paste(Delta*CO[2])), size = 4) +
    geom_text(aes(x = 175, y = -100), label = expression(paste(Delta*O[2])), size = 4) +
    geom_hline(yintercept = 0) +
    ylab("") +
    xlab("") +
    ggtitle(paste("Year:", yr)) +
    theme_classic()
  
  plots[[as.character(yr)]] <- p
}

# Arrange plots in a grid
plot_grid <- ggarrange(plotlist = plots, ncol = 3, nrow = 3)

# Display the grid of plots
print(plot_grid)


ggsave("Data/Figures/presentations/co2_o2_all_years_sep.jpg", plot_grid, dpi = 300, width = 10, height = 6, units = "in")
