#prepare workspace 
rm(list =ls())

# Load libraries
library(tidyverse)
library(ggpubr)
library(cowplot)
library(tidyverse)
library(scales)
library(ggpubr)
library(MetBrewer)
library(patchwork)
library(nlme)


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
    geom_vline(aes(xintercept = doy_start), linetype = "dashed", color = "#FFC107", linewidth = 1.5) +
    geom_vline(aes(xintercept = doy_end), linetype = "dashed", color = "#FFC107", linewidth = 1.5) +
    scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), labels = c("June", "July", "Aug", "Sept", "Oct", "Nov")) +
    geom_text(aes(x = 175, y = 50), label = expression(paste(Delta*CO[2])), size = 4) +
    geom_text(aes(x = 175, y = -100), label = expression(paste(Delta*O[2])), size = 4) +
    geom_hline(yintercept = 0) +
    ylab("") +
    xlab("") +
    theme_classic() +
    theme(plot.title = element_blank())  # Remove the plot title
  
  plots[[as.character(yr)]] <- p
}


#plots co2 and o2 accumulation



# Load data 
df_1 <- read.csv("Data/Processed/combined/comp_years_V2.csv", header = TRUE)
lf <- read.csv("Data/Processed/lake/co2_o2_post_lf.csv", header = TRUE)

# Convert 'year' to a factor for grouping in the model
lf$year <- as.factor(lf$year)

comp <- df_1 %>% 
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
  geom_point(alpha = 0.5, size =2) +
  theme_classic() +
  scale_color_manual(values = met.brewer("Hiroshige", 8), name = "Year") +
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label, limits = c(244,307)) +
  labs(y = "", x = "", title = "")+
  theme(legend.position = "none")

# Plot GLS regression for O2
p2_1 <- ggplot(lf, aes(x = doy, y = o2, color = year)) +
  geom_line(aes(y = gls_o2), linetype = "solid", linewidth = 1.1) +  # GLS regression line
  geom_point(alpha = 0.5) +
  theme_classic() +
  scale_color_manual(values = met.brewer("Hiroshige", 8)) +
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label, limits = c(244,307)) +
  labs(y = "", x = "", title = "")+
  theme(legend.position = "none")









# Add extra plots to the list
plots[["extra1"]] <- p1_1
plots[["extra2"]] <- p2_1

# Arrange plots in a grid using cowplot
plot_grid <- plot_grid(
  plots[[1]], plots[[2]], plots[[3]],
  plots[[4]], plots[[5]], plots[[6]],
  plots[[7]], plots[[8]], plots[[9]],
  ncol = 3, align = "hv"
)

# Add the additional plots to the grid
plot_grid <- plot_grid(
  plot_grid,
  extra_plot1 + theme(plot.margin = margin(0, 0, 0, 0)),
  extra_plot2 + theme(plot.margin = margin(0, 0, 0, 0)),
  align = "hv", ncol = 3, rel_heights = c(1, 1, 2)
)
# 
# # Arrange plots in a grid
# plot_grid <- ggarrange(plotlist = plots, ncol = 3, nrow = 3, widths = c(1, 1, 1), heights = c(1, 1, 1))
# 
# # Display the grid of plots
# print(plot_grid)

# Save the grid of plots to a file (optional)
ggsave("Data/Figures/presentations/co2_o2_all_years_sep_9plot_v2.jpg", plot_grid, dpi = 300, width = 10, height = 6, units = "in")
