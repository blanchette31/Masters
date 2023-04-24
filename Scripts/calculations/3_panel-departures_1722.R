## 3 panel co2:o2 departure space ## 

# prepare workspace 
rm(list = ls())

# libraries
library(tidyverse)
library(cowplot)

# Load data
df <- read.csv("Data//Processed//boue//delta_co2-2_model_2015.csv", header = TRUE)
pheno <- read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)

df = merge(df, pheno[, c("doy_start", "doy_end", "year")], by = "year")

df <- df %>%
  rowwise() %>%
  mutate(period_rel_leaf_fall = case_when(
    doy >= doy_start ~ "after",
    doy < (doy_start - 14) ~ "summer",
    TRUE ~ "before_leaf_fall"))

# Apply filtering for 2017 and 2022 separately
df <- df %>%
  filter(
    !(year == 2017 & doy < 273),  # Exclude rows before DOY 273 in 2017
    !(year == 2022 & doy < 257)   # Exclude rows before DOY 257 in 2022
  )

df_date <- df %>% 
  group_by(date, year, doy, period_rel_leaf_fall) %>% 
  summarise(co2 = mean(delta.CO2),
            o2 = mean(do_deviation),
            o2_meta = mean(do_deviation_meta),
            o2_hypo = mean(do_deviation_hypo))

post_lf <- df_date[df_date$period_rel_leaf_fall == "after",]

# Function to calculate metrics for linear model
calculate_model_metrics <- function(model) {
  predictions <- predict(model)
  residuals <- residuals(model)
  rmse <- sqrt(mean(residuals^2))
  rsquared <- summary(model)$r.squared
  n <- length(residuals)
  min_value <- min(predictions)
  max_value <- max(predictions)
  
  result <- data.frame(
    rmse = rmse,
    rsquared = rsquared,
    n = n,
    min_value = min_value,
    max_value = max_value
  )
  
  return(result)
}

# Function to get linear model equation
get_model_equation <- function(model) {
  coef <- coef(model)
  slope <- round(coef[2], 3)
  intercept <- round(coef[1], 3)
  equation <- paste("y =", slope, "x +", intercept)
  return(equation)
}

# Fit linear models for each year and calculate metrics
linear_model_metrics <- post_lf %>%
  group_by(year) %>%
  do({
    model <- lm(o2 ~ co2, data = .)
    metrics <- calculate_model_metrics(model)
    equation <- get_model_equation(model)
    bind_cols(data.frame(year = .$year[1], equation = equation), metrics)
  }) %>%
  ungroup()  # Remove grouping

# Merge metrics information back into the main data frame
post_lf <- left_join(post_lf, linear_model_metrics, by = "year")


lf_58 <- post_lf[post_lf$year %in% c(2015, 2018), ]

lf_16 <- post_lf[post_lf$year %in% c(2016),]

lf_al <- post_lf[!post_lf$year %in% c(2015, 2016, 2018),]


col_ind <- c("#44adce", "#8ad4dd", "#f2393a", "#1f5486", "#fde0a1", "#fe691d", "#ffc047")


## plot 2015, 2018

# Plot
p58 <- ggplot(lf_58, aes(x = co2, y = o2, group = year)) +
  geom_point(aes(color = as.factor(year)), alpha = 0.5, size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(color = as.factor(year))) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(-100, 0)) +
  scale_color_manual(values = c("#44adce","#1f5486" ))+
  geom_abline(intercept = 0, slope = -1, lty = 2) +
  geom_hline(yintercept = 0) +
  theme_cowplot() +
  labs(title = "") +
  ylab(expression(paste("Average daily  ", O[2], " departure (", mu, "mol L"^-1, ")")))+
  xlab(expression(paste("Average daily ", CO[2],  " departure (", mu, "mol  L"^-1, ")")))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.placement = "inside",
        strip.text = element_text(hjust = 0, vjust = 2, face = "bold", size = 10),
        strip.background = element_blank(),
        legend.position = c(75, -50))

p58

p58_1 <- p58 + theme(axis.text.x = element_text(size = 16), # Increase axis tick mark text size
                     axis.text.y = element_text(size = 16), # Increase axis tick mark text size
                     axis.title.x = element_text(size = 18), # Increase axis labels size
                     axis.title.y = element_text(size = 18))
p58_1

ggsave("Data/Figures/presentations/cold_co2_o2_departures_1722.jpg", p58_1, dpi = 300, width = 7 , height = 6, units = "in")

# # Plot
# pal <- ggplot(lf_al, aes(x = co2, y = o2)) +
#   geom_point(aes(color = as.factor(year)), alpha = 0.5) +
#   geom_smooth(method = "lm", se = FALSE, color = "black") +
#   scale_x_continuous(limits = c(0, 100)) +
#   scale_y_continuous(limits = c(-100, 0)) +
#   geom_abline(intercept = 0, slope = -1, lty = 2) +
#   geom_vline(xintercept = 0) +
#   geom_hline(yintercept = 0) +
#   theme_cowplot() +
#   labs(title = "") +
#   ylab(expression(paste("Average daily ", O[2],  " departure ( ", mu,"mol/L)")))+
#   xlab(expression(paste("Average daily ", CO[2],  " departure ( ", mu,"mol/L)")))+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.placement = "inside",
#         strip.text = element_text(hjust = 0, vjust = 2, face = "bold", size = 10),
#         strip.background = element_blank(),
#         legend.position = "none")
# 
# pal


# Plot
p16 <- ggplot(lf_16, aes(x = co2, y = o2)) +
  geom_point( alpha = 0.5, size = 3, pch = 21, fill = "#8ad4dd", color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "#8ad4dd") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(-100, 0)) +
  geom_abline(intercept = 0, slope = -1, lty = 2) +
  scale_color_manual(values = c("#8ad4dd"))+
  # geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_cowplot() +
  labs(title = "") +
  ylab(expression(paste("Average daily  ", O[2], " departure (", mu, "mol L"^-1, ")")))+
  xlab(expression(paste("Average daily ", CO[2],  " departure (", mu, "mol  L"^-1, ")")))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.placement = "inside",
        strip.text = element_text(hjust = 0, vjust = 2, face = "bold", size = 10),
        strip.background = element_blank(),)

p16

p16_1 <- p16 + theme(axis.text.x = element_text(size = 16), # Increase axis tick mark text size
                     axis.text.y = element_text(size = 16), # Increase axis tick mark text size
                     axis.title.x = element_text(size = 18), # Increase axis labels size
                     axis.title.y = element_text(size = 18))
p16_1

ggsave("Data/Figures/presentations/16_co2_o2_departures_1722.jpg", p16_1, dpi = 300, width = 7 , height = 6, units = "in")







# Plot 2017, 2019, 2021, 2022 but with separate slopes 
pal2 <- ggplot(lf_al, aes(x = co2, y = o2)) +
  geom_point(aes(fill = as.factor(year)), alpha = 0.5, size = 3, pch = 21, color = "black") +
  geom_smooth(method = "lm", se = FALSE, aes(color = as.factor(year),group = year)) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(-100, 0)) +
  geom_abline(intercept = 0, slope = -1, lty = 2) +
  scale_fill_manual(values = c("#f2393a", "#fde0a1", "#fe691d", "#ffc047"))+
  scale_color_manual(values = c("#f2393a", "#fde0a1", "#fe691d", "#ffc047"))+
  # geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_cowplot() +
  labs(title = "") +
  ylab(expression(paste("Average daily  ", O[2], " departure (", mu, "mol L"^-1, ")")))+
  xlab(expression(paste("Average daily ", CO[2],  " departure (", mu, "mol  L"^-1, ")")))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.placement = "inside",
        strip.text = element_text(hjust = 0, vjust = 2, face = "bold", size = 10),
        strip.background = element_blank(),
        legend.position = "none")

pal2


pal2_1 <- pal2 + theme(axis.text.x = element_text(size = 16), # Increase axis tick mark text size
                       axis.text.y = element_text(size = 16), # Increase axis tick mark text size
                       axis.title.x = element_text(size = 18), # Increase axis labels size
                       axis.title.y = element_text(size = 18))
pal2_1
ggsave("Data/Figures/presentations/hot_co2_o2_departures_1722.jpg", pal2_1, dpi = 300, width = 7 , height = 6, units = "in")


grid <- plot_grid(p58,  p16+ theme(axis.title.y = element_blank() ), pal2+ theme(axis.title.y = element_blank() ), nrow = 1, axis = "tb", align ="h" , labels = NULL)

grid 

ggsave("Data/Figures/presentations/3_panel_co2_o2_departures_1722.jpg", grid, dpi = 300, width = 10, height = 5, units = "in")
