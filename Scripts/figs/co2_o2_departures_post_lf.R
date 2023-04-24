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

# # Create a new row to be added
# new_row <- data.frame(
#   year = 2014
# )
# 
# post_lf <- rbind(post_lf,new_row)

# Plot
p7 <- ggplot(post_lf, aes(x = co2, y = o2)) +
  geom_point(color = "#FFC107") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "CO2 departure", y = 'O2 departure') +
  scale_x_continuous(limits = c(-50, 150)) +
  scale_y_continuous(limits = c(-150, 50)) +
  geom_abline(intercept = 0, slope = -1, lty = 2) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  theme_classic() +
  labs(title = "") +
  ylab(expression(paste("Average daily ", O[2],  " departure ( ", mu,"mol/L)")))+
  xlab(expression(paste("Average daily ", CO[2],  " departure ( ", mu,"mol/L)")))+
  facet_wrap(~year, ncol = 3, nrow = 3, scales = "free")+
  theme(panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
strip.placement = "inside",
strip.text = element_text(hjust = 0, vjust = 2, face = "bold", size = 10),
strip.background = element_blank())

  # geom_text(
  #   aes(label = paste(equation, "\nR-squared =", round(rsquared, 2))),
  #   x = 50, y = 25, color = "black", data = linear_model_metrics
  # )

# Print the plot
print(p7)

ggsave("Data/Figures/co2-o2_departures_post_lf4.jpg", p7, dpi = 300, width = 10, height = 6, units = "in")
