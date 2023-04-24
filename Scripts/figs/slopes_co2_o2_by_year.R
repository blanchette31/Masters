## prepare workspace 

rm(list = ls())

## load libraries 

library(emmeans)
library(sjPlot)
library(tidyverse)
library(lme4)
library(ggpubr)
library(cowplot)
library(corrplot)
library(lattice)





## fixed effects need to be independant
## residuals have to be normally distributed
## load data 

lf <- read.csv("Data/Processed/lake/co2_o2_post_lf.csv", header = TRUE)

# Convert 'year' to a factor for grouping in the model
lf$year <- as.factor(lf$year)

# Assuming df is your data frame with columns year, doy, co2, and o2

# Fit ANCOVA model for co2 ~ doy
co2_ancova_model <- lm(co2 ~ doy * as.factor(year), data = lf)

# Perform ANOVA for co2 ~ doy
co2_ancova_anova <- anova(co2_ancova_model)

# Print ANOVA table for co2 ~ doy
print(co2_ancova_anova)


# Post hoc tests for co2 ~ doy
co2_tukey <- TukeyHSD(co2_ancova_anova, "doy * year")
# Get estimated marginal means (EMMs)
co2_emm <- emmeans(co2_ancova_model, ~ year, cov.reduce = FALSE)

# Pairwise comparisons
co2_emm_pairwise <- pairs(co2_emm)

# Adjust p-values for multiple comparisons
co2_emm_pairwise_adj <- summary(co2_emm_pairwise, adjust = "bonferroni")

# Print adjusted p-values
print(co2_emm_pairwise_adj)

# Fit ANCOVA model for o2 ~ doy
o2_ancova_model <- lm(o2 ~ doy * as.factor(year), data = lf)

# Perform ANOVA for o2 ~ doy
o2_ancova_anova <- anova(o2_ancova_model)

# Print ANOVA table for o2 ~ doy
print(o2_ancova_anova)

# Check the significance of the interaction term
interaction_p_value_o2 <- summary(o2_ancova_model)$coefficients["doy:as.factor(year)1", "Pr(>|t|)"]
cat("p-value for interaction term in o2 ANCOVA:", interaction_p_value_o2, "\n")


# lf_s22 <- subset(lf, year != "2022")

# Linear mixed-effects model for CO2 accumulation

co2_model <- lmer(co2 ~ year + (doy | year ), data = lf, REML = FALSE)
co2_model_2 <- lmer(co2 ~ doy + (1 | year ), data = lf, REML = FALSE)

summary(co2_model)
summary(co2_model_2)

res <- resid(co2_model)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 


# Extract the random effects
random_effects <- ranef(co2_model)


# Plot QQ plots for each random effect
par(mfrow = c(1, length(random_effects)))
for (i in seq_along(random_effects)) {
  residuals_i <- as.vector(random_effects[[i]][, 1])
  qqnorm(residuals_i)
  qqline(residuals_i, col = 2)
  title(main = paste("QQ Plot - Random Effect", i))
}



plot(density(res))

ggCaterpillar(ranef(co2_model, condVar=TRUE))  ## using ggplot2
qqmath(ranef(co2_model, condVar=TRUE)) 

#produce residual vs. fitted plot
plot(fitted(co2_model), res)

#add a horizontal line at 0 
abline(0,0)


# Linear mixed-effects model for O2 depletion
o2_model <- lmer(o2 ~ year + (doy | year), data = lf, REML = FALSE)
o2_s22_model <- lmer(o2 ~ doy + (1 | year), data = lf_s22, REML = FALSE)

reso2 <- resid(o2_model)
qqnorm(reso2)
qqline(reso2)




# Summary of the models
summary(co2_model)
summary(o2_model)
summary(o2_s22_model)


# Assuming your linear mixed-effects model is stored in o2_model
cov_matrix <- vcov(o2_model)

# Extract the correlation matrix for fixed effects
cor_matrix <- cor(o2_model)

# Extract the correlation matrix for fixed effects
cor_matrix <- get_variance_decomposition(o2_model)$cor.fixed

# Create a correlation plot
sjt.correlation(cor_matrix)
# Slope of CO2 over time grouped by year

p1 <- ggplot(lf, aes(x = doy, y = co2, color = year))+
  geom_smooth(method = lm, se = FALSE)+
  theme_classic()+
  labs(title = "Change in delta CO2 over time by year")


# slope of O2 over time grouped by year

p2 <- ggplot(lf, aes(x = doy, y = o2, color = year))+
  geom_smooth(method = lm, se = FALSE)+
  theme_classic()+
  labs(title = "Change in delta O2 over time by year")

p3 <- ggarrange(p1,p2, ncol = 2, common.legend = TRUE, legend = "right")
p3

ggsave("Data/Figures/co2_o2_delta_time_group.jpg", p3, dpi = 300, width = 10, height = 6, units = "in")


## Stats by year ## 


# Fit linear models for each year
co2_slopes_df <- lf %>%
  group_by(year) %>%
  summarize(co2_slope = coef(lm(co2 ~ doy))[2])

o2_slopes_df <- lf %>%
  group_by(year) %>%
  summarize(o2_slope = coef(lm(o2 ~ doy))[2])

# Merge slope information back into the main data frame
lf <- left_join(lf, co2_slopes_df, by = "year")
lf <- left_join(lf, o2_slopes_df, by = "year")


# Function to calculate RMSE, R-squared, and p-value
calculate_model_metrics <- function(model, response_variable) {
  predictions <- predict(model)
  residuals <- residuals(model)
  rmse <- sqrt(mean(residuals^2))
  rsquared <- summary(model)$r.squared
  p_value <- summary(model)$coefficients[2, 4]  # p-value for the predictor variable
  
  # Create a data frame with dynamically named columns
  result <- setNames(data.frame(rmse, rsquared, p_value),
                     c(paste0(response_variable, "_RMSE"),
                       paste0(response_variable, "_R_squared"),
                       paste0(response_variable, "_p_value")))
  
  return(result)
}

# Fit linear models for each year and calculate metrics
co2_metrics_df <- lf %>%
  group_by(year) %>%
  do({
    model <- lm(co2 ~ doy, data = .)
    metrics <- calculate_model_metrics(model, "co2")
    bind_cols(data.frame(year = .$year[1]), metrics)
  })

o2_metrics_df <- lf %>%
  group_by(year) %>%
  do({
    model <- lm(o2 ~ doy, data = .)
    metrics <- calculate_model_metrics(model, "o2")
    bind_cols(data.frame(year = .$year[1]), metrics)
  })

# Merge metrics information back into the main data frame
lf <- left_join(lf, co2_metrics_df, by = "year")
lf <- left_join(lf, o2_metrics_df, by = "year")

# Plot for CO2 with facet labels
p_co2 <- ggplot(lf, aes(x = doy, y = co2)) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~year, ncol = 3) +
  geom_text(aes(label = paste("CO2 Slope:", round(co2_slope, 4),
                              "\nCO2 RMSE:", round(co2_RMSE, 4),
                              "\nCO2 R-squared:", round(co2_R_squared, 4),
                              "\nCO2 p-value:", format(co2_p_value, scientific = TRUE, digits = 2))),
            x = 259, y = 60, size = 3)

# Plot for O2 with facet labels
p_o2 <- ggplot(lf, aes(x = doy, y = o2)) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~year, ncol = 3) +
  geom_text(aes(label = paste("O2 Slope:", round(o2_slope, 4),
                              "\nO2 RMSE:", round(o2_RMSE, 4),
                              "\nO2 R-squared:", round(o2_R_squared, 4),
                              "\nO2 p-value:", format(o2_p_value, scientific = TRUE, digits = 2))),
            x = 256, y = -80, size = 3)

# Print the plots
print(p_co2)
print(p_o2)

ggsave("Data//Figures//lf_co2_over_doy.jpg",p_co2, dpi = 300, width = 10, height = 6, units = "in")
ggsave("Data//Figures//lf_o2_over_doy.jpg",p_o2, dpi = 300, width = 10, height = 6, units = "in")

ggplot(lf, aes(y = co2/o2, x = doy))+
  geom_line(linewidth = 2)+
  geom_smooth(method = "lm", se =TRUE)+
  facet_wrap(~year, ncol = 3)+
  theme_classic()
