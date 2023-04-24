# prepare workspace 
rm(list = ls())

library(car)
library(multcomp)
library(lsmeans)
library(ggplot2)
library(multcomp)
 
 lf <- read.csv("Data/Processed/lake/co2_o2_post_lf.csv", header = TRUE)
 
 # Convert 'year' to a factor for grouping in the model
 lf$year <- as.factor(lf$year)
 
 lf_22 <-  lf[lf$year != 2022, ]

# ANCOVA for CO2
co2_model <- aov(co2 ~ doy * year, data = lf)
summary(co2_model)

# ANCOVA for O2
o2_model <- aov(o2 ~ doy * year, data = lf)
summary(o2_model)

o2_22 <- aov(o2 ~ doy * year, data = lf_22)
summary(o2_model)

# Check for interaction effect (difference in slopes)
interaction_test_co2 <- Anova(co2_model, type = "III")
interaction_test_o2 <- Anova(o2_model, type = "III")

# Print the results
print(interaction_test_co2)
print(interaction_test_o2)

# Pairwise comparisons for CO2 slopes
co2_glht <- glht(co2_model, linfct = mcp(year = "Tukey"))
summary(co2_glht)

# Pairwise comparisons for O2 slopes
o2_glht <- glht(o2_model, linfct = mcp(year = "Tukey"))
summary(o2_glht)

# Post-hoc pairwise comparisons for CO2
co2_means <- lsmeans(co2_model, "year", cov.reduce = FALSE)
co2_pairwise <- pairs(co2_means)
print(co2_pairwise)

# Post-hoc pairwise comparisons for O2
o2_means <- lsmeans(o2_model, "year", cov.reduce = FALSE)
o2_pairwise <- pairs(o2_means)
print(o2_pairwise)

