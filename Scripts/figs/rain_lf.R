## prepare workspace

rm(list = ls())

## libraries

library(tidyverse)
library(MetBrewer)
library(cowplot)

# load data

rain <- read.csv("Data/Processed/precip/era5_cut_buoy.csv")
pheno <- read.csv("Data/Processed/phenocam/pheno_clean.csv")

# merge pheno
df = merge(rain, pheno[, c("doy_start", "doy_end", "year")],
           by = "year")


df_fall <- subset(df, doy >= 244)



p1 <- ggplot(df_fall, aes(x = doy, y = rain))+
  geom_rect(data = df_fall, aes(xmin = doy_start, xmax = doy_end, ymin = -Inf, ymax = Inf),
            fill = "#FFC107", alpha = 0.01)+
  geom_point(aes(y = wind_sp.m.s., color = "red"))+
  geom_col(color = "black")+
  scale_x_continuous(limits = c(244, 307),breaks = c(244, 274, 305), labels = c("Sep","Oct","Nov"))+
  facet_wrap(~year, ncol = 3)+
  theme_classic()+
  labs(x = "", y = "Precipitations (mm)")+
  theme()
p1

ggsave("Data/Figures/rain_sept1.jpg", dpi = 300, width = 10, height = 6, units = 'in')



# Create an empty dataframe to store correlation results
correlation_table <- data.frame(Year = numeric(), Correlation = numeric(), P_Value = numeric())

# Loop through each year and perform correlation test
for (year in unique(df_fall$year)) {
  subset_data <- df_fall[df_fall$year == year, ]
  correlation_result <- cor.test(subset_data$wind_sp.m.s., subset_data$rain)
  correlation_table <- rbind(correlation_table, data.frame(Year = year, Correlation = correlation_result$estimate, P_Value = correlation_result$p.value))
}

# Print the correlation table
print(correlation_table)
