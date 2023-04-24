#prepare workspace

# load libraries
library(tidyverse)


# load data
df <- read.csv("Data/Processed/precip/era5_cut_lf.csv")

# make sure the date column is recognized as Date
df$date <- as.Date(df$date)

# plot
p1 <- ggplot(df, aes(x = date, y = rain)) +
  geom_col(fill = "steelblue") +
  theme_classic(base_size = 24) +
  facet_wrap(~ year, nrow = 3, scales = "free_x") +
  scale_x_date(
    date_breaks = "1 month",   # tick every month
    date_labels = "%b",        # show abbreviated month names
    expand = c(0, 0)           # remove extra white space
  ) +
  labs(
    y = "Daily Precipitation (mm)",
    x = NULL,
    title = NULL
  )


ggsave("Data/Figures/comments/precipitation_era5_by_year.jpg", dpi = 300, width = 16, height = 9 , units = "in")
