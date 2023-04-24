#prepare workspace
rm(list = ls())


# load libraries
library(tidyverse)
library(ggpubr)
library(cowplot)


# load data 

df <- read.csv("Data//Processed//boue//delta_co2-2_model_2015.csv", header = TRUE)

df <- df[!is.na(df$year), ]

df_daily = df %>% 
  group_by(date, year, doy) %>% 
  summarise(
    do_mg_l = mean(do_mg_l),
    do_mg_l_meta = mean(do_mg_l_meta),
    do_mg_l_hypo = mean(do_mg_l_hypo))

# Filter out rows with NA in do_mg_l_hypo
df_daily_filtered <- df_daily %>% filter(!is.na(do_mg_l_hypo))

write.csv(df_daily_filtered, "Data/Processed/comments/do_daily_mgl.csv", row.names = FALSE)

# Filter the first row for each year
first_value_per_year <- df_daily_filtered %>%
  group_by(year) %>%
  slice(1)


# Create a table with year, date, and do_mg_l_hypo columns
table_result <- first_value_per_year %>%
  select(year, date, do_mg_l_hypo)

# Prepare month breaks
month_breaks <- data.frame(
  doy = yday(as.Date(paste0("2000-", 1:12, "-01"))),  # first day of each month
  month = month.abb
)

ggplot(df_daily, aes(x = as.numeric(doy))) +
  geom_line(aes(y = do_mg_l, color = factor("epi", levels = c("epi", "meta", "hypo"))), linewidth = 1) +
  geom_line(aes(y = do_mg_l_meta, color = factor("meta", levels = c("epi", "meta", "hypo"))), linewidth = 1) +
  geom_line(aes(y = do_mg_l_hypo, color = factor("hypo", levels = c("epi", "meta", "hypo"))), linewidth = 1) +
  facet_wrap(~year, ncol = 3) +
  scale_color_manual(
    name = "Layers",
    values = c("epi" = "red", "meta" = "blue", "hypo" = "green"),
    labels = c("Epilimnion", "Metalimnion", "Hypolimnion")  # nicer legend labels
  ) +
  scale_x_continuous(
    breaks = month_breaks$doy,
    labels = month_breaks$month
  ) +
  labs(
    title = NULL,
    x = "", 
    y = "Dissolved Oxygen (mg/L)"
  ) + theme_minimal(base_size = 16)

 
ggsave("Data/Figures/comments/do_obs_yearly_fixed.jpg", dpi = 300, width = 10.5, height = 7, units = "in")

spring_do <- df_daily %>% 
  filter(doy <= 150) %>% 
  filter(!is.na(do_mg_l_hypo))

mean_do_spring <- df_daily %>% 
  group_by(year) %>% 
  summarise(do_hypo = mean(do_mg_l_hypo, na.rm = TRUE))

mean_do_spring_meta <- df_daily %>% 
  group_by(year) %>% 
  summarise(do_meta = mean(do_mg_l_meta, na.rm = TRUE))

may26 <- df %>% 
  filter(date == "2015-05-26")

mean(may26$do_mg_l)
mean(may26$do_mg_l_meta)
mean(may26$do_mg_l_hypo)


june23 <- df %>% 
  filter(date == "2015-06-23")

mean(june23$do_mg_l)
mean(june23$do_mg_l_meta)
mean(june23$do_mg_l_hypo)



july21 <- df %>% 
  filter(date == "2015-07-21")

mean(july21$do_mg_l)
mean(july21$do_mg_l_meta)
mean(july21$do_mg_l_hypo)

august18 <- df %>% 
  filter(date == "2015-08-18")

mean(august18$do_mg_l)
mean(august18$do_mg_l_meta)
mean(august18$do_mg_l_hypo)

sept15 <- df %>% 
  filter(date == "2015-09-15")

mean(sept15$do_mg_l)
mean(sept15$do_mg_l_meta)
mean(sept15$do_mg_l_hypo)
