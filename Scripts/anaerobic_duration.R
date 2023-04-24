## anaerobic duration 

## prepare workspace
rm(list = ls())

#library 
library(tidyverse)
library(ggplot2)
library(zoo)
library(ggpubr)

df <- read.csv("Data//Processed//boue//delta_co2-2.csv", header = TRUE)

df <- df %>% 
  select(timestamp, year, do_mg_l_hypo, date) %>% 
  group_by(date, year) %>% 
  summarise(do_mg_l_hypo = mean(do_mg_l_hypo))
# Remove rows with NA values in the Date column
df <- df[complete.cases(df$date), ]



df$date <- as.Date(df$date)
# Find the first non-NA value after the initial NAs
first_non_na <- df$do_mg_l_hypo[which(!is.na(df$do_mg_l_hypo))[1]]

# Fill the first 9 rows with the value from the 10th row
df$do_mg_l_hypo[1:9] <- first_non_na

# Find the last non-NA value in the 'do_mg_l_hypo' column
last_valid_value <- tail(df$do_mg_l_hypo[!is.na(df$do_mg_l_hypo)], 1)

# Identify the rows after the last valid value
rows_to_fill <- which(is.na(df$do_mg_l_hypo) & seq_along(df$do_mg_l_hypo) > which(!is.na(df$do_mg_l_hypo))[length(which(!is.na(df$do_mg_l_hypo)))])

# Fill NAs with the last valid value for the identified rows
df$do_mg_l_hypo[rows_to_fill] <- last_valid_value

df$do_mg_l_hypo <- zoo::na.approx(df$do_mg_l_hypo)

# Threshold for anaerobic condition
threshold <- 0.5
current_year <- current_year <- as.numeric(format(df$date[1], "%Y"))
anaerobic_duration_vector <- numeric()

# Loop through the dataframe
for (i in 1:nrow(df)) {
  # Check if a new year has started
  if (as.numeric(format(df$date[i], "%Y")) != current_year) {
    anaerobic_duration <- 0
    current_year <- as.numeric(format(df$date[i], "%Y"))
  }
  
  if (df$do_mg_l_hypo[i] < threshold) {
    anaerobic_duration <- anaerobic_duration + 1
  } else {
    anaerobic_duration <- 0
  }
  anaerobic_duration_vector <- c(anaerobic_duration_vector, anaerobic_duration)
}

# Add the anaerobic duration as a new column in the dataframe
df$ad <- anaerobic_duration_vector

# Display the updated dataframe
print(df)


p1 <- ggplot(data = df, aes(x = as.factor(year), y = ad))+
  geom_bar(stat = "summary", fun = "max", fill = "blue")+
  labs(title = "Maximum Anaerobic Duration",
       x = "Year",
       y = "Maximum Anaerobic Duration")+
  theme_classic()

ggsave("Data//Figures//anaerobic_duration.jpg",p1, dpi = 300)

# Set the threshold for hypoxic condition (2 mg/L)
threshold_hypoxic <- 2

# Initialize variables to keep track of hypoxic duration and current_year
hypoxic_duration <- 0
hypoxic_duration_vector <- numeric()
current_year <- as.numeric(format(df$date[1], "%Y"))

# Loop through the dataframe to calculate hypoxic duration
for (i in 1:nrow(df)) {
  # Check if a new year has started
  if (as.numeric(format(df$date[i], "%Y")) != current_year) {
    hypoxic_duration <- 0
    current_year <- as.numeric(format(df$date[i], "%Y"))
  }
  
  if (df$do_mg_l_hypo[i] < threshold_hypoxic) {
    hypoxic_duration <- hypoxic_duration + 1
  } else {
    hypoxic_duration <- 0
  }
  hypoxic_duration_vector <- c(hypoxic_duration_vector, hypoxic_duration)
}

# Add the hypoxic duration as a new column in the dataframe
df$hd <- hypoxic_duration_vector


# Set the threshold for hypoxic condition (2 mg/L)
threshold_hypoxic <- 2

# Initialize variables to keep track of hypoxic duration and current_year
hypoxic_duration <- 0
hypoxic_duration_vector <- numeric()
current_year <- as.numeric(format(df$date[1], "%Y"))

# Loop through the dataframe to calculate hypoxic duration
for (i in 1:nrow(df)) {
  # Check if a new year has started
  if (as.numeric(format(df$date[i], "%Y")) != current_year) {
    hypoxic_duration <- 0
    current_year <- as.numeric(format(df$date[i], "%Y"))
  }
  
  if (df$do_mg_l_hypo[i] < threshold_hypoxic) {
    hypoxic_duration <- hypoxic_duration + 1
  }
  hypoxic_duration_vector <- c(hypoxic_duration_vector, hypoxic_duration)
}

# Add the hypoxic duration as a new column in the dataframe
df$hd <- hypoxic_duration_vector

# Use dplyr to group by year and summarize the maximum hd for each year
result <- df %>%
  group_by(year) %>%
  summarize(max_hd = max(hd))

# View the result
print(result)



p2 <- ggplot(data = df, aes(x = as.factor(year), y = hd))+
  geom_bar(stat = "summary", fun = "max", fill = "blue")+
  labs(title = "Maximum hypoxic Duration",
       x = "Year",
       y = "Maximum hypoxic Duration")+
  theme_classic()

ggsave("Data//Figures//hypoxic_duration.jpg",p2, dpi = 300)

combined <- ggarrange(p1,p2, nrow =1, ncol = 2)

ggsave("Data//Figures//ad_hypo_duration.jpg",combined, dpi = 300, width = 10, height = 7, units = "in")


df_22 <- subset(df, year == "2022")
