# prepare workspace
rm(list = ls())

#load libraries
library(tidyverse)
library(ggpubr)
library(car)
library(rcompanion)

## load data
pheno <- read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)

# Get a list of all CSV files in the folder with the specified format

csv_files <- list.files("Data/Processed/lake/", pattern = "^temp_\\d{4}\\.csv$", full.names = TRUE)

# Create an empty list to store dataframes
dfs <- list()

# Loop through each CSV file, read it into a dataframe, and store it in the list
for (csv_file in csv_files) {
  # Extract the year from the file name
  year <- as.numeric(gsub("^temp_(\\d{4})\\.csv$", "\\1", basename(csv_file)))
  
  # Read the CSV file into a dataframe
  df <- read.csv(csv_file)
  
  # Assign the dataframe to a variable with the name df_year
  assign(paste("df_", year, sep = ""), df)
  
  # Optionally, store the dataframe in the list for further processing
  dfs[[year]] <- df
}

# Print the list of dataframe names
print(ls(pattern = "^df_"))

# Merge all dataframes into one
merged_df <- do.call(rbind, dfs)

# Print the merged dataframe
print(merged_df)
merged_df$year <- as.factor(merged_df$year)

merged_df <- merged_df %>% 
  mutate(doy = strftime(Date, format = "%j"))

merged_df$doy <- as.numeric(merged_df$doy)


merged_df = merge(merged_df, pheno[, c("doy_start", "doy_end", "year")],
           by = "year")

merged_df = merged_df %>%
  rowwise() %>%
  mutate( period = case_when(
    doy >= doy_start ~ "leaf_fall",
    doy < doy_start  ~ "summer"))



ggplot(merged_df, aes(x = doy, y = z_m))+
  geom_line()+
  theme_classic()+
  scale_y_continuous(limits = c(9.9, 11.1), breaks = c(9.9, 10.2, 10.5, 10.8, 11.1))+
  facet_wrap(~year, ncol = 3)



# Filter to keep only rows where Date is on or after September 1st of each year
df_filtered <- merged_df %>%
  filter(month(Date) >= 9)

# Print the filtered dataframe
head(df_filtered)

p1 <-  ggplot(df_filtered, aes(x = doy, y = z_m))+
  geom_line()+
  theme_classic()+
  scale_y_continuous(limits = c(9.9, 11.1), breaks = c(9.9, 10.2, 10.5, 10.8, 11.1))+
  scale_x_continuous(breaks = c(244, 274, 305), 
                     labels = c("Sept", "Oct", "Nov"))+
  facet_wrap(~year, ncol = 3)+
  labs(x = "", y = "Water level (m)")
p1

ggsave("Data/Figures/comments/water_level_sept_1st.jpg", p1, dpi = 300, width = 10, height = 6, units = "in")



# Define lake surface area (m²)
lake_surface_area <- 62848  

# Ensure Date is properly formatted and remove NAs
df_filtered <- df_filtered %>%
  mutate(Date = as.Date(Date)) %>%
  drop_na(Date, z_m)

# Compute water level changes
diff_wl <- df_filtered %>%
  group_by(year) %>%
  arrange(Date) %>%
  summarise(
    first_day_wl = first(z_m),         # Water level on first recorded day
    last_day_wl = last(z_m),           # Water level on last recorded day
    max_day_wl = max(z_m),             # Maximum water level recorded
    max_day_date = Date[which.max(z_m)],  # Date of max water level
    diff_wl_firstlast = last_day_wl - first_day_wl,  # Change from first to last day
    diff_wl_max = max_day_wl - first_day_wl,  # Change from first to max water level
    volume_changefirstlast_m3 = diff_wl_firstlast * lake_surface_area,  # Volume change (first to last)
    volume_change_maxwl_m3 = diff_wl_max * lake_surface_area,  # Volume change (first to max)
    .groups = "drop"
  )

# Print the updated table
print(diff_wl)

write.csv(diff_wl,"Data/Processed/comments/fall_waterlevel_calcs.csv")

# Create a list to store individual plots
plots_list <- list()

# Create individual plots for each year
for (year in unique(merged_df$year)) {
  subset_df <- merged_df[merged_df$year == year, ]
  leaf_fall_start_day <- pheno$doy_start[pheno$year == year]
  
  p <- ggplot(subset_df, aes(x = doy, y = z_m)) +
    geom_line() +
    theme_classic() +
    ggtitle(paste(year)) +
    scale_y_continuous(limits = c(9.9, 11.1), breaks = c(9.9, 10.2, 10.5, 10.8, 11.1))+
    geom_vline(xintercept = leaf_fall_start_day, linetype = "dashed", color = "red")
  
  # Save the plot to the list
  plots_list[[as.character(year)]] <- p
}

# Arrange the individual plots in a grid
grid_arranged <- ggarrange(plotlist = plots_list, ncol = 3, nrow = 3)

# Print the grid of plots
print(grid_arranged)
# Extract and plot water level for each year
for (year in unique(merged_df$year)) {
  subset_df <- merged_df[merged_df$year == year, ]
  
  # Find leaf fall start day for the current year
  leaf_fall_start_day <- pheno$doy_start[pheno$year == year]  
  p <- ggplot(subset_df, aes(x = doy, y = z_m)) +
    geom_line() +
    theme_classic() +
    ggtitle(paste("Water Level - Year", year)) +
    scale_y_continuous(limits = c(9.9, 11.1), breaks = c(9.9, 10.2, 10.5, 10.8, 11.1))+
    geom_vline(xintercept = leaf_fall_start_day, linetype = "dashed", color = "red") +
    facet_wrap(~year, ncol = 3)
  
  print(p)
}


# Perform ANCOVA for summers
summer_data <- subset(merged_df, period == "summer")
ancova_summer <- aov(z_m ~ doy + year, data = summer_data)
summary(ancova_summer)

leaf_fall_data <- subset(merged_df, period == "leaf_fall")
ancova_leaf_fall <- aov(z_m ~ doy + year, data = leaf_fall_data)
summary(ancova_leaf_fall)

# Load the necessary library for post hoc tests
library(multcomp)

# Create a Tukey's HSD test object
tukey_test <- glht(ancova_summer, linfct = mcp(year = "Tukey"))

# Perform the post hoc test
tukey_result <- summary(tukey_test)

# Print the results
print(tukey_result)
# Perform ANCOVA for leaf fall periods
leaf_fall_data <- subset(merged_df, period == "leaf_fall")
ancova_leaf_fall <- aov(z_m ~ temperature + as.factor(year), data = leaf_fall_data)

# Print the results
cat("ANCOVA for Summer:\n")
print(summary(ancova_summer))

cat("\nANCOVA for LeafFall:\n")
print(summary(ancova_leaf_fall))


# Assuming your dataframe has a "year" column and a "z_m" column for depth
# Calculate standard deviation for each year
yearly_variability <- aggregate(z_m ~ year, data = summer_data, FUN = sd)



# Print the calculated standard deviations
print(yearly_variability)

levene_test_result <- leveneTest(z_m ~ year, data = summer_data)

# Print the results
print(levene_test_result)

pairwise_test_result <- pairwise.wilcox.test(summer_data$z_m, summer_data$year, p.adjust.method = "bonferroni")

# Print the results of the pairwise Wilcoxon tests
print(pairwise_test_result)


# Plot the variability using a boxplot
library(ggplot2)

ggplot(summer_data, aes(x = year, y = z_m)) +
  geom_boxplot() +
  labs(title = "Variability of Depth Within Each Summer", x = "Year", y = "Depth") +
  theme_minimal()


ggplot(leaf_fall_data, aes(x = year, y = z_m)) +
  geom_boxplot() +
  labs(title = "Variability of Depth Within Each fall", x = "Year", y = "Depth") +
  theme_minimal()

ggplot(leaf_fall_data, aes(x = doy, y = z_m))+
  geom_line()+
  theme_classic()+
  scale_y_continuous(limits = c(9.9, 11.1), breaks = c(9.9, 10.2, 10.5, 10.8, 11.1))+
  facet_wrap(~year, ncol = 3)


